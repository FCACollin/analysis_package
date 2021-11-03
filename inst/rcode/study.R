
## ---- study_settings ----

devtools::load_all()
library(dplyr)
library(ggplot2)

## ---- study_ads ----

prepare_adam_pasi() %>%
  attach()

# Analysis dataset
ads <- adsl %>%
  select(USUBJID, ARMCD) %>%
  left_join(adpasi) %>%
  filter(PARAMCD == "PASITOT") %>%
  rename(
    subj = USUBJID,
    pasi = AVAL,
    grp = ARMCD,
    week = AVISIT
  ) %>%
  mutate(
    time = gsub("^WEEK(.*)$", week, replacement = "\\1"),
    time = as.numeric(time)
  ) %>%
  select(subj, grp, week, pasi, time) %>%
  arrange(subj, time) %>%
  as_tibble()


## ---- study_ads_outlook ----

ads

## ---- study_helper_functions ----

# Last Observation Carried Forward, adapated from `zoo::na.locf`.
locf <- function(y) {

  ok <- which(!is.na(y))
  if (is.na(y[1L])) ok <- c(1L, ok)
  gaps <- diff(c(ok, length(y) + 1L))
  rep(y[ok], gaps)

}

# Next Observation Carried Backward derived from locf.
nocb <- function(y) rev(locf(rev(y)))

apply_ifany <- function(x, cond, fun = min, otherwise = Inf) {
  if (any(cond)) do.call(fun, list(x[cond]))
  else otherwise
}

pasi_fun <- function(x) {
  y <- cut(
    x,
    breaks = c(-Inf, 0, 2, 3, Inf),
    labels = c("PASI = 0", "PASI <= 2", "PASI <= 3", "PASI > 3"),
    include.lowest = TRUE
  )
  y <- factor(y, levels = c(levels(y), "Lost Response", "Missing"))
  y[is.na(y)] <- "Missing"
  y
}

## ---- helper_functions_ggplot ----

theme_spin <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(size = 7),
      title = element_text(size = 7)
    ) +
    theme(...)
}

StatStepArea <- ggproto(# nolint - ggplot syntax
  "StatStepArea", Stat,
  compute_group = function(data, scales) {
    data <- as.data.frame(data)[order(data$x), ]
    n <- nrow(data)
    if (n <= 1) {
      # Need at least one observation
      return(data[0, , drop = FALSE])
    }
    ys <- rep(1:n, each = 2)[-2 * n]
    xs <- c(1, rep(2:n, each = 2))
    x <- data$x[xs]
    y <- data$y[ys]
    data_attr <- data[xs, ]
    data_attr$x <- x
    data_attr$y <- y
    data_attr$ymin <- 0
    data_attr$ymax <- y
    y <- data_attr
    y
  },
  required_aes = c("x", "y")
)

stat_steparea <- function(mapping = NULL,
                          data = NULL,
                          geom = "area",
                          position = "identity",
                          na_rm = FALSE,
                          show_legend = NA,
                          inherit_aes = TRUE,
                          ...) {
  layer(
    stat = StatStepArea, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show_legend, inherit.aes = inherit_aes,
    params = list(na.rm = na_rm, ...)
  )
}


## ---- study_response_description ----

ads <- ads %>%
  group_by(subj) %>%
  mutate(
    rsp = nocb(pasi),
    nocbfl = is.na(pasi) & !is.na(rsp),
    # PASI Categories
    pasic = pasi_fun(rsp),
    # First time lower than pasi <= 2
    crsp_onset = apply_ifany(time, rsp <= 2 & !is.na(rsp)),
    # First wk after wk onset when pasi > 2
    crsp_end = apply_ifany(time, rsp > 2 & !is.na(rsp) & time > crsp_onset),
    rsp = factor(
      ifelse(time >= crsp_end, "Lost Response", as.character(pasic)),
      levels = levels(pasic)
    ),
    subset = case_when(
      all(crsp_onset == Inf) ~ "Never\nresponder",
      all(crsp_onset < Inf & crsp_end > 52) ~ "Continuous\nresponder",
      all(crsp_onset < Inf & crsp_end < Inf) ~ "Non-sustained\nresponder"
    )
  ) %>%
  ungroup() %>%
  mutate(subj = factor(subj, levels = unique(subj[order(-1 * crsp_onset)])))


## ---- km_estimator_endpoint_01 ----

library(survival)

data <- ads %>%
  group_by(subj) %>%
  mutate(wk_last = max(time[rsp != "Missing"])) %>%
  ungroup() %>%
  distinct(subj, crsp_onset, wk_last, crsp_end) %>%
  mutate(
    km_rsp_1 = crsp_onset < Inf,
    time_1 = ifelse(crsp_onset < wk_last, crsp_onset, wk_last),
    km_rsp_2 = crsp_end < Inf,
    time_2 = ifelse(crsp_end < wk_last, crsp_end, wk_last) - crsp_onset
  )

data

surv_mod <- survfit(Surv(time_1, km_rsp_1) ~ 1, data = data)
dta_gg <- summary(
  surv_mod, time = unique(ads$time)
)[c("time", "n.censor", "surv")] %>%
  as_tibble() %>%
  mutate(y =  1 - surv)

dta_gg


## ---- surv_01_a1 ----

gg1 <- dta_gg %>%
  ggplot(aes(x = time, y = y)) +
  geom_step(col = "green4") +
  stat_steparea(fill = "green4", alpha = .3) +
  scale_x_continuous(breaks = c(0, 16, 52)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 52)) +
  labs(
    title = "When?",
    subtitle = "Probability for a response onset"
  ) +
  theme_spin(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = margin(2, 0, 0, 5)
  )

gg1

## ---- surv_01_a2 ----

gg2 <- dta_gg %>%
  ggplot(aes(x = time, y = n.censor)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0, 16, 52)) +
  coord_cartesian(xlim = c(0, 52)) +
  xlab("Response onset week") +
  labs(
    subtitle =
      "Censoring: freq. of never-responders during participation to the study"
  ) +
  theme_spin(
    plot.margin = margin(2, 0, 0, 5),
    axis.title.y = element_blank()
  )

gg2


## ---- km_estimator_endpoint_02 ----

surv_mod <- survfit(Surv(time_2, km_rsp_2) ~ 1, data = data[data$km_rsp_1, ])
dta_gg <- summary(
  surv_mod, time = unique(ads$time)
)[c("time", "n.censor", "surv")] %>%
  as_tibble() %>%
  mutate(y = surv)


## ---- surv_01_b1 ----

gg3 <- dta_gg %>%
  ggplot(aes(x = time, y = y)) +
  geom_step(color = "green4") +
  stat_steparea(fill = "green4", alpha = .3) +
  scale_x_continuous(breaks = c(0, 16, 52)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "How long?",
    subtitle = "Probability of maintained response since onset"
  ) +
  theme_spin(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    plot.margin = margin(2, 0, 0, 5)
  )

gg3


## ---- surv_01_b2 ----

gg4 <- dta_gg %>%
  ggplot(aes(x = time, y = n.censor)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = c(0, 16, 52)) +
  scale_y_continuous(breaks = c(0, 100, 250)) +
  xlab("Weeks of sustained response") +
  labs(
    subtitle = "Censoring: freq. of still responders at last available obs"
  ) +
  theme_spin(
    plot.margin = margin(2, 0, 0, 5),
    axis.title.y = element_blank()
  )

gg4


## ---- surv_01_c ----

data <- ads %>%
  group_by(subj) %>%
  mutate(wk_last = max(week[rsp != "Missing"])) %>%
  ungroup() %>%
  distinct(subj, crsp_onset, wk_last, crsp_end) %>%
  mutate(
    km_rsp_1 = crsp_onset < Inf,
    time_1 = ifelse(crsp_onset < wk_last, crsp_onset, wk_last),
    km_rsp_2 = crsp_end < Inf,
    time_2 = ifelse(crsp_end < wk_last, crsp_end, wk_last) - crsp_onset,
    duration = time_2
  )

gg5 <- data %>%
  filter(duration > 0, crsp_end < Inf) %>%
  mutate(censored = crsp_end == Inf) %>%
  ggplot(aes(x = crsp_onset, y = duration)) +
  geom_boxplot(aes(group = crsp_onset), outlier.shape = NA) +
  scale_x_continuous(breaks = c(0, 16, 52)) +
  xlab("Response onset week") +
  ylab("Weeks of sustained\nresponse") +
  labs(title = "Non-sustained responders: Onset vs Duration") +
  scale_fill_viridis_c() +
  theme_spin()

gg5


## ---- surv_01_combination ----

gg <- cowplot::plot_grid(
  cowplot::plot_grid(
    gg1, gg3, gg2, gg4,
    align = "v",
    ncol = 2,
    rel_heights = c(3, 1.5),
    labels = c("A", "B")
  ),
  gg5,
  ncol = 1,
  rel_heights = c(2, 1),
  labels = c(NA, "C")
)

gg
