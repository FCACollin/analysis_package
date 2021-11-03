sustained_rsp <- read.table(
  file.path(
    "https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays",
    "master/data/2021/2021-04-14/WWW_SustainedResponse.csv"
  ),
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

library(tidyr)
library(dplyr)

adsl <- sustained_rsp %>%
  as_tibble() %>%
  distinct(USUBJID, TRT, BASELINE) %>%
  rename(PASIBL = BASELINE) %>%
  mutate(
    ARMCD = factor(
      TRT,
      levels = c(
        "COMPARATOR TREATMENT",
        "ACTIVE TREATMENT DOSE 01",
        "ACTIVE TREATMENT DOSE 02"
      ),
      labels = c("ARM A", "ARM B", "ARM C")
    )
  )

adpasi <- rbind(
  sustained_rsp  %>%
    as_tibble() %>%
    select(USUBJID, BASELINE) %>%
    distinct() %>%
    rename(AVAL = BASELINE) %>%
    mutate(AVISIT = "WEEK00", PARAMCD = "PASITOT", ABLFL = "Y") %>%
    select(USUBJID, PARAMCD, AVISIT, AVAL, ABLFL),
  sustained_rsp  %>%
    as_tibble() %>%
    select(-TRT, -BASELINE) %>%
    gather(key = "AVISIT", value = "AVAL", -USUBJID) %>%
    mutate(PARAMCD = "PASITOT", ABLFL = "") %>%
    select(USUBJID, PARAMCD, AVISIT, AVAL, ABLFL)
)

adam_pasi <- list(
  adsl = adsl,
  adpasi = adpasi
)

usethis::use_data(adam_pasi, overwrite = TRUE)
