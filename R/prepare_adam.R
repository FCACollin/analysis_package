#' ADaM PASI Preparation
#'
#' Add ADSL and ADPASI in the Global environment.
#'
#' @export
#' @examples
#'
#' library(dplyr)
#' prepare_adam_pasi() %>%
#'   attach()
#'
#' head(adsl)
#' tail(adpasi)
#'
prepare_adam_pasi <- function() {

  dta <- utils::read.table(
    file.path(
      "https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays",
      "master/data/2021/2021-04-14/WWW_SustainedResponse.csv"
    ),
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )

  adsl <- unique(dta[c("USUBJID", "TRT", "BASELINE")])
  names(adsl)[names(adsl) == "BASELINE"] <- "PASIBL"
  adsl$ARMCD <- factor( # nolint - standard
    adsl$TRT,
    levels = c(
      "COMPARATOR TREATMENT",
      "ACTIVE TREATMENT DOSE 01",
      "ACTIVE TREATMENT DOSE 02"
    ),
    labels = c("ARM A", "ARM B", "ARM C")
  )


  adpasi_bl <- dta[c("USUBJID", "BASELINE")]
  names(adpasi_bl)[names(adpasi_bl) == "BASELINE"] <- "AVAL"
  adpasi_bl$AVISIT <- "WEEK00" # nolint - standard
  adpasi_bl$PARAMCD <- "PASITOT" # nolint - standard
  adpasi_bl$ABLFL <- "Y" # nolint - standard

  adpasi_fu <- dta[-which(names(dta) %in% c("TRT", "BASELINE"))]
  adpasi_fu <- tidyr::gather(
    adpasi_fu,
    key = "AVISIT",
    value = "AVAL",
    -"USUBJID"
  )
  adpasi_fu$PARAMCD <- "PASITOT" # nolint - standard
  adpasi_fu$ABLFL <- "" # nolint - standard

  list(
    adsl = adsl,
    adpasi = rbind(adpasi_bl, adpasi_fu)
  )
}
