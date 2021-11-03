#' Data: ADaM PASI
#'
#' The Psoriasis Area and Severity Index (PASI) is a continuous measurement
#' of severity, the lower, the better the patient condition. The
#' derived dataset represents patient level follow-up during 8 visits over a
#' year of monitoring.
#'
#' @source Derived from
#'   Wonderful Wednesdays GitHub data repository (2021-04-14).
#'
#' @format A list with to ADaM datasets:
#' \describe{
#'   \item{ADSL}{subject level dataset}
#'   \item{ADPASI}{PASI records, follows basic data structure (BDS)}
#' }
#' @examples
#' str(adam_pasi, 1)
#'
"adam_pasi"
