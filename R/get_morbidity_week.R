#' Gets morbidity week of a given date.
#'
#' @param date The date which we get the morbidity week for
#'
#' @returns Morbidity of date in integer form
#'
#' @import dplyr
#' @import lubridate
#' @import magrittr
#' @import googlesheets4
#'
#' @export
get_morbidity_week <- function(date) {
  # authorize and read function
  if (!googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth(scopes='https://www.googleapis.com/auth/spreadsheets.readonly')
  }

  morbidity_weeks <- googlesheets4::read_sheet('1lTwxbm96nTffaM0WG77lhpNcqDBlLDQJ3CfQFrWuAmE')

  from_date <- as.Date(date)

  morbidity_weeks$to_get <- from_date

  result <- morbidity_weeks %>%
    dplyr::filter(year == lubridate::year(from_date)) %>%
    dplyr::filter(start <= to_get & to_get <= end) %>%
    select(week)

  if (nrow(result) > 0) return(result$week)
  else return(NA)
}
