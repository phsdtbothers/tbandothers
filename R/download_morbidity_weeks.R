#' Downloads morbidity weeks.
#'
#' Downloads the morbidity week reference from Google Sheet. Sheet contains year, week number, start date, and end date information for each morbidity week.
#'
#' @returns dataframe, sourced from morbidity weeks google sheet.
#'
#' @import googlesheets4
#' @import magrittr
#' @import dplyr
#'
#' @export
download_morbidity_weeks <- function() {
  if (!googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth()
  }

  morbidity_weeks <- googlesheets4::read_sheet('1zy_p_qW6fqaAkr1QBLG-QuV4ucZOkrCaDgg0w4tohzQ', sheet='morbidity_weeks') %>%
    dplyr::mutate(
      year = as.numeric(year),
      week = as.numeric(week),
      start = as.Date(start),
      end = as.Date(end)
    )

  return(morbidity_weeks)
}
