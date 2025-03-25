#' Gets the Start and End of a given Morbidity Week
#'
#' Accepts a week number and year, then returns info about the morbidity week. This info contains the year, week number, start date, and end date.
#'
#' @param week_number Week number of desired morbidity week.
#' @param from_year Optional. Year for what morbidity week calendar to use. If not given, then the current year is used.
#' @param morbidity_weeks Optional. Morbidity week reference from tbandothers::download_morbidity_weeks. To be used to skip downloading per use.
#'
#' @import lubridate
#' @import magrittr
#'
#' @returns A tibble. This can be then used to call for specific morbidity week info using dplyr::pull()
#' @export
get_morbidity_week_info <- function(week_number, from_year=lubridate::year(Sys.Date()), morbidity_weeks=NULL) {
  # if morbidity_weeks is not given, then download
  if (is.null(morbidity_weeks)) {
    morbidity_weeks <- tbandothers::download_morbidity_weeks()
  }

  # filter and return morbidity week dataframe using arguments
  morbidity_week_info <- morbidity_weeks %>% filter(year==from_year, week==week_number)

  return(head(morbidity_week_info, 1))
}
