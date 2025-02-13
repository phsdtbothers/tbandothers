#' Gets the Start and End of a given Morbidity Week
#'
#' Accepts a week number and year, then returns info about the morbidity week. This info contains the year, week number, start date, and end date.
#'
#' @param week_number Week number of desired morbidity week.
#' @param from_year Year for what morbidity week calendar to use.
#'
#' @returns A tibble. This can be then used to call for specific morbidity week info using dplyr::pull()
#' @export
get_morbidity_week_info <- function(week_number, from_year) {
  # authorize and read google sheet
  if (!googlesheets4::gs4_has_token()) {
    googlesheets4::gs4_auth(scopes='https://www.googleapis.com/auth/spreadsheets.readonly')
  }

  morbidity_weeks <- googlesheets4::read_sheet('1lTwxbm96nTffaM0WG77lhpNcqDBlLDQJ3CfQFrWuAmE', sheet='morbidity_weeks')

  morbidity_weeks <- morbidity_weeks %>% mutate(
    year = as.numeric(year),
    week = as.numeric(week),
    start = as.Date(start),
    end = as.Date(end)
  )

  morbidity_week_info <- morbidity_weeks %>% filter(year==from_year, week==week_number)

  return(head(morbidity_week_info, 1))
}
