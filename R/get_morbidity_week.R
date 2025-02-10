#' Gets morbidity week of either a single date, or a vector of dates.
#'
#' @param date A single date, or vector of dates which we get the morbidity week/s for.
#'
#' @returns If single date, then integer of morbidity week of said date. If vector of dates, then vector of morbidity weeks of said dates.
#'
#' @import dplyr
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

  morbidity_weeks <- morbidity_weeks %>% mutate(
    year = as.numeric(year),
    week = as.numeric(week),
    start = as.Date(start),
    end = as.Date(end)
  )

  # get week number from date/dates
  from_dates <- data.frame(dates=as.Date(date))

  from_dates <- from_dates %>%
    dplyr::rowwise() %>%
    dplyr::mutate(week = morbidity_weeks$week[which(morbidity_weeks$start <= dates & dates <= morbidity_weeks$end)][1])

  # if single item, return integer only
  if (length(from_dates) == 1) return(from_dates[1, week])
  else return(from_dates$week)
}
