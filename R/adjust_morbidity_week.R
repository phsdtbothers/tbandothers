#' Gets the morbidity weeks of a date or dates adjusted to a provided year.
#'
#' @param dates Either a date or a vector of dates.
#' @param to_year The year that the years are based on.
#'
#' @returns A vector of morbidity weeks adjusted to provided year.
#'
#' @import lubridate
#'
#' @export
adjust_morbidity_week <- function(dates, to_year) {
  # create vector of dates adjusted to year to_year
  adjusted_dates <- data.frame(
    day=sapply(as.Date(dates), lubridate::day),
    month=sapply(as.Date(dates), lubridate::month)
  )

  # adjusted_dates$year <- to_year
  adjusted_dates$dates <- as.Date(ISOdate(to_year, adjusted_dates$month, adjusted_dates$day))

  # run get_morbidity_week function with generated dates
  adjusted_morbidity_weeks <- tbandothers::get_morbidity_week_number(adjusted_dates$dates)

  return(adjusted_morbidity_weeks)
}
