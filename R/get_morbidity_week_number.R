#' Gets morbidity week of either a single date, or a vector of dates.
#'
#' @param dates A single date, or vector of dates which we get the morbidity week/s for.
#' @param morbidity_weeks Optional. Morbidity week reference from tbandothers::download_morbidity_weeks. To be used to skip downloading per use.
#'
#' @returns If single date, then integer of morbidity week of said date. If vector of dates, then vector of morbidity weeks of said dates.
#'
#' @import dplyr
#' @import magrittr
#'
#' @export
get_morbidity_week_number <- function(dates, morbidity_weeks = NULL) {
  # if morbidity_weeks is not given, then download
  if (is.null(morbidity_weeks)) {
    morbidity_weeks <- tbandothers::download_morbidity_weeks()
  }

  # get week number from date/dates
  from_dates <- data.frame(dates=as.Date(dates))

  from_dates <- from_dates %>%
    dplyr::rowwise() %>%
    dplyr::mutate(week = morbidity_weeks$week[which(morbidity_weeks$start <= dates & dates <= morbidity_weeks$end)][1])

  # if single item, return integer only
  if (length(from_dates) == 1) return(from_dates[1, week])
  else return(from_dates$week)
}
