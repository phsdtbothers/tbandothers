#' Get table shard for morbidity week
#'
#' @param from_date date to generate shard from, if null, current date is used
#' @param morbidity_weeks optional, morbidity week reference from tbandothers::download_morbidity_weeks()
#'
#' @returns date for shard
#' @export
get_date_shard <- function(from_date = Sys.Date(), morbidity_weeks = NULL) {
  # if morbidity_weeks is not given, then download
  if (is.null(morbidity_weeks)) {
    morbidity_weeks <- tbandothers::download_morbidity_weeks()
  }

  # just in case string is inputted
  from_date <- as.Date(from_date)

  # default value NA
  final_date <- NA

  mw_end <- tbandothers::get_morbidity_week_number(from_date, morbidity_weeks = morbidity_weeks) %>%
    tbandothers::get_morbidity_week_end(morbidity_weeks = morbidity_weeks)

  if (lubridate::wday(mw_end, week_start = 7) != 7) {
    final_date <- mw_end
  }

  final_date <- from_date + (6 - lubridate::wday(from_date, week_start = 7))

  return(final_date %>% format('%Y%m%d'))
}
