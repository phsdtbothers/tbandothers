#' Provides week information based on a given date.
#'
#' Provides week information based on a given date. This
#'
#' @param run_date Optional. The date that the code will base the output on. If not given, then the current date will be used.
#'
#' @returns A data frame containing information regarding the current and previous weeks based on the provided run_date.
#' @export
get_global_dates <- function(run_date=Sys.Date()) {
  run_date <- as.Date(run_date)

  global_dates <- data.frame(run_date=run_date)

  # generate info for current week
  this_week <- tbandothers::get_morbidity_week_number(as.Date(run_date)) %>% tbandothers::get_morbidity_week_info(from_year=lubridate::year(run_date))

  global_dates$current_week <- this_week$week
  global_dates$current_dateshard <- global_dates$run_date - lubridate::days(lubridate::wday(global_dates$run_date) - 6)
  global_dates$current_start <- this_week$start
  global_dates$current_end <- this_week$end

  # generate info for previous week
  last_week <- this_week$start - 7
  last_week <- tbandothers::get_morbidity_week_number(last_week) %>% tbandothers::get_morbidity_week_info(from_year=lubridate::year(run_date))

  global_dates$previous_week <- last_week$week
  global_dates$previous_dateshard <- global_dates$current_dateshard <- global_dates$run_date - lubridate::days(lubridate::wday(global_dates$run_date) - 6)
  global_dates$previous_start <- last_week$start
  global_dates$previous_end <- last_week$end

  return(global_dates)
}
