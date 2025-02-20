#' Provides week information based on a given date.
#'
#' Provides week information based on a given date. This contains the morbidity week info for the current week and the previous week. Note that if the week does not end on a friday or a saturday, then the dateshard generated will be the final day of the week.
#'
#' @param run_date Optional. The date that the code will base the output on. If not given, then the current date will be used.
#'
#' @returns A data frame containing information regarding the current and previous weeks based on the provided run_date.
#' @export
get_global_dates <- function(run_date=Sys.Date()) {
  run_date <- as.Date(run_date)

  global_dates <- data.frame(
    run_date=run_date,
    run_dateshard=run_date %>% format('%Y%m%d')
  )

  # generate info for current week
  this_week <- tbandothers::get_morbidity_week_number(as.Date(run_date)) %>% tbandothers::get_morbidity_week_info(from_year=lubridate::year(run_date))

  global_dates$current_week <- this_week$week
  global_dates$current_start <- this_week$start
  global_dates$current_end <- this_week$end
  global_dates$current_dateshard <- ifelse( # to account for morbidity weeks with special start dates (i.e. 2024 MW 52 does not end on a saturday)
    lubridate::wday(this_week$end) >= 6,
    this_week$start - lubridate::days(lubridate::wday(this_week$start) - 6),
    this_week$end
  ) %>% as.Date() %>% format('%Y%m%d') # format to BQ dateshard prefix

  # generate info for previous week
  last_week <- this_week$start - 7
  last_week <- tbandothers::get_morbidity_week_number(last_week) %>% tbandothers::get_morbidity_week_info(from_year=lubridate::year(last_week))

  global_dates$previous_week <- last_week$week
  global_dates$previous_start <- last_week$start
  global_dates$previous_end <- last_week$end
  global_dates$previous_dateshard <- ifelse( # to account for morbidity weeks with special start dates (i.e. 2024 MW 52)
    lubridate::wday(last_week$end) >= 6,
    last_week$start - lubridate::days(lubridate::wday(last_week$start) - 6),
    last_week$end
  ) %>% as.Date() %>% format('%Y%m%d') # format to BQ dateshard prefix

  return(global_dates)
}
