utils::globalVariables(c('col_date', 'col_integer', 'cols', 'end', 'select', 'start', 'to_get', 'week', 'year'))

#' Gets morbidity week of a given date.
#'
#' @param date the date which we get the morbidity week for
#'
#' @returns Integer of morbidity week
#'
#' @import dplyr
#' @import lubridate
#' @import readr
#' @import magrittr
#'
#' @export
get_morbidity_week <- function(date) {
  from_date <- as.Date(date)

  # reads morbidity_weeks.csv, which contains all dates for morbidity weeks
  morbidity_weeks <- system.file('extdata', 'morbidity_weeks.csv', package='tbandothers') %>%
    readr::read_csv(
      show_col_types=FALSE,
      col_types = readr::cols(
         year = readr::col_integer(),
         week = readr::col_integer(),
         start = readr::col_date(format='%m/%d/%Y'),
         end = readr::col_date(format='%m/%d/%Y')
      )
    )

  morbidity_weeks$to_get <- from_date

  result <- morbidity_weeks %>%
    dplyr::filter(year == lubridate::year(from_date)) %>%
    dplyr::filter(start <= to_get & to_get <= end) %>%
    select(week)

  if (nrow(result) > 0) return(result$week)
  else return(NA)
}
