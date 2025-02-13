#' Gets the first date of a given morbidity week.
#'
#' @param week_number Desired week number.
#' @param from_year Optional. Which year's morbidity week calendar to reference. If not provided, then the current year is used.
#'
#' @import lubridate
#' @import dplyr
#'
#' @returns Date
#' @export
get_morbidity_week_start <- function(week_number, from_year=NULL) {
  if (is.null(from_year)) {
    from_year <- lubridate::year(Sys.Date())
  }

  mw_start <- get_morbidity_week_info(week_number, from_year)

  return(mw_start %>% dplyr::pull(start) %>% as.Date())
}
