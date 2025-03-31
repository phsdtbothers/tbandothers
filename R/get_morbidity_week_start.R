#' Gets the first date of a given morbidity week.
#'
#' @param week_number Desired week number.
#' @param from_year Optional. Which year's morbidity week calendar to reference. If not provided, then the current year is used.
#' @param morbidity_weeks Optional. Morbidity week reference from tbandothers::download_morbidity_weeks. To be used to skip downloading per use.
#'
#' @import lubridate
#' @import dplyr
#'
#' @returns Date
#' @export
get_morbidity_week_start <- function(week_number, from_year=lubridate::year(Sys.Date()), morbidity_weeks=NULL) {
  # if morbidity_weeks is not given, then download
  if (is.null(morbidity_weeks)) {
    morbidity_weeks <- tbandothers::download_morbidity_weeks()
  }

  mw_start <- get_morbidity_week_info(week_number, from_year)

  return(mw_start %>% dplyr::pull(start) %>% as.Date())
}
