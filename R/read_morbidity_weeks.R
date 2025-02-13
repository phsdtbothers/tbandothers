#' Reads TB and Others morbidity week Google Sheet.
#'
#' Reads TB and Others morbidity week Google Sheet. Only authorized when first ran during session. No confidential information is stored in the Google Sheet.
#'
#' @import googlesheets4
#' @import dplyr
#' @import magrittr
#'
#' @returns A data frame of all stored morbidity weeks.
#' @export
read_morbidity_weeks <- function() {
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

  return(morbidity_weeks)
}
