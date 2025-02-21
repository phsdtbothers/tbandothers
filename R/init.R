#' @import bigrquery
#' @import googledrive
#' @import googlesheets4
.onLoad <- function(libname, pkgname) {
  # initialize BigQuery connection; requests authorization key input
  bigrquery::bq_auth()

  # initialize Google Drive connection
  googledrive::drive_auth()

  # initialize Google Sheets connection
  googlesheets4::gs4_auth()
}

# read morbidity week Google Sheet
morbidity_weeks <- googlesheets4::read_sheet('1lTwxbm96nTffaM0WG77lhpNcqDBlLDQJ3CfQFrWuAmE', sheet='morbidity_weeks')

morbidity_weeks <- morbidity_weeks %>% mutate(
  year = as.numeric(year),
  week = as.numeric(week),
  start = as.Date(start),
  end = as.Date(end)
)
