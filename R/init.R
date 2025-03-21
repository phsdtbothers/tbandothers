#' @import googlesheets4
# read morbidity week Google Sheet - MUST INSTALL AND AUTHORIZE googlesheets4 LOCALLY BEFORE LOADING
morbidity_weeks <- googlesheets4::read_sheet('1lTwxbm96nTffaM0WG77lhpNcqDBlLDQJ3CfQFrWuAmE', sheet='morbidity_weeks')

morbidity_weeks <- morbidity_weeks %>% mutate(
  year = as.numeric(year),
  week = as.numeric(week),
  start = as.Date(start),
  end = as.Date(end)
)
