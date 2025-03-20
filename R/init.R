#' # read morbidity week Google Sheet - MUST INSTALL googlesheets4 package first
morbidity_weeks <- googlesheets4::read_sheet('1lTwxbm96nTffaM0WG77lhpNcqDBlLDQJ3CfQFrWuAmE', sheet='morbidity_weeks')

morbidity_weeks <- morbidity_weeks %>% mutate(
  year = as.numeric(year),
  week = as.numeric(week),
  start = as.Date(start),
  end = as.Date(end)
)
