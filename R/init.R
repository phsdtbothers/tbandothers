#' @import googlesheets4
#' @import utils
# read morbidity week Google Sheet - MUST INSTALL AND AUTHORIZE googlesheets4 LOCALLY BEFORE LOADING
morbidity_weeks <- googlesheets4::read_sheet('1lTwxbm96nTffaM0WG77lhpNcqDBlLDQJ3CfQFrWuAmE', sheet='morbidity_weeks')

morbidity_weeks <- morbidity_weeks %>% mutate(
  year = as.numeric(year),
  week = as.numeric(week),
  start = as.Date(start),
  end = as.Date(end)
)

# read populations
populations <- googlesheets4::read_sheet('1szMIimHP1klIM__fPFlm5GVQFtCmXWDPYhqhPT9hsI8', sheet='population')

# declare global variables (for no comments during devtools::check())
utils::globalVariables(c('case_id', 'case_id_num', 'dup_completeness', 'dup_completeness_match','dup_completeness_max', 'dup_id', 'dup_points', 'dup_points_match', 'dup_points_max', 'end', 'hfmd_latest', 'proxy_onset_date', 'start', 'population', 'region', 'provcity'))
