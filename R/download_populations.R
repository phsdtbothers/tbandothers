#' Downloads population
#'
#' Downloads the population reference from Google Sheet. Sheet contains year, provcity, region, population information for each province/HUC.
#'
#' @returns dataframe, sourced from population google sheet.
#'
#' @import googlesheets4
#'
#' @export
download_populations <- function() {
  populations <- googlesheets4::read_sheet('1szMIimHP1klIM__fPFlm5GVQFtCmXWDPYhqhPT9hsI8', sheet='population')

  return(populations)
}
