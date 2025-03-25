#' Gets the FHSIS population estimate given the year, region, and province/HUC.
#'
#' @param to_year year of population estimates
#' @param to_region region of population estimates
#' @param to_provcity province/HUC of population estimates
#' @param population Optional. Population reference from tbandothers::download_populations. To be used to skip downloading per use.
#'
#' @import magrittr
#' @import dplyr
#'
#' @returns integer of population
#' @export
get_population <- function(to_year, to_region, to_provcity, population=NULL) {
  # if population is not given, then download
  if (is.null(population)) {
    population <- tbandothers::download_populations()
  }

  final_population <- population %>%
    dplyr::filter(year == to_year & provcity == to_provcity & region == to_region)

  if (nrow(final_population) == 0) (
    return(NA)
  )
  return(final_population$population)
}
