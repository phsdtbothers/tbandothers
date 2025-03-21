#' Gets the FHSIS population estimate given the year, region, and province/HUC.
#'
#' @param to_year year of population estimates
#' @param to_region region of population estimates
#' @param to_provcity province/HUC of population estimates
#'
#' @import magrittr
#' @import dplyr
#'
#' @returns integer of popoulation
#' @export
get_population <- function(to_year, to_region, to_provcity) {
  final_population <- populations %>%
    dplyr::filter(year == to_year & provcity == to_provcity & region == to_region) %>%
    dplyr::pull(population)

  return(final_population)
}
