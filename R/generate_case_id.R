#' Title
#'
#' @returns dataframe with populated case_id column
#'
#' @import dplyr
#' @import lubridate
#' @import utils
#'
#' @export
generate_case_id <- function(linelist, disease) {
  # generate max numbers per year
  case_id_max <- linelist %>%
    dplyr::mutate(
      year = lubridate::year(proxy_onset_date),
      case_id_num = strsplit(case_id, '-') %>%
        sapply(utils::tail, 1) %>%
        as.integer() %>%
        dplyr::coalesce(0)
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      case_id_max = max(case_id_num)
    )

  # iterate through line list, add 1 to case_id_max year based on proxy_onset_date, then generate case_id using case_id_format
  case_id_format <- paste0(disease, '-PHL-')

  linelist_w_caseid <- linelist %>% dplyr::filter(!is.na(case_id))
  linelist_wo_caseid <- linelist %>% dplyr::filter(is.na(case_id))

  for (idx in 1:nrow(linelist_wo_caseid)) {
    target_year <- lubridate::year(linelist_wo_caseid$proxy_onset_date[idx])
    target_max <- case_id_max %>% dplyr::filter(year == target_year) %>% dplyr::pull(case_id_max)

    new_max <- target_max + 1
    new_case_id <- paste0(case_id_format, target_year, '-', sprintf('%06d', new_max))

    linelist_wo_caseid$case_id[idx] <- new_case_id
    case_id_max[which(case_id_max$year == target_year), ]$case_id_max <- new_max
  }

  linelist <- dplyr::bind_rows(linelist_w_caseid, linelist_wo_caseid)

  return(linelist)
}
