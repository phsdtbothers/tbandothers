#' Deduplicates HFMD cases
#'
#' Deduplicates cases based on first name, last name, birthdate, and disease onset date. Prioritizes case definition (prioritizes confirmed), and outcome (prioritizes 'D').
#'
#' @param cases_new required, line list for this week
#' @param cases_prev required, line list containing cases from previous weeks
#' @param run_date optional, gets system date if not provided
#'
#' @returns final disease dataframe
#' @export
#'
#' @import dplyr
#' @import lubridate
#' @import stringr
deduplicate_hfmd <- function(cases_new, cases_prev, run_date = Sys.Date()) {
  # --- PRE-PROCESSING ---
  # standardize schema of cases_new to cases_prev
  cols_same <- lubridate::intersect(names(cases_prev), names(cases_new))
  cols_diff <- lubridate::setdiff(names(cases_prev), names(cases_new))

  for (col in cols_diff) {
    cases_new[[col]] <- NA
  }

  cases_new <- cases_new[, c('last_modified', names(cases_prev))]

  # get last week info (for point reference)
  this_week <- tbandothers::get_global_dates(run_date)

  # set is_new for new cases
  cases_new$case_id <- NA
  cases_new$is_new <- 'Y'

  # get latest line list (only required rows); and reset is_new
  cases_prev$is_new <- 'N'

  # merge line lists
  cases_all <- dplyr::bind_rows(cases_new, cases_prev)

  # group by names, proxy_onset_date, birth date
  cases_all$dup_id <- paste0(
    format(cases_all$proxy_onset_date, '%Y%m%d'),
    format(cases_all$birth_date, '%Y%m%d'),
    gsub('(\\W|\\s+)', '', cases_all$first_name),
    gsub('(\\W|\\s+)', '', cases_all$last_name)
  )

  # --- DUPLICATE HANDLING ---
  # get points for row completeness
  cases_all$dup_completeness <- (rowSums(!is.na(cases_all)) / ncol(cases_all))

  # get points per requirement, higher the points, the higher the priority to keep
  cases_all$dup_points <- 0

  # ... if is_new = 'Y', get points based time since last modified and run date (+0-1)
  cases_all$dup_points <- ifelse(
    cases_all$is_new == 'Y',
    cases_all$dup_points + (as.numeric(this_week$previous_start - cases_all$last_modified) / as.numeric(this_week$previous_start - this_week$previous_end)),
    cases_all$dup_points
  )

  # ... if final_disease_classification = confirmed (+5)
  cases_all$dup_points <- ifelse(
    grepl('CONFIRMED', cases_all$final_disease_classification, fixed=TRUE),
    cases_all$dup_points + 5,
    cases_all$dup_points
  )

  # ... if outcome = dead (+10)
  cases_all$dup_points <- ifelse(
    cases_all$outcome == 'D',
    cases_all$dup_points + 10,
    cases_all$dup_points
  )

  # --- DUPLICATE SIMPLIFYING ---
  # gets years integer value of case_id (for case_id generation to prioritize previously generated case_ids)
  cases_all$dup_case_id <- stringr::str_sub(cases_all$case_id, -7) %>% as.integer()

  # only include highest points and completeness
  # ... compile highest dup_points
  cases_points <- cases_all %>%
    dplyr::group_by(dup_id) %>%
    dplyr::summarise(
      dup_points_max = max(dup_points),
      dup_completeness_max = max(dup_completeness),
      dup_case_id_min = min(dup_case_id)
    )

  # ... merge to same dataframe for easy access
  cases_all <- cases_all %>%
    dplyr::left_join(cases_points, by='dup_id')

  # ... set case_id to lowest dup_case_id (to retain case_id of earliest duplicate)
  cases_all <- cases_all %>%
    mutate(
      case_id = ifelse(
        is.na(case_id) & !is.na(dup_case_id_min), # no case id, but previous case_id found
        paste0('HFMD-PHL-', lubridate::year(proxy_onset_date), '-', sprintf('%07d', dup_case_id_min)),
        case_id
      )
    )

  # ... filter table per dup_id and dup_points
  cases_all <- cases_all %>%
    dplyr::mutate(dup_points_match = dup_points == dup_points_max) %>%
    dplyr::mutate(dup_completeness_match = dup_completeness == dup_completeness_max) %>%
    dplyr::filter(dup_points_match) %>%
    dplyr::filter(dup_completeness_match)

  # --- FINALIZATION ---
  # remove temporary deduplication columns
  cases_all$dup_id <- NULL
  cases_all$dup_case_id <- NULL
  cases_all$dup_case_id_min <- NULL
  cases_all$dup_points <- NULL
  cases_all$dup_points_match <- NULL
  cases_all$dup_points_max <- NULL
  cases_all$dup_completeness <- NULL
  cases_all$dup_completeness_match <- NULL
  cases_all$dup_completeness_max <- NULL
  cases_all$last_modified <- NULL

  # ... get distinct rows only
  cases_all <- cases_all %>% dplyr::distinct()


  # generate case id
  cases_all <- tbandothers::generate_case_id(cases_all, 'HFMD')

  return(cases_all)
}
