#' Title
#'
#' @returns final disease dataframe
#' @export
#'
#' @import dplyr
deduplicate_hfmd <- function() {
  # cases_new <- test_dedup_2
  cases_new$is_new <- 'Y'
  # cases_new$last_modified <- as.Date("2000-01-01") + sample(0:10000, 50)

  # get latest line list (only required rows)
  # cases_prev <- test_dedup_1
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

  # get points per requirement, higher the points, the higher the priority to keep
  cases_all$dup_points <- 0

  # ... if last_modified = latest (+1)
  cases_all$dup_points <- ifelse(
    is.na(cases_all$last_modified),
    cases_all$dup_points,
    cases_all$dup_points + (as.numeric(min(cases_all$last_modified, na.rm=TRUE) - cases_all$last_modified) / as.numeric(min(cases_all$last_modified, na.rm=TRUE) - max(cases_all$last_modified, na.rm=TRUE)))
  )

  # ... if is_new = 'Y' (+1)
  cases_all$dup_points <- ifelse(
    cases_all$is_new == 'Y',
    cases_all$dup_points + 1,
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

  # only include highest points
  # ... compile highest dup_points
  cases_points <- cases_all %>%
    dplyr::group_by(dup_id) %>%
    dplyr::summarise(
      dup_points_max = max(dup_points)
    )

  # ... merge to same dataframe for easy access
  cases_all <- cases_all %>%
    dplyr::left_join(cases_points, by='dup_id')

  # ... filter table per dup_id and dup_points
  cases_all <- cases_all %>%
    dplyr::filter(cases_all$dup_points == cases_all$dup_points_max)

  # TODO generate case id when null

}

# query <- 'SELECT * FROM `hfmd_source.30_hfmd_latest` LIMIT 100'
# output <- bigrquery::bq_table_download(bigrquery::bq_project_query('doh-covid-dwh', query))
# test_dedup_1 <- output %>% slice_tail(n=50)
# test_dedup_2 <- output %>% slice_tail(n=50)
# #
# test_dedup_2$case_id <- NA


