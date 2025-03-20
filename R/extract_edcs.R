#' Extracts from EDCS modified within the week previous to the run_date.
#'
#' Extracts from EDCS modified within the week previous to the run_date. Extracts both EDCS and EDCS lab profiles that were last modified in the week before the given run_date.
#'
#' @param bq_project BigQuery project where the data is found.
#' @param bq_edcs_dataset BigQuery dataset where EDCS data is found.
#' @param bq_disease Code of disease on BigQuery.
#' @param run_date Optional. Date to base the morbidity week info off of. If none is given, then the current date will be used.
#'
#' @returns A dataframe that left joins the EDCS patient and lab modules. All the information from the patient module is maintained, and only the unique lab module columns are joined.
#' @import bigrquery
#' @export
extract_edcs <- function(bq_project, bq_edcs_dataset, bq_disease, run_date=Sys.Date()) {
  date_info <- tbandothers::get_global_dates(run_date=run_date)
  date_today <- run_date %>% format('%Y%m%d')

  # get columns from edcs patient and laboratory profiles
  schema_pat <- bigrquery::bq_table_fields(bigrquery::bq_table(bq_project, bq_edcs_dataset, paste0(bq_disease, '_view_', date_today))) %>% sapply(function(x) x$name)
  schema_lab <- bigrquery::bq_table_fields(bigrquery::bq_table(bq_project, bq_edcs_dataset, paste0(bq_disease, '_view_lab_', date_today))) %>% sapply(function(x) x$name)

  schema_lab_only <- schema_lab[!schema_lab %in% schema_pat]

  # call from bigquery, get all pat fields, then new lab fields
  query <- paste0("SELECT pat.*, ", paste0('lab.', schema_lab_only, collapse=','), " FROM ", bq_edcs_dataset, ".", bq_disease, "_view_", date_today, " pat LEFT JOIN ", bq_edcs_dataset, ".", bq_disease, "_view_lab_", date_today, " lab ON pat.case_id = lab.case_id WHERE pat.last_modified_date BETWEEN '", date_info$previous_start, "' AND '", date_info$previous_end, "' OR lab.last_modified_date BETWEEN '", date_info$previous_start, "' AND '", date_info$previous_end, "'")

  output <- bigrquery::bq_table_download(bigrquery::bq_project_query(bq_project, query))

  return(output)
}
