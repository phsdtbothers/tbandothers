#' Check for acronyms in strings
#'
#' returns a boolean or a vector of booleans that checks for acronyms. note that arguments are all case sensitive, and it is recommended to only use capital letters for all arguments.
#'
#' @param strings_vector a string or a vector of strings to look from acronyms from
#' @param target_acronym acronym to look for. only include letters here, periods for each letter are not needed. (e.g. "JE", not "J.E.")
#'
#' @returns a boolean or a vector of booleans for inputted string or strings
#' @export
#'
#' @import stringr
#'
#' @examples
#' check_acronym("WITH J.E.", "JE")
#' check_acronym(c("WITH HFMD", "HAS HFMD", "NONE"), "HFMD")
check_acronym <- function(strings_vector, target_acronym) {
  acronym_code <- strsplit(target_acronym, split='') %>%
    unlist() %>%
    paste0(collapse='(|.)')

  regexp_code <- paste0('(\\s|^|\\W)', acronym_code, '(|.)(\\s|$|\\W)')

  return(stringr::str_detect(strings_vector, regexp_code))
}
