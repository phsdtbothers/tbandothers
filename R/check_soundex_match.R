#' Title
#'
#' @param strings_vector Vector or column to search soundex from
#' @param target_word Target word, will search for actual word or matching soundex code
#'
#' @import magrittr
#' @import stringr
#' @import purrr
#' @import phonics
#'
#' @returns Vector of booleans, for search output from strings_vector
check_soundex_match <- function(strings_vector, target_word) {
  # clean words
  strings <- dplyr::tibble(
    words = strings_vector
  )

  target_word <- target_word %>% toupper() %>% trimws()

  # clean words
  strings$words <- strings$words %>%
    toupper() %>%
    gsub('[^A-Za-z]', ' ', .) %>%
    gsub('\\s+', ' ', .) %>%
    trimws()

  # split words, and generate soundex of each word, find if soundex is in word
  strings <- strings %>%
    dplyr::mutate(
      words_split = strsplit(words, ' '),
      words_soundex = purrr::map_chr(words_split, ~ paste(phonics::soundex(.), collapse=' ')),
      words_soundex_in = stringr::str_detect(words_soundex, phonics::soundex(target_word)) | stringr::str_detect(words, paste0('(\\s|^|\\W)', target_word, '(\\s|$|\\W)'))
    )

  return(strings %>% pull(words_soundex_in))
}
