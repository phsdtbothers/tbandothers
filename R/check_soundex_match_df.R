check_soundex_match_df <- function(df, column_name, target_word) {

  # Soundex function
  soundex <- function(x) {
    sapply(x, function(word) {
      word <- toupper(word)
      soundex_code <- substr(word, 1, 1)
      word <- gsub("[^A-Z]", "", word)  # Remove non-alphabetical characters
      if (nchar(word) > 1) {
        word <- gsub("[AEIOUHWY]", "", substr(word, 2, nchar(word)))  # Remove vowels
        soundex_code <- paste0(soundex_code, substr(word, 1, 3))
      }
      soundex_code
    })
  }

  # Clean up the column text: replace non-word characters with spaces
  cleaned_column <- gsub("[^A-Za-z]", " ", df[[column_name]])

  # Split the column into words
  words_list <- strsplit(cleaned_column, " ")

  # Apply Soundex to each word in each row of the column
  soundex_words_list <- lapply(words_list, soundex)

  # Compute the Soundex for the target word
  soundex_target <- soundex(c(target_word))

  # Find matching Soundex words for each row
  matching_sounds <- sapply(soundex_words_list, function(words) {
    words[words %in% soundex_target]
  })

  # Add the matching words to the original data frame as a new column
  df$matching_words <- matching_sounds

  return(df)
}
