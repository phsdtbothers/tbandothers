#' Downloads from translation reference
#'
#' Downloads the population reference from Google Sheet, and the given sheet name (acts as individual dictionaries for sorting).
#'
#' Sheet translations of system generated strings to more readable formats. Typically used for visualization generation.
#'
#' @param sheet_name Required, sheet name to download
#'
#' @returns named vector, sourced from translation google sheet, under provided sheet_name.
#'
#' @import googlesheets4
#'
#' @export
download_translations <- function(sheet_name) {
  translation <- googlesheets4::read_sheet('12nyuK_Sy8WWCMOGlrwROOCB1koAX5MFFMB_iw5eaIao', sheet=sheet_name)
  translation <- setNames(translation$translate, translation$raw)

  return(translation)
}
