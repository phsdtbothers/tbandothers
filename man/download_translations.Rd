% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_translations.R
\name{download_translations}
\alias{download_translations}
\title{Downloads from translation reference}
\usage{
download_translations(sheet_name)
}
\arguments{
\item{sheet_name}{Required, sheet name to download}
}
\value{
named vector, sourced from translation google sheet, under provided sheet_name.
}
\description{
Downloads the population reference from Google Sheet, and the given sheet name (acts as individual dictionaries for sorting).
}
\details{
Sheet translations of system generated strings to more readable formats. Typically used for visualization generation.
}
