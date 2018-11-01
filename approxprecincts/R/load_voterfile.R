#' Load the public voterfile for a county
#'
#' Loads a csv of the public voterfile in a county. Currently only supports Ohio counties
#'
#' @param state Postal abbreviation of a US state \code{state}
#' @param county County name \code{county}
#'
#' @return csv of most current voterfile for the county
#'
#' @keywords voterfiles
#'
#' \dontrun
#' @examples
#' load_voterfile(state = "oh", county = "williams")
#'
#' @export

library(readr)

load_voterfile <- function(state = "OH", county){
  url <- "https://raw.githubusercontent.com/ykelly/approxprecincts/master/voterfiles%20_ohio.csv"
  csv <- suppressMessages(read_csv(url(url)))
  sub <- csv[ which(csv$State== toupper(state) & csv$County == toupper(county)), ]
  link <- sub[[1, 3]]
  voterfile <- read_csv(url(link))
  return(voterfile)
}
