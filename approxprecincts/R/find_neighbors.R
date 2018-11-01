#' Assign a neighborhood to each block
#'
#' Using rook or queen contiguity, create a vector of neighbors for each block. Queen contiguity includes corners while rook does not. Rook contiguity is most commonly used.
#'
#' @param x Voterfile dataframe
#' @param type Neighborhood type ('rook' or 'queen')
#'
#'
#' @return x with a neighborhood column
#'
#' @keywords neighbors
#'
#' \dontrun
#' @examples
#' find_neighbors(noble_join, type = 'rook')
#'
#' @export


find_neighbors <- function(x, type = "rook"){
  if (type == "rook"){
    NB <- st_rook(x)
  } else if (type == "queen") {
    NB <- st_queen(x)
  } else {
    stop("Please enter 'rook' or 'queen' for neighbor type")
  }
  x$NB <- NA
  for (i in 1:length(NB)){
    x$NB[i] <- NB[i]
  }

  return(x)
}
