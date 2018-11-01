#' Lookup the precinct names of each block in a neighborhood.
#'
#' This function translates the vector of neighbors for each census block into a vector of precinct names. This is useful for the nearest neighbor classification.
#'
#' @param x Voterfile dataframe, output from find_neighbors()
#'
#' @return x with a column of precinct names for each element in the neighborhood
#'
#' @export



lookup_precincts_nn <- function(x){
  precinct_nn <- list()

  for (i in 1:nrow(x)) {
    precincts <- c()

    for (y in x$NB[[i]]){
      precincts <- c(precincts, x[[y, 'PRECINCT_NAME']])
    }

    precinct_nn[[i]] <- precincts

  }

  x$precinct_nn <- precinct_nn

  return(x)

}
