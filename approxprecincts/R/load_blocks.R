#' Load 2010 census blocks for a county
#'
#' Takes state and county as inputs and returns an 'sf' dataframe of 2010 census blocks using the {tigris} package.
#'
#' @param state Postal abbreviation or 2 digit FIPS code of US state \code{state}
#' @param county County name or 3 digit FIPS code \code{county}
#'
#' @return output 'sf' dataframe of 2010 census blocks for the given county
#'
#' @keywords census, blocks
#'
#'
#' \dontrun
#' @examples
#' load_blocks(state = "OH", county = "Vinton")
#' load_blocks(state = 39, county = 165) # these will produce the same result
#'
#' @export

load_blocks <- function(state = "OH", county){
  x <- tigris::blocks(state = state, county = county)
  x_sf <- sf::st_as_sf(x)
  return(x_sf)
}
