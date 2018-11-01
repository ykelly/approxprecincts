#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voterfile separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voterfile dataframe.
#'
#' @param x Voterfile dataframe \code{x}
#' @param address_field Field in x that refers to the street address (house number & street name) \code{address_field}
#' @param city_field Field in x that refers to the city name \code{city_field}
#' @param state_field Field in x that refers to the state name \code{state_field}
#' @param runtime If TRUE, returns the runtime of the geocoding as a message, meaured as start sys.Time() - end sys.Time(). Default = TRUE \code{runtime}
#'
#'
#' @return vector of block GEOIDs that can be appended to the voterfile dataframe with dplyr::bind_rows()
#'
#' @keywords blocks, geotag
#'
#' \dontrun
#' @examples
#' geotag_voterfile(voterfile, 'RESIDENTIAL_ADDRESS1', 'RESIDENTIAL_CITY', 'RESIDENTIAL_STATE')
#'
#' @export

library(purrr)
library(tigris)

geotag_voterfile <- function(x, address_field = 'RESIDENTIAL_ADDRESS1', city_field = 'RESIDENTIAL_CITY', state_field = 'RESIDENTIAL_STATE', runtime = TRUE){
  start_time <- Sys.time()
  vec <- map_chr(1:nrow(x), function(i) call_geolocator(x[[address_field]][i], x[[city_field]][i], x[[state_field]][i]))
  end_time <- Sys.time()
  runtime <- end_time - start_time
  coverage <- sum(!is.na(vec))/length(vec)
  if(runtime) message(paste0("Time to geocode: ", as.character(runtime), " minutes"))
  message(paste0(as.character(coverage), "% of addresses were valid"))
  x$BLOCK_GEOID <- vec

  return(x)
}
