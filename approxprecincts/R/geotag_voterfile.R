#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voterfile separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voterfile dataframe.
#'
#' @param x Voterfile dataframe \code{x}
#' @param start Line in \code{x} to start geocoding from (updated from default value of 1 in case of geocoding error)
#' @param file_path File path for output csv file of geocoded addresses, including file name
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
library(readr)

geotag_voterfile <- function(x, start = 1, file_path, address_field = 'RESIDENTIAL_ADDRESS1', city_field = 'RESIDENTIAL_CITY',
                             state_field = 'RESIDENTIAL_STATE', runtime = TRUE){
  start_time <- Sys.time()
  if (start == 1) { x$BLOCK_GEOID <- NA }     # initialize column for GEOID if first run through function

  for (i in start:nrow(x)) {
    x$BLOCK_GEOID[[i]] <- call_geolocator(x[[address_field]][i], # for loop method
                                          x[[city_field]][i],
                                          x[[state_field]][i])
  }

  i <- 8

  write_csv(x, file_path)      # write csv of progress, which is overwritten if this function is called again

  if (i < nrow(x)) {         # save progress in new dataframe called countyname_new
    stop(paste0("Geocoding interrupted. Your progress left off at line ", i, " and was saved in a vector.\nPlease run the code below to restart from this point:
                              \ngeotag_voterfile(x = x_new, start = ", i+1, ")"))
  }

  end_time <- Sys.time()
  runtime <- end_time - start_time
  coverage <- sum(!is.na(x$BLOCK_GEOID))/nrow(x)
  if(runtime) message(paste0("Time to geocode: ", as.character(runtime), " minutes"))
  message(paste0(as.character(coverage*100), "% of addresses were valid"))

  return(x)
}
