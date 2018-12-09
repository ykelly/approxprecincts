#' Geotag voterfile addresses with 2010 Census block GEOIDs
#'
#' We refer to "geotagging" as assigning a Census geography id to an address. This function takes addresses from a voter file separated into residential address, city and state. It creates a vector of 2010 Census block GEOIDs that can be appended to the voter file dataframe.
#'
#' Sometimes there is an error with the Census API and the function fails, but your progress will be saved. To find out where the function left off, run the code below, then rerun the geotag function with the new start value to continue:
#'
#' x %>% mutate(row = row_number()) %>%
#'    select(BLOCK_GEOID, row) %>%
#'    drop_na(BLOCK_GEOID) %>%
#'    arrange(desc(row))
#'
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
#' If the first run encounters an error and the last geocoded row is the 10000th, for example:
#' geotag_voterfile(voterfile, start = 10001)
#'
#' Input the same file path as in the first run to overwrite your first file of geocoded addresses, or specify a new file path to create a new file for the second run.
#'
#' @export

library(tigris)
library(readr)

geotag_voterfile <- function(x, start = 1, file_path = ".", address_field = 'RESIDENTIAL_ADDRESS1', city_field = 'RESIDENTIAL_CITY',
                             state_field = 'RESIDENTIAL_STATE', runtime = TRUE){
  start_time <- Sys.time()
  if (start == 1) { x$BLOCK_GEOID <- NA }     # initialize column for GEOID if first run through function

  for (i in start:nrow(x)) {
    x$BLOCK_GEOID[[i]] <- call_geolocator(x[[address_field]][i], # for loop method
                                          x[[city_field]][i],
                                          x[[state_field]][i])
  }

  write_csv(x, file_path)      # write csv of progress, which is overwritten if this function is called again

  end_time <- Sys.time()
  runtime <- end_time - start_time
  coverage <- sum(!is.na(x$BLOCK_GEOID))/nrow(x)
  if(runtime) message(paste0("Time to geocode: ", as.character(runtime), " minutes"))
  message(paste0(as.character(coverage*100), "% of addresses were valid"))

  return(x)
}
