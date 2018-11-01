#' Join the geotagged voterfile to 2010 census blocks
#'
#' Use this function to assign 2010 census blocks to a particular precinct based on the most common precinct assignment of the voters geotagged there.
#'
#' @param x Geotagged voterfile dataframe, should be output from geotag_voterfile()
#' @param blocks 'sf' dataframe of 2010 census blocks. Use load_blocks() to load the blocks for your county
#' @param block_geoid_voters Field in x that refers to the census block geoid. Should be called 'BLOCK_GEOID' from geotag_voterfile() output.
#' @param precinct_name Field in x that refers to the precinct name. Default= 'PRECINCT_NAME'.
#'
#'
#' @return dataframe of blocks with assigned precincts using the most common precinct of the voters in that block
#'
#' @keywords blocks, voterfile
#'
#' \dontrun
#' @examples
#' join_voters_to_blocks(noble_voters, noble_blocks, block_geoid_voters = "GEOID10")
#'
#' @export

join_voters_to_blocks <- function(voters, blocks, block_geoid_voters = "BLOCK_GEOID", precinct_name = "PRECINCT_NAME"){
  colnames(voters)[colnames(voters)==block_geoid_voters] <- 'BLOCK_GEOID'
  colnames(voters)[colnames(voters)==precinct_name] <- 'PRECINCT_NAME'
  precincts <- voters %>%
    # mutate(BLOCK_GEOID = as.character(!! block_geoid_q)) %>%
    # rename(PRECINCT_NAME = (!! precinct_name_q)) %>%
    dplyr::group_by(BLOCK_GEOID, PRECINCT_NAME) %>% # for each block, precinct combination
    dplyr::summarise(c=n()) %>% # counts the number of times a precinct is counted for a particular block
    dplyr::filter(row_number(desc(c))==1) # dataframe of precincts from voterfile, takes the most common precinct assignment for a block

  precincts_geo <- blocks %>%
    dplyr::mutate(GEOID10 = as.character(GEOID10)) %>%
    dplyr::left_join(precincts %>% st_set_geometry(NULL), by = c("GEOID10" = "BLOCK_GEOID")) %>% # combine precincts with block shapefile
    dplyr::mutate(dimension = st_dimension(.)) %>%
    dplyr::filter(!(is.na(dimension))) # take out empty polygons

  return(precincts_geo)

}
