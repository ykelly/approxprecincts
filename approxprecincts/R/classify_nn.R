#' Classify each unclassified block with a precinct name based on the most frequent classification of the blocks in its neighborhood
#'
#' @param x Voterfile dataframe, output from lookup_precincts_nn()
#'
#' @return x with 'PRECINCT_NAME' filled in for blocks with >=1 classified neighbor
#'
#' @export


classify_nn <- function(x){
  precincts_nb_sub <- x %>%
    filter(!is.na(precinct_nn),
           !identical(precinct_nn, character(0)))

  clean_precincts_fun <- function(i){
    y <- precincts_nb_sub$precinct_nn[[i]]
    new_y <- y[!is.na(y)]
    return(new_y)
  }

  p_new <- map(1:nrow(precincts_nb_sub), clean_precincts_fun)

  p_lengths <- map(1:length(p_new), function(x) length(p_new[[x]]))

  precincts_nb_sub <- precincts_nb_sub %>%
    mutate(precinct_nn_clean = p_new,
           precinct_nn_length = p_lengths) %>%
    filter(precinct_nn_length > 0) # take out the blocks without meaningful neighbors

  for (i in 1:nrow(precincts_nb_sub)){
    precincts_nb_sub$PRECINCT_NAME[i] <- ifelse(is.na(precincts_nb_sub$PRECINCT_NAME[i]), names(which.max(table(precincts_nb_sub$precinct_nn[[i]]))), precincts_nb_sub$PRECINCT_NAME[i])
  } # assign a precinct as the max of the vector of precincts

  precincts_nb_full <- x %>%
    left_join(precincts_nb_sub %>% st_set_geometry(NULL) %>% select(GEOID10, PRECINCT_NAME), by = "GEOID10") %>%
    mutate(PRECINCT_NAME.x = if_else(is.na(PRECINCT_NAME.x), PRECINCT_NAME.y, PRECINCT_NAME.x)) %>%
    select(-PRECINCT_NAME.y) %>%
    rename(PRECINCT_NAME = PRECINCT_NAME.x)

  message(paste0("There are ", sum(is.na(x$PRECINCT_NAME)), " unclassified blocks."))

  return(precincts_nb_full)
}
