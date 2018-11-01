
## add documentation

st_rook <- function(a, b = a) sf::st_relate(a, b, pattern = "F***1****")

st_queen <- function(a, b = a) sf::st_relate(a, b, pattern = "F***T****")

lookup_precinct <- function(index, precinct_name_var = "PRECINCT_NAME"){
  precinct <- precincts_nb[index, precinct_name_var]
  return(precinct)
}

create_precinct_vector <- function(){
  prec <- purrr::map(precincts_nb$NB_ROOK, lookup_precinct)
  prec_vec <- purrr::map(prec, dplyr::pull)
  return(prec_vec)
}


clean_precincts_fun <- function(i){
  y <- precincts_nb_sub$precinct_nn[[i]]
  print(y)
  new_y <- y[!is.na(y)]
  return(new_y)
}
