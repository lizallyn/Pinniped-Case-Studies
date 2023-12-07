# create get_influenced function


get_influenced <- function(forage_locs_list, num_seals, seal_num_neighbours_2_copy, seal_prob_2_copy){
  social_info <- mean(forage_locs_list[
    sample(1:num_seals, seal_num_neighbours_2_copy, replace = F)]) * seal_prob_2_copy
  if(runif(1, 0, 1) < social_info) {
    return(1)
  } else {return(0)}
}


