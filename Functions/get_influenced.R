# create get_influenced function
# not finished - realize this is going to go through some iterations still I bet

get_influenced <- function(seal_num_neighbours_2_copy, num_seals, forage_locs_list, prob_2_copy) {
  seals_to_be_influenced <- which(forage_locs_list == 0)
  loc_of_seals_2_copy <- forage_locs_list[sample(1:num_seals, seal_num_neighbours_2_copy, replace = F)]
  social_info <- mean(loc_of_seals_2_copy)
  if(runif(1, 0, 1) < (seal_prob_2_copy * social_info)) {
    forage_locs_list[seals_to_be_influenced] <- 1
  }
  return(forage_locs_list)
}

# get_influenced(seal_num_neighbours_2_copy = 2, num_seals = 10, forage_locs_list = forage_locs_list_test, prob_2_copy = seal_prob_2_copy)
