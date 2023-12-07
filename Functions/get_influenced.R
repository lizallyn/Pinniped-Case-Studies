# create get_influenced function
# not finished - realize this is going to go through some iterations still I bet

# get_influenced <- function(seal_num_neighbours_2_copy, num_seals, forage_locs_list, prob_2_copy) {
#   seals_to_be_influenced <- which(forage_locs_list == 0)
#   loc_of_seals_2_copy <- forage_locs_list[sample(1:num_seals, seal_num_neighbours_2_copy, replace = F)]
#   social_info <- seal_prob_2_copy * mean(loc_of_seals_2_copy)
#   flip_copy_coin <- function(social_info) {
#     if(runif(1, 0, 1) < (social_info)) {
#       return(1)
#     } else {return(0)}
#   }
#   sapply()
#   }
#   return(forage_locs_list[seal_to_be_influenced])
# }
# forage_locs_list_test <- sample(c(0,1),20, replace = T)

get_influenced <- function(forage_locs_list, num_seals, seal_num_neighbours_2_copy, seal_prob_2_copy){
  social_info <- mean(forage_locs_list[
    sample(1:num_seals, seal_num_neighbours_2_copy, replace = F)]) * seal_prob_2_copy
  if(runif(1, 0, 1) < social_info) {
    return(1)
  } else {return(0)}
}


# get_social_info(forage_locs_list_test, num_seals, seal_num_neighbours_2_copy, seal_prob_2_copy)
# replicate(10, get_social_info(forage_locs_list_test, num_seals, seal_num_neighbours_2_copy, seal_prob_2_copy))
