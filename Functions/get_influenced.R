# create get_influenced function
# not finished - realize this is going to go through some iterations still I bet

for(seal in 1:num_seals) {
  # if the seal wasn't going to the gauntlet
  # if(seal_forage_loc[seal,t, y] == 0) {
  loc_of_seals_2_copy <- seal_forage_loc[sample(1:num_seals, seal_num_neighbours_2_copy, replace = F),t, y]
  social_information <- mean(loc_of_seals_2_copy)
  if(runif(1, 0, 1) < (seal_prob_2_copy * social_information)) {
    seal_forage_loc[seal,t, y] <- 1
    # }
  }
}

get_influenced <- function(num_2_copy, num_seals, seal_forage_loc, prob_2_copy) {
  loc_of_seals_2_copy <- seal_forage_loc[sample(1:num_seals, num_2_copy, replace = F)]
  social_info <- mean(loc_of_seals_2_copy)
  if(runif(1, 0, 1) < (prob_2_copy * social_info)) {
    seal_forage_loc <- social_info
  }
}