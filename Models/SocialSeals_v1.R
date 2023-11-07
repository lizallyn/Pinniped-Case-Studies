### I give up, let's do it in R for the rest of the day

# packages and data


# functions
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Sockeye%20arrival%20function%20creation.R")

# Set Parameters
days <- 1
num_seals <- 20
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0
seal_num_neighbours_2_copy <- 2
seal_prob_2_copy <- 0.5
prob_seal_forage_success <- 0.3

# Variable setup
salmon_arrive <- rep(NA, days)
seal_prob_gauntlet <- array(dim = c(num_seals, days), 
                            data = rep(seal_initial_prob_gauntlet, 
                                       num_seals * days))
seal_forage_loc <- array(dim = c(num_seals, days), 
                         data = rep(seal_start_loc, 
                                    num_seals * days))

# Run time loop
for(t in 1:days) {
  
  salmon_arrive[t] <- predict.fish(day = t, params = fish.fit.optim$par, start.day = 163)
  
  # decide where each seal goes that day
  for(seal in 1:num_seals) {
    if(rnorm(1) < seal_prob_gauntlet[seal,t]) 
    {seal_forage_loc[seal,t] <- 1} 
    else seal_forage_loc[seal,t] <- 0
  }
  
  # round of copying
  for(seal in 1:num_seals) {
    # if the seal wasn't going to the gauntlet
    if(seal_forage_loc[seal,t] == 0) {
      seals_2_copy <- seal_forage_loc[sample(1:num_seals, seal_num_neighbours_2_copy, replace = F)]
      social_information <- mean(seals_2_copy)
      if(rnorm(1) < seal_prob_2_copy * social_information) {
        seal_forage_loc[seal] <- 1
      }
    }
  }
  
  # round of eating
  prob_seal_encounter_salmon <- salmon_arrive[t]/
  # not sure what this should be dependent on yet, probably density and competition
  for(seal in 1:num_seals) {
    salmon_consumed[seal] <- seal_forage_loc[seal] * prob_seal_encounter_salmon * prob_seal_forage_success[seal]
  }

}
print(seal_forage_loc)
