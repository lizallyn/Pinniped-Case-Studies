### I give up, let's do it in R for the rest of the day

# packages and data


# functions
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Sockeye%20arrival%20function%20creation.R")

# Set Parameters
days <- 365
num_seals <- 20
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0
seal_num_neighbours_2_copy <- 2
seal_prob_2_copy <- 0.5
prob_seal_forage_success <- 0.3
max_salmon <- max(sockeye$DailyCount)

escape_rate <- 0.3

# Variable setup
salmon_escape <- rep(NA, days)
salmon_escape[1] <- 0
gauntlet_salmon <- rep(NA, days)
gauntlet_salmon[1] <- 0
salmon_consumed <- rep(NA, num_seals)
seal_prob_gauntlet <- array(dim = c(num_seals, days), 
                            data = rep(seal_initial_prob_gauntlet, 
                                       num_seals * days))
seal_forage_loc <- array(dim = c(num_seals, days), 
                         data = rep(seal_start_loc, 
                                    num_seals * days))

# Run time loop
for(t in 2:days) {
  
  # salmon at the gauntlet on that day=arrive-leave
  salmon_arrive <- round(predict.fish(day = t, params = fish.fit.optim$par, start.day = 163), digits = 0)
  gauntlet_salmon[t] <- gauntlet_salmon[t-1] + salmon_arrive - salmon_escape[t-1]
  salmon_escape[t] <- gauntlet_salmon[t] * escape_rate
  
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
  for(seal in 1:num_seals) {
    prob_seal_encounter_salmon <- gauntlet_salmon[t]/max_salmon
    prob_seal_forage_success <- rnorm(n = 1, mean = 20, sd = 7) #Freeman et al 2022
    salmon_consumed[seal] <- seal_forage_loc[seal] * prob_seal_encounter_salmon *
      gauntlet_salmon[t] * prob_seal_forage_success
  }

}
print(salmon_consumed)
