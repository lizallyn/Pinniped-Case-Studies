### I give up, let's do it in R for the rest of the day

# Load Packages and Data


# Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Sockeye%20arrival%20function%20creation.R")

# Set Parameters
days <- 365
num_seals <- 20
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0
seal_num_neighbours_2_copy <- 2
seal_prob_2_copy <- 0.5
prob_seal_forage_success <- 0.3
satiation_threshold <- 5

max_salmon <- max(sockeye$DailyCount)
escape_rate <- 0.3

# Set Up Variables
salmon_escape <- rep(NA, days)
salmon_escape[1] <- 0
gauntlet_salmon <- rep(NA, days)
gauntlet_salmon[1] <- 0

salmon_consumed <- array(dim = c(num_seals, days), 
                            data = rep(0, num_seals * days))
seal_prob_gauntlet <- array(dim = c(num_seals, days), 
                            data = rep(seal_initial_prob_gauntlet, 
                                       num_seals * days))
seal_forage_loc <- array(dim = c(num_seals, days), 
                         data = rep(seal_start_loc, 
                                    num_seals * days))

# Run time loop
for(t in 2:(days-1)) {
  
  # salmon at the gauntlet on that day=arrive-leave
  salmon_arrive <- round(predict.fish(day = t, params = fish.fit.optim$par, start.day = 163), digits = 0)
  gauntlet_salmon[t] <- round(gauntlet_salmon[t-1] + salmon_arrive - salmon_escape[t-1], digits = 0)
  
  
  # decide where each seal goes that day
  for(seal in 1:num_seals) {
    if(runif(1, 0, 1) < seal_prob_gauntlet[seal,t]) {
      seal_forage_loc[seal,t] <- 1
    } else seal_forage_loc[seal,t] <- 0
  }
  
  # round of copying
  for(seal in 1:num_seals) {
    # if the seal wasn't going to the gauntlet
    if(seal_forage_loc[seal,t] == 0) {
      loc_of_seals_2_copy <- seal_forage_loc[sample(1:num_seals, seal_num_neighbours_2_copy, replace = F),t]
      social_information <- mean(loc_of_seals_2_copy)
      if(runif(1, 0, 1) < seal_prob_2_copy * social_information) {
        seal_forage_loc[seal,t] <- 1
      }
    }
  }
  
  # feeding as random provisioning
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  salmon_available_to_eat <- gauntlet_salmon[t]
  
  if(length(seals_at_gauntlet) < 1) {
    salmon_consumed[,t] <- 0
  } else {
    while(length(seals_at_gauntlet) > 0) {
      if(salmon_available_to_eat < length(seals_at_gauntlet)) {
        break
      } else {
        seals_that_eat <- sample(seals_at_gauntlet, sample(1:length(seals_at_gauntlet), 1), replace = F)
        salmon_consumed[seals_that_eat,t] <- salmon_consumed[seals_that_eat,t] + 1
        salmon_available_to_eat <- salmon_available_to_eat - length(seals_that_eat)
        # remove satiated seals
        satiated_seals <- seals_that_eat[which(salmon_consumed[seals_that_eat,t] >= satiation_threshold)]
        seals_at_gauntlet <- seals_at_gauntlet[which(!seals_at_gauntlet %in% satiated_seals)]
      }
    }
  }
  
  
  # consumption impacts salmon survival
  gauntlet_salmon[t] <- gauntlet_salmon[t] - sum(salmon_consumed[,t])
  salmon_escape[t] <- gauntlet_salmon[t] * escape_rate
  
  
  # seal foraging success impacts prob gauntlet on next time step
  for(seal in seals_at_gauntlet) {
    if()
    seal_prob_gauntlet[seal, t+1] <- (salmon_consumed[seal, t]/5) + (salmon_consumed[seal,t-1]/10)
  }
}

## check it out:
# number of seals at the gauntlet per day
plot(1:days, colSums(seal_forage_loc)) # looks like no response to salmon
# salmon at the gauntlet per day
plot(1:days, gauntlet_salmon) # looks almost right, some negative
# successful foraging seals per day at the gauntlet
plot(1:days, colSums(salmon_consumed))

