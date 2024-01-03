### V6
# Jan 2024

#### Set Up ####

## Load Packages and Data
library(ggplot2)
library(tidyr) #formatting for visualization

## Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/salmon_arrive.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/eat_some_fish_2.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/decide_foraging_destination.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/get_influenced.R")

## Set Parameters

# loop parameters
years <- 2
days <- 365

# seal parameters
num_seals <- 25
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0
seal_num_neighbours_2_copy <- 2
seal_prob_2_copy <- 0.5
seal_handling_time <- 0.1
seal_satiation <- 5

# salmon parameters
escape_rate <- 0.3

# seal learning parameters
alpha_fish <- 1
alpha_hunt <- 2
x_max <- 20
baseline <- 0.1
steepness <- 0.2
threshold <- 10

# fishing
gillnetters <- 6
fish_start <- 253
fish_end <- 321
catch_rate <- 0.3

## Set Up Variables
salmon_escape <- array(dim = c(days, years),
                       data = rep(0, days * years))
dimnames(salmon_escape) <- list(Day = 1:days, Year = 1:years)
gauntlet_salmon <- array(dim = c(days, years),
                         data = rep(0, days * years))
dimnames(gauntlet_salmon) <- list(Day = 1:days, Year = 1:years)
salmon_consumed <- array(dim = c(num_seals, days, years), 
                         data = rep(0, num_seals * days * years))
dimnames(salmon_consumed) <- list(Seal = 1:num_seals, Day = 1:days, 
                                  Year = 1:years)
seal_prob_gauntlet <- array(dim = c(num_seals, days, years), 
                            data = rep(seal_initial_prob_gauntlet, 
                                       num_seals * days * years))
dimnames(seal_prob_gauntlet) <- list(Seal = 1:num_seals, Day = 1:days, 
                                     Year = 1:years)
seal_forage_loc <- array(dim = c(num_seals, days, years), 
                         data = rep(seal_start_loc, 
                                    num_seals * days * years))
dimnames(seal_forage_loc) <- list(Seal = 1:num_seals, Day = 1:days, 
                                  Year = 1:years)

# Variables for x learning bit
x <- array(dim = c(seals, days, years), data = rep(0, seals * days * years))
C <- array(dim = c(seals, days, years), data = rep(0, seals * days * years))
B <- array(dim = c(seals, days, years), data = rep(0, seals * days * years))

#### Run time loop ####
for(y in 1:years) {
  
  # grab V's from end of last year
  # if(y>1) {
  # 
  # }
  
  for(t in 1:(days-1)) {
    
    # Calculate seal_prob_gauntlet
    for(seal in 1:num_seals) {
      seal_prob_gauntlet[seal, t, y] <- 1-(1/(1 + exp(-steepness * (threshold - x[seal, t, y]))))
    }
    
    # decide where each seal goes that day
    for(seal in 1:num_seals) {
      seal_forage_loc[seal,t,y] <- decide_foraging_destination(seal_prob_gauntlet[seal,t,y])
    }
    
    # round of copying
    seals_to_be_influenced <- which(seal_forage_loc[,t,y] == 0)
    for(seal in seals_to_be_influenced) {
      seal_forage_loc[seal,t,y] <-get_influenced(seal_forage_loc[,t,y], num_seals,
                                                 seal_num_neighbours_2_copy, seal_prob_2_copy)
    }
    
    # theoretical consumption 
    seals_at_gauntlet <- which(seal_forage_loc[,t,y] == 1)
    salmon_to_be_eaten <- 
      eat_some_fish(gauntlet_salmon[t,y], length(seals_at_gauntlet), seal_handling_time)
    if(salmon_to_be_eaten == 0) {
      predation_rate <- 0
    } else {
      predation_rate <- salmon_to_be_eaten/gauntlet_salmon[t, y]
    }
    
    # calculate salmon inst mortality
    predation <- salmon_to_be_eaten / (predation_rate + catch_rate) * 
      (1 - exp(-predation_rate - catch_rate))
    fish_catch <- gauntlet_salmon[t, y] * catch_rate / (predation_rate + catch_rate) * 
      (1 - exp(-predation_rate - catch_rate))
    
    # assign actual consumption
    salmon_consumed[seals_at_gauntlet, t, y] <- rep(round(predation/length(seals_at_gauntlet), 
                                                          digits = 0), length(seals_at_gauntlet))
    
    # consumption impacts salmon survival to next time step
    # salmon at the gauntlet on that day = arrive-leave
    salmon_arriving <- sum(salmon_arrive(day = (t+1))$avg)
    salmon_escape[t, y] <- gauntlet_salmon[t, y] * salmon_escape_rate(day = t)
    gauntlet_salmon[t+1, y] <- round(gauntlet_salmon[t, y] - sum(salmon_consumed[ , t, y]) - 
                                       salmon_escape[t, y] + salmon_arriving - fish_catch, digits = 0)
    
    # calculate x for next time step
    for(seal in 1:num_seals){
      C[seal, t, y] <- salmon_consumed[seal, t, y]/seal_satiation
      B[seal, t, y] <- 0
      delta <- alpha_fish * (C[seal, t, y] - baseline) - alpha_hunt * B[seal, t, y]
      x[seal,(t+1),y] <- x[seal, t, y] + delta
      if(x[seal, t+1, y]>xmax){
        x[seal,t+1] <- xmax
      }
    }
    
  } # days loop
} # years loop

