## Current working version of the gauntlets model
# Maybe add a fishing mortality piece
# Dec 2023

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

# salmon parameters
escape_rate <- 0.3

# seal learning parameters
salience <- 0.35

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

# Variables for learning bit from MS 2007, 2013
V_G <- array(dim = c(num_seals, days, years), 
             data = rep(0.1, num_seals * days * years)) # geography of gauntlet
dimnames(V_G) <- list(Seal = 1:num_seals, Day = 1:days, 
                                  Year = 1:years)
V_W <- array(dim = c(num_seals, days, years), 
             data = rep(0.1, num_seals * days * years)) # geography of open water
dimnames(V_W) <- list(Seal = 1:num_seals, Day = 1:days, 
                      Year = 1:years)
V_F <- array(dim = c(num_seals, days, years), 
             data = rep(0, num_seals * days * years)) # presence of salmon
dimnames(V_F) <- list(Seal = 1:num_seals, Day = 1:days, 
                      Year = 1:years)
V_gauntlet <- array(dim = c(num_seals, days, years))
dimnames(V_gauntlet) <- list(Seal = 1:num_seals, Day = 1:days, 
                      Year = 1:years)
V_gauntlet[,1,1] <- 0.1
V_open <- array(dim = c(num_seals, days, years))
dimnames(V_open) <- list(Seal = 1:num_seals, Day = 1:days, 
                      Year = 1:years)
V_open[,1,1] <- 0.1

P_gauntlet <- array(dim = c(num_seals, days, years))
dimnames(P_gauntlet) <- list(Seal = 1:num_seals, Day = 1:days, 
                      Year = 1:years)
P_gauntlet[,1,1] <- 0.5
P_open <- array(dim = c(num_seals, days, years))
dimnames(P_open) <- list(Seal = 1:num_seals, Day = 1:days, 
                             Year = 1:years)
P_open[,1,1] <- 0.5

#### Run time loop ####
for(y in 1:years) {
  
  # grab V's from end of last year
  if(y>1) {
    V_G[,1,y] <- V_G[,365,y-1]
    V_W[,1,y] <- V_W[,365,y-1]
    V_F[,1,y] <- V_F[,365,y-1]
  }
  
  for(t in 1:(days-1)) {
    
    # Calculate seal_prob_gauntlet
    for(seal in 1:num_seals) {
      V_gauntlet[seal, t, y] <- V_G[seal, t, y] + V_F[seal, t, y]
      V_open[seal, t, y] <- V_W[seal, t, y]
      P_gauntlet[seal, t, y] <- V_gauntlet[seal, t, y]/(V_gauntlet[seal, t, y] + V_open[seal, t, y])
      P_open[seal, t, y] <- V_open[seal, t, y]/(V_gauntlet[seal, t, y] + V_open[seal, t, y])
      # correct P_Ls to 0 or 1 if out of range (MS 2013)
      if(P_gauntlet[seal, t, y] > 1) {
        P_gauntlet[seal, t, y] <- 1
      } else {
        if(P_gauntlet[seal, t, y] < 0) {
          P_gauntlet[seal, t, y] <- 0
        }
      }
      if(P_open[seal, t, y] > 1) {
        P_open[seal, t, y] <- 1
      } else {
        if(P_open[seal, t, y] < 0) {
          P_open[seal, t, y] <- 0
        }
      }
      # assign P_gauntlet to seal_prob_gauntlet
      seal_prob_gauntlet[seal, t, y] <- P_gauntlet[seal, t, y]
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
    
    # calculate delta Vs for next time step
    for(seal in 1:num_seals){
      if(salmon_consumed[seal, t, y] == 0){ #V_F not presented so no change, open water rewarded
        lambda_g <- 0
        lambda_o <- 1
        V_G[seal, t+1, y] <- V_G[seal, t, y] + salience * 
          (lambda_g - (V_G[seal, t, y])) * P_gauntlet[seal, t, y]
        V_W[seal, t+1, y] <- V_W[seal, t, y] + salience * 
          (lambda_o - (V_W[seal, t, y])) * P_open[seal, t, y]
        V_F[seal, t+1, y] <- V_F[seal, t, y]
      } else { # gauntlet rewarded and V_F presented
        lambda_g <- 1
        lambda_o <- 0
        V_G[seal, t+1, y] <- V_G[seal, t, y] + salience * 
          (lambda_g - (V_G[seal, t, y] + V_F[seal, t, y])) * P_gauntlet[seal, t, y]
        V_W[seal, t+1, y] <- V_W[seal, t, y] + salience * 
          (lambda_o - (V_W[seal, t, y])) * P_open[seal, t, y]
        V_F[seal, t+1, y] <- V_F[seal, t, y] + salience * 
          (lambda_g - (V_G[seal, t, y] + V_F[seal, t, y])) * P_gauntlet[seal, t, y]
      }
    }
    
  } # days loop
} # years loop


# Testing Space




#### Visualize ####
# number of seals at the gauntlet per day - multiple years
num_seals_at_gauntlet_day_year <- data.frame(cbind(1:days, colSums(seal_forage_loc)))
colnames(num_seals_at_gauntlet_day_year) <- c("Day", 1:years)
num_seals_at_gauntlet_day_year_long <- num_seals_at_gauntlet_day_year %>%
  pivot_longer(!Day, names_to = "Year", values_to = "Num_Seals")
plot_seals_at_gauntlet <- 
  ggplot(data = num_seals_at_gauntlet_day_year_long, aes(x = Day)) +
  geom_point(aes(y = Num_Seals, group = Year, color = Year)) + 
  labs(title = "seals at gauntlet")
plot_seals_at_gauntlet

# These only show the last year
par(mfrow = c(2,2))
plot(1:days, colSums(seal_forage_loc[,,y]), main = "Number of seals at the gauntlet")
plot(1:days, colMeans(seal_prob_gauntlet[,,y]), main = "avg. prob gauntlet")
plot(1:days, gauntlet_salmon[,y], main = "salmon at the gauntlet")
plot(1:days, colSums(salmon_consumed[,,y]), main = "salmon consumed")

