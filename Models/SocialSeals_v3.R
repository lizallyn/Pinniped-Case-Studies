### Making it run for multiple years

#### Set Up ####

# Load Packages and Data
library(gdata) #resample in feeding
library(ggplot2)
library(tidyr) #formatting for visualization

# Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Sockeye%20arrival%20function%20creation.R")

# Set Parameters
years <- 1
days <- 365
num_seals <- 20
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0
seal_num_neighbours_2_copy <- 2
seal_prob_2_copy <- 0.5
satiation_threshold <- 5

escape_rate <- 0.3

seal_handling_time <- 0.1

salience <- 0.15 # seems to be standard for this model?
learning_rate <- 1 # standard bc no data I guess?
V_g <- 0.1


# Set Up Variables
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
V_g <- array(dim = c(num_seals, days, years), 
                         data = rep(0.1, 
                                    num_seals * days * years))
dimnames(V_g) <- list(Seal = 1:num_seals, Day = 1:days, 
                                  Year = 1:years)

#### Run time loop ####
for(y in 1:years) {
  for(t in 2:(days-1)) {
    
    # salmon at the gauntlet on that day = arrive-leave
    salmon_arrive <- round(predict.fish(day = t, params = fish.fit.optim$par, start.day = 163), digits = 0)
    gauntlet_salmon[t,y] <- round(gauntlet_salmon[t-1, y] + salmon_arrive - salmon_escape[t-1, y], digits = 0)
    
    
    # decide where each seal goes that day
    for(seal in 1:num_seals) {
      if(runif(1, 0, 1) < seal_prob_gauntlet[seal,t, y]) {
        seal_forage_loc[seal,t, y] <- 1
      } else seal_forage_loc[seal,t, y] <- 0
    }
    
    # round of copying
    for(seal in 1:num_seals) {
      # if the seal wasn't going to the gauntlet
      if(seal_forage_loc[seal,t, y] == 0) {
        loc_of_seals_2_copy <- seal_forage_loc[sample(1:num_seals, seal_num_neighbours_2_copy, replace = F),t, y]
        social_information <- mean(loc_of_seals_2_copy)
        if(runif(1, 0, 1) < (seal_prob_2_copy * social_information)) {
          seal_forage_loc[seal,t, y] <- 1
        }
      }
    }
    
    # feeding as random provisioning
    # seals_at_gauntlet <- which(seal_forage_loc[,t, y] == 1)
    # if(length(seals_at_gauntlet) < 1 | gauntlet_salmon[t, y] < 1) {
    #   salmon_consumed[,t, y] <- 0
    # } else {
    #   for(fish in 1:gauntlet_salmon[t, y]) {
    #     seal_eating <- gdata::resample(seals_at_gauntlet, 1)
    #     if(salmon_consumed[seal_eating, t, y] == satiation_threshold) {
    #       next
    #     } else {
    #       salmon_consumed[seal_eating, t, y] <- salmon_consumed[seal_eating, t, y] + 1
    #     }
    #   }
    # }
    
    # consumption via equation from Andrew
    # calculate num salmon to be eaten in that time step
    seals_at_gauntlet <- which(seal_forage_loc[,t, y] == 1)
    salmon_to_be_eaten <- gauntlet_salmon[t, y] * (length(seals_at_gauntlet) / 
      (1 + length(seals_at_gauntlet) + seal_handling_time * gauntlet_salmon[t, y]))
    salmon_per_seal <- salmon_to_be_eaten / length(seals_at_gauntlet)
    for(seal in length(seals_at_gauntlet)) {
      salmon_consumed[seal, t, y] <- rpois(1, salmon_per_seal)
    }
    
    # consumption impacts salmon survival
    gauntlet_salmon[t, y] <- gauntlet_salmon[t, y] - sum(salmon_consumed[ , t, y])
    salmon_escape[t, y] <- gauntlet_salmon[t, y] * escape_rate
    
    # seal foraging success impacts prob gauntlet on next time step
    for(seal in seals_at_gauntlet) {
      if(salmon_consumed[seal, t, y] >= satiation_threshold) {
        seal_prob_gauntlet[seal, t+1, y] <- 1
      } else {
        seal_prob_gauntlet[seal, t+1, y] <- (salmon_consumed[seal, t, y]/5) + 
          (salmon_consumed[seal,t-1, y]/10)
      }
    }
    
    # Trying a simple Rescorla-Wagner element
    # just geography of the gauntlet
    for(seal in seals_at_gauntlet) {
      if(salmon_consumed[seal, t, y] > 1) {
        lambda <- 1
      } else {lambda <- 0}
      V_g[seal, t+1, y] <- V_g[seal, t, y] + salience*(lambda - V_g[seal, t, y])
    }
    
  }# days loop
}# years loop

# Testing Space

# trying to explore reasonable handling time values
# needs to be very small for 100% consumption to be possible by less than hundreds of seals
# plot(1:500, 1:500 /
#        (1 + 1:500 + seal_handling_time * 1))


#### Visualize ####
# number of seals at the gauntlet per day
num_seals_at_gauntlet_day_year <- data.frame(cbind(1:days, colSums(seal_forage_loc)))
colnames(num_seals_at_gauntlet_day_year) <- c("Day", 1:years)
num_seals_at_gauntlet_day_year_long <- num_seals_at_gauntlet_day_year %>%
  pivot_longer(!Day, names_to = "Year", values_to = "Num_Seals")

plot_seals_at_gauntlet <- ggplot(data = num_seals_at_gauntlet_day_year_long, aes(x = Day)) +
  geom_point(aes(y = Num_Seals, group = Year, color = Year))
plot_seals_at_gauntlet

plot(1:days, colSums(seal_forage_loc[,,1])) # looks like almost no response to salmon
# salmon at the gauntlet per day
plot(1:days, gauntlet_salmon[,1]) # looks almost right, some negative
# successful foraging seals per day at the gauntlet
plot(1:days, colSums(salmon_consumed[,,1]))

