## adding the learning piece from Rescorla Wagner/MS

#### Set Up ####

# Load Packages and Data
# library(gdata) #resample in feeding
library(ggplot2)
library(tidyr) #formatting for visualization

# Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Sockeye%20arrival%20function%20creation.R")

# Set Parameters
years <- 1
days <- 247
num_seals <- 5
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0
seal_num_neighbours_2_copy <- 2
seal_prob_2_copy <- 0.5
# satiation_threshold <- 5

escape_rate <- 0.3

seal_handling_time <- 0.1

salience <- 0.35

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

V_G <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years)) # geography of gauntlet
V_W <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years)) # geography of open water
V_F <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years)) # presence of salmon
V_B <- array(dim = c(num_seals, days, years), data = rep(0.1, num_seals * days * years)) # some element(s) in common between both

V_gauntlet <- array(dim = c(num_seals, days, years))
V_gauntlet[,1,1] <- 0.1
V_open <- array(dim = c(num_seals, days, years))
V_open[,1,1] <- 0.1

P_gauntlet <- array(dim = c(num_seals, days, years))
P_gauntlet[,1,1] <- 0.5
P_open <- array(dim = c(num_seals, days, years))
P_open[,1,1] <- 0.5

#### Run time loop ####
for(y in 1:years) {
  for(t in 2:(days-1)) {
    
    # salmon at the gauntlet on that day = arrive-leave
    salmon_arrive <- round(predict.fish(day = t, params = fish.fit.optim$par, start.day = 163), digits = 0)
    gauntlet_salmon[t,y] <- round(gauntlet_salmon[t-1, y] + salmon_arrive - salmon_escape[t-1, y], digits = 0)
    
    # Calculate seal_prob_gauntlet
    for(seal in 1:num_seals) {
      V_gauntlet[seal, t, y] <- V_G[seal, t, y] + V_F[seal, t, y] + V_B[seal, t, y]
      V_open[seal, t, y] <- V_W[seal, t, y] + V_B[seal, t, y]
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
      if(runif(1, 0, 1) < seal_prob_gauntlet[seal,t, y]) {
        seal_forage_loc[seal,t, y] <- 1
      } else seal_forage_loc[seal,t, y] <- 0
    }
    
    # round of copying
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
    if(gauntlet_salmon[t, y] < 1) {
      salmon_to_be_eaten <- 0
    } else {
      salmon_to_be_eaten <- gauntlet_salmon[t, y] * 
        (length(seals_at_gauntlet) / (1 + length(seals_at_gauntlet) + 
                                        seal_handling_time * gauntlet_salmon[t, y]))
    }
    salmon_per_seal <- round(salmon_to_be_eaten / length(seals_at_gauntlet))
    # assign the salmon to the seals at the gauntlet
    for(seal in length(seals_at_gauntlet)) {
      while(salmon_to_be_eaten > salmon_per_seal) {
        salmon_consumed[seal, t, y] <- rpois(1, salmon_per_seal)
        salmon_to_be_eaten <- salmon_to_be_eaten - salmon_consumed[seal, t, y]
      }
    }
    
    # consumption impacts salmon survival
    gauntlet_salmon[t, y] <- gauntlet_salmon[t, y] - sum(salmon_consumed[ , t, y])
    salmon_escape[t, y] <- gauntlet_salmon[t, y] * escape_rate
    
    # calculate delta Vs for next time step
    for(seal in 1:num_seals){
      if(salmon_consumed[seal, t, y] == 0){ #V_F not presented so no change, open water rewarded
        lambda_g <- 0
        lambda_o <- 0.5
        V_G[seal, t+1, y] <- V_G[seal, t, y] + salience * (lambda_g - (V_G[seal, t, y] + V_B[seal, t, y])) * P_gauntlet[seal, t, y]
        V_W[seal, t+1, y] <- V_W[seal, t, y] + salience * (lambda_o - (V_W[seal, t, y] + V_B[seal, t, y])) * P_open[seal, t, y]
        V_F[seal, t+1, y] <- V_F[seal, t, y]
        V_B[seal, t+1, y] <- V_B[seal, t, y] + 
          (salience * (lambda_o - (V_B[seal, t, y] + V_W[seal, t, y])) * P_open[seal, t, y]) + 
          (salience * (lambda_g - (V_B[seal, t, y] + V_G[seal, t, y])) * P_gauntlet[seal, t, y])
      } else { # gauntlet rewarded and V_F presented
        lambda_g <- 1
        lambda_o <- 0
        V_G[seal, t+1, y] <- V_G[seal, t, y] + salience * (lambda_g - (V_G[seal, t, y] + V_F[seal, t, y] + V_B[seal, t, y])) * P_gauntlet[seal, t, y]
        V_W[seal, t+1, y] <- V_W[seal, t, y] + salience * (lambda_o - (V_W[seal, t, y] + V_B[seal, t, y])) * P_open[seal, t, y]
        V_F[seal, t+1, y] <- V_F[seal, t, y] + salience * (lambda_g - (V_G[seal, t, y] + V_F[seal, t, y] + V_B[seal, t, y])) * P_gauntlet[seal, t, y]
        V_B[seal, t+1, y] <- V_B[seal, t, y] + 
          (salience * (lambda_o - (V_B[seal, t, y] + V_W[seal, t, y])) * P_open[seal, t, y]) + 
          (salience * (lambda_g - (V_B[seal, t, y] + V_F[seal, t, y] + V_G[seal, t, y])) * P_gauntlet[seal, t, y])
      }
    }
    
    
    
  }# days loop
}# years loop

# Testing Space



#### Visualize ####
# number of seals at the gauntlet per day
# num_seals_at_gauntlet_day_year <- data.frame(cbind(1:days, colSums(seal_forage_loc)))
# colnames(num_seals_at_gauntlet_day_year) <- c("Day", 1:years)
# num_seals_at_gauntlet_day_year_long <- num_seals_at_gauntlet_day_year %>%
#   pivot_longer(!Day, names_to = "Year", values_to = "Num_Seals")
# 
# plot_seals_at_gauntlet <- ggplot(data = num_seals_at_gauntlet_day_year_long, aes(x = Day)) +
#   geom_point(aes(y = Num_Seals, group = Year, color = Year))
# plot_seals_at_gauntlet

# If only one year, can use these
plot(1:days, colSums(seal_forage_loc[,,1]), main = "Number of seals at the gauntlet")
plot(1:days, colMeans(seal_prob_gauntlet[,,1]))
plot(1:days, gauntlet_salmon[,1], main = "salmon at the gauntlet")
plot(1:days, colSums(salmon_consumed[,,1]), main = "salmon consumed")

