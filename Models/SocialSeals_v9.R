# Social Seals version 9
# For separating out the salmon species
# February/March 2024

# Set Up ----

## Load Packages and Data

## Load Data Files
source("Functions/Prep_data_for_Salmon_functions.R")
source("Functions/Prep_data_for_Harvest_functions.R")

## Load Function Files
source("Functions/makeArray.R")
source("Functions/collusion.R")
source("Functions/salmonSpeciesUpdate.R")
source("Functions/createHarvestPlan.R")
source("Functions/decideForagingDestination.R")
source("Functions/learnX.R")
source("Functions/learnY.R")
source("Functions/rungeKutta.R")
source("Functions/getHarvested.R")

## Load Set Up Files
source("Functions/BaseRun_set_pars.R")
source("Functions/BaseRun_initialize_variables.R")

# source("Functions/set_pars.R")
# source("Functions/initialize_variables.R")


# Run Loop ----
  
for(t in 1:(days-1)) {
  
  # salmon arrive at the gauntlet
  daily_update <- salmonSpeciesUpdate(day = t, data = Daily_fish)
  gauntlet_chinook[t] <- daily_update$Chinook[1]
  gauntlet_sockeye[t] <- daily_update$Sockeye[1]
  gauntlet_coho[t] <- daily_update$Coho[1]
  gauntlet_salmon[t] <- sum(c(gauntlet_chinook[t], gauntlet_sockeye[t], gauntlet_coho[t]))
  
  # decide where each seal goes that day
  for(seal in 1:num_seals) {
    seal_forage_loc[seal,t] <- decideForagingDestination(seal_prob_gauntlet[seal,t])
  }
  
  # round of copying
  for(seal in 1:num_seals) {
    if(num_seals_2_copy > 0){
      P_social[seal, t] <- collusion(probs_list = seal_prob_gauntlet[,t], 
                                     prob_gauntlet_of_seal = seal_prob_gauntlet[seal, t], 
                                     seals_2_copy = num_seals_2_copy, 
                                     mean = mean, beta = beta)
      seal_forage_loc[seal,t] <- decideForagingDestination(P_social[seal,t])
    }
  }
  
  # calculate salmon mortality 
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  salmon_result <- run_rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                                  alpha = alpha, Ns = c(gauntlet_sockeye[t], gauntlet_chinook[t], gauntlet_coho[t]), 
                                  gamma = gamma, Y = Y, E = c(sockeye_escape_rate, chinook_escape_rate, coho_escape_rate), 
                                  F_catch = c(sockeye_catch_rate[t], chinook_catch_rate[t], coho_catch_rate[t]), M = natural_mort, deltat = 1/24)

  # assign escape and gauntlet updates
  escape_sockeye[t+1] <- escape_sockeye[t] + salmon_result["Sockeye", "E"]
  escape_chinook[t+1] <- escape_chinook[t] + salmon_result["Chinook", "E"]
  escape_coho[t+1] <- escape_coho[t] + salmon_result["Coho", "E"]
  
  eaten_sockeye[t] <- salmon_result["Sockeye", "C"]
  eaten_chinook[t] <- salmon_result["Chinook", "C"]
  eaten_coho[t] <- salmon_result["Coho", "C"]
  
  gauntlet_sockeye[t+1] <- salmon_result["Sockeye", "Ns"]
  gauntlet_chinook[t+1] <- salmon_result["Chinook", "Ns"]
  gauntlet_coho[t+1] <- salmon_result["Coho", "Ns"]
  
  # check it's ok
  if(sum(salmon_result["Sockeye",c("C", "Catch", "E")]) > gauntlet_sockeye[t]) {
    screwy <- rbind(screwy, c(species = "Sockeye", day = t, gauntlet_t = gauntlet_sockeye[t], salmon_result["Sockeye",]))
    print(paste("Sockeye day", t, "check screwy!!!"))
  } 
  if(sum(salmon_result["Chinook",c("C", "Catch", "E")]) > gauntlet_chinook[t]) {
    screwy <- rbind(screwy, c(species = "Chinook", day = t, gauntlet_t = gauntlet_chinook[t], salmon_result["Chinook",]))
    print(paste("Chinook day", t, "check screwy!!!"))
    # 
  } 
  if(sum(salmon_result["Coho",c("C", "Catch", "E")]) > gauntlet_coho[t]) {
    screwy <- rbind(screwy, c(species = "Coho", day = t, gauntlet_t = gauntlet_coho[t], salmon_result["Coho",]))
    print(paste("Coho day", t, "check screwy!!!"))
    # 
  }
  
  # assign consumed salmon to seals at gauntlet
  consumed_total[t] <- sum(c(eaten_sockeye[t], eaten_chinook[t], eaten_coho[t]))
  if(length(seals_at_gauntlet) == 0 | consumed_total[t] == 0) {
    salmon_consumed[,t] <- 0
  } else {
    salmon_consumed[seals_at_gauntlet, t] <- consumed_total[t]/length(seals_at_gauntlet)
  }

  # seal harvest
  num_fishers <- sample(min_fishers:max_fishers, 1)
  H[t] <- getHarvested(day_plan = harvest_plan[t], list_gauntlet_seals = seals_at_gauntlet, num_fishers = num_fishers,
                        zone_efficiency = zone_efficiency, efficiency = efficiency, steepness = steepness)
  
  if(H[t] > 0){
    killed <- sample(seals_at_gauntlet, H[t])
    kill_list <- c(kill_list, killed)
  }
  
  
  # calculate x, y and prob_gauntlet for next time step
  ## This could all become some functions
  for(seal in 1:num_seals){
    if(!(seal %in% kill_list)){
      C[seal, t] <- salmon_consumed[seal, t] - w
      
      # calculate d_x and d_y
      d_x <- learnX(food = C[seal, t], x_t = x[seal, t], 
                    forage_loc = seal_forage_loc[seal, t],  step = step, 
                    xmin = xmin, xmax = xmax, decay = decay, dead = seal %in% kill_list)
      d_y <- learnY(hunting = H[t], y_t = y[seal, t], 
                    seal_forage_loc[seal, t], step = step, 
                    ymin = ymin, ymax = ymax, decay = decay, dead = seal %in% kill_list)
      
      # update x and y and P_x and P_y
      x[seal, t+1] <- x[seal, t] + d_x
      P_x[seal, t+1] <- x[seal, t+1] * slope_x + intercept_x
      
      y[seal, t+1] <- y[seal, t] + d_y
      P_y[seal, t+1] <- 1-(1/((1+buffer_Pymin) + exp(-steepness * (threshold - y[seal, t+1]))))
      
      seal_prob_gauntlet[seal, t+1] <- P_x[seal, t+1] * P_y[seal, t+1]
    } else {
      seal_prob_gauntlet[seal, t+1] <- 0
      seal_forage_loc[seal, t+1] <- NA
      x[seal, t+1] <- NA
      y[seal, t+1] <- NA
      C[seal, t] <- NA
      P_x[seal, t+1] <- NA
      P_y[seal, t+1] <- NA
    }
  }
  
} # days loop


# Testing Space ----




# Summary Plots ----

## Quick and Dirty ----
par(mfrow = c(2, 2))
plot(1:days, colSums(seal_forage_loc), main = "Number of seals at the gauntlet")
plot(1:days, colMeans(seal_prob_gauntlet), main = "avg. prob gauntlet")
plot(1:days, gauntlet_salmon, main = "salmon at the gauntlet")
plot(1:days, colSums(salmon_consumed), main = "salmon consumed")


plot(1:days, colMeans(C, na.rm = T))
plot(1:days, colMeans(P_x, na.rm = T))
H.plot <- data.frame(cbind(1:days, H))
plot(H.plot)
plot(1:days, colMeans(P_y, na.rm = T))

## Slightly Nicer ----

source("Functions/Plots.R")

gauntlet_plot / eaten_sp_plot / escape_plot + plot_layout(guides = "collect")

eaten_sp_plot/plot_eaten/plot_C/plot_x/plot_Px + plot_layout(guides = "collect")

plot_seals/plot_H/plot_y/plot_Py + plot_layout(guides = "collect")

plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")
