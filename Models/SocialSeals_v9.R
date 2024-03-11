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
source("Functions/get_dXdt.R")
source("Functions/rungeKutta.R")
source("Functions/getHarvested.R")

## Load Set Up Files
source("Functions/set_pars.R")
source("Functions/initialize_variables.R")

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
  sockeye_result <- run_rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_sockeye[t], 
                               gamma = gamma, Y = Y, E = sockeye_escape_rate, 
                               F_catch = sockeye_catch_rate[t], M = natural_mort, deltat = 1)
  chinook_result <- run_rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_chinook[t], 
                               gamma = gamma, Y = Y, E = chinook_escape_rate, 
                               F_catch = chinook_catch_rate[t], M = natural_mort, deltat = 1)
  coho_result <- run_rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_coho[t], 
                               gamma = gamma, Y = Y, E = coho_escape_rate, 
                               F_catch = coho_catch_rate[t], M = natural_mort, deltat = 1)
  if(any(c(sockeye_result, sum(sockeye_result[2:4])) > gauntlet_sockeye[t])) {
    screwy <- rbind(screwy, c(species = "Sockeye", day = t, gauntlet_t = gauntlet_sockeye[t],sockeye_result))
    print(paste("day", t, "check screwy!!!"))
    escape_sockeye[t+1] <- escape_sockeye[t] + gauntlet_sockeye[t] * sockeye_escape_rate
    gauntlet_sockeye[t+1] <- gauntlet_sockeye[t] - gauntlet_sockeye[t] * sockeye_escape_rate
  } else {
    gauntlet_sockeye[t+1] <- sockeye_result["Ns"]
    escape_sockeye[t+1] <- escape_sockeye[t] + sockeye_result["E"]
    }
  if(any(c(chinook_result, sum(chinook_result[2:4])) > gauntlet_chinook[t])) {
    screwy <- rbind(screwy, c(species = "Chinook", day = t, gauntlet_t = gauntlet_chinook[t],chinook_result))
    print(paste("day", t, "check screwy!!!"))
    escape_chinook[t+1] <- escape_chinook[t] + gauntlet_chinook[t] * chinook_escape_rate
    gauntlet_chinook[t+1] <- gauntlet_chinook[t] - gauntlet_chinook[t] * chinook_escape_rate
    
  } else {
    gauntlet_chinook[t+1] <- chinook_result["Ns"]
    escape_chinook[t+1] <- escape_chinook[t] + chinook_result["E"]
    }
  if(any(c(coho_result, sum(coho_result[2:4])) > gauntlet_coho[t])) {
    screwy <- rbind(screwy, c(species = "Coho", day = t, gauntlet_t = gauntlet_coho[t], coho_result))
    print(paste("day", t, "check screwy!!!"))
    escape_coho[t+1] <- escape_coho[t] + gauntlet_coho[t] * coho_escape_rate
    gauntlet_coho[t+1] <- gauntlet_coho[t] - gauntlet_coho[t] * coho_escape_rate
    
  } else {
    gauntlet_coho[t+1] <- coho_result["Ns"]
    escape_coho[t+1] <- escape_coho[t] + coho_result["E"]
    }
  
  # assign consumed salmon to seals at gauntlet
  consumed_sum <- c(sockeye_result["C"], chinook_result["C"], coho_result["C"])
  consumed_sum[which(consumed_sum < 0)] <- 0
  salmon_consumed[seals_at_gauntlet, t] <- sum(consumed_sum)/length(seals_at_gauntlet)
  # if(any(salmon_consumed[,t] > 100)){
  #   print(c(consumed_sum, t))
  #   print(salmon_consumed[,t])
  #   print(gauntlet_chinook[t])
  #   print(chinook_result)
  #   print(gauntlet_sockeye[t])
  #   print(sockeye_result)
  #   print(seals_at_gauntlet)
  # } # for troubleshooting

  # seal harvest
  H[t] <- getHarvested(day_plan = harvest_plan[t], num_gauntlet_seals = length(seals_at_gauntlet), 
                        zone_efficiency = zone_efficiency, Hmax = harvest_max_perboat, 
                        processing = processing_time, min_fishers = min_fishers, max_fishers = max_fishers, 
                        gamma = gamma_H, Y = Y_H)
  if(H[t] > 0){
    seal_prob_gauntlet[sample(seals_at_gauntlet, H[t]), t+1] <- NA
  }
  
  # calculate x, y and prob_gauntlet for next time step
  ## This could all become some functions
  for(seal in 1:num_seals){
    # calculate C
    C[seal, t] <- salmon_consumed[seal, t] - w
    
    # calculate d_x and d_y
    d_x <- learnX(food = C[seal, t], x_t = x[seal, t], 
                   forage_loc = seal_forage_loc[seal, t],  step = step, 
                   xmin = xmin, xmax = xmax, decay = decay)
    d_y <- learnY(hunting = H[t], y_t = y[seal, t], 
                   seal_forage_loc[seal, t], step = step, 
                   ymin = ymin, ymax = ymax, decay = decay)
    
    # update x and y and P_x and P_y
    x[seal, t+1] <- x[seal, t] + d_x
    P_x[seal, t+1] <- x[seal, t+1] * slope_x + intercept_x
    
    y[seal, t+1] <- y[seal, t] + d_y
    P_y[seal, t+1] <- 1-(1/((1+buffer_Pymin) + exp(-steepness * (threshold - y[seal, t+1]))))
    
    # calculate Prob gauntlet
    seal_prob_gauntlet[seal, t+1] <- P_x[seal, t+1] * P_y[seal, t+1]
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


plot(1:days, colMeans(C))
plot(1:days, colMeans(P_x))
H.plot <- data.frame(cbind(1:days, H))
plot(H.plot)
plot(1:days, colMeans(P_y))

## Slightly Nicer ----

