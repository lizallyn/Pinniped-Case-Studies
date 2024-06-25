## This will make no sense without "assembleTheLegos.R"
# Runs just the daily loop
# March 2024

for(t in 1:(days - 1)) {
  
  # salmon arrive at the gauntlet
  daily_update <- salmonSpeciesUpdate(day = t, chinook = gauntlet_chinook[t], 
                                      sockeye = gauntlet_sockeye[t], coho = gauntlet_coho[t], 
                                      data = Chosen_fish)
  gauntlet_chinook[t] <- daily_update$Chinook[1]
  gauntlet_sockeye[t] <- daily_update$Sockeye[1]
  gauntlet_coho[t] <- daily_update$Coho[1]
  gauntlet_salmon[t] <- sum(c(gauntlet_chinook[t], gauntlet_sockeye[t], gauntlet_coho[t]))
  
  # decide where each seal goes that day
  seal_forage_loc[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = decideForagingDestination)
  
  # round of copying
  if(num_seals_2_copy > 0){
    P_social[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = collusion, 
                           probs_list = seal_prob_gauntlet[,t], seals_2_copy = num_seals_2_copy, 
                           mean = mean, beta = beta)
    seal_forage_loc[,t] <- sapply(X = P_social[,t], FUN = decideForagingDestination)
  }
  
  # calculate salmon mortality 
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  seals_at_gauntlet_save[[t]] <- seals_at_gauntlet
  num_seals_at_gauntlet <- length(seals_at_gauntlet)
  salmon_result <- run_rungeKutta(Cmax = Cmax, Cmax_SL = Cmax, Nseal = num_seals_at_gauntlet, Nsealion = 0,
                                  alpha = alpha, alpha_SL = alpha, Ns = c(gauntlet_sockeye[t], gauntlet_chinook[t], gauntlet_coho[t]), 
                                  gamma = gamma, gamma_SL = gamma, Y = Y, Y_SL = Y,
                                  E = c(sockeye_escape_rate, chinook_escape_rate, coho_escape_rate), 
                                  F_catch = c(sockeye_catch_rate[t], chinook_catch_rate[t], coho_catch_rate[t]), 
                                  M = natural_mort, deltat = deltat_val)
  
  # assign escape and gauntlet updates
  escape_sockeye[t+1] <- escape_sockeye[t] + salmon_result["Sockeye", "E"]
  escape_chinook[t+1] <- escape_chinook[t] + salmon_result["Chinook", "E"]
  escape_coho[t+1] <- escape_coho[t] + salmon_result["Coho", "E"]
  
  eaten_sockeye[t] <- salmon_result["Sockeye", "C"]
  eaten_chinook[t] <- salmon_result["Chinook", "C"]
  eaten_coho[t] <- salmon_result["Coho", "C"]
  
  fished_sockeye[t] <- salmon_result["Sockeye", "Catch"]
  fished_chinook[t] <- salmon_result["Chinook", "Catch"]
  fished_coho[t] <- salmon_result["Coho", "Catch"]
  
  gauntlet_sockeye[t+1] <- salmon_result["Sockeye", "Ns"]
  gauntlet_chinook[t+1] <- salmon_result["Chinook", "Ns"]
  gauntlet_coho[t+1] <- salmon_result["Coho", "Ns"]
  
  # assign consumed salmon to seals at gauntlet
  consumed_total[t] <- sum(c(eaten_sockeye[t], eaten_chinook[t], eaten_coho[t]))
  if(num_seals_at_gauntlet == 0 | consumed_total[t] == 0) {
    salmon_consumed[,t] <- 0
  } else {
    salmon_consumed[seals_at_gauntlet, t] <- consumed_total[t]/num_seals_at_gauntlet
  }
  
  # seal harvest
  num_fishers <- sample(min_fishers:max_fishers, 1)
  H[t] <- getHarvested(day_plan = harvest_plan[t], list_gauntlet_seals = seals_at_gauntlet, 
                       num_fishers = num_fishers, zone_efficiency = zone_efficiency, 
                       efficiency = efficiency, steepness = steepness)
  
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
                    xmin = xmin, xmax = xmax, decay = decay, dead = seal %in% kill_list, 
                    baseline = baseline_x[seal])
      d_y <- learnY(hunting = H[t], y_t = y[seal, t], 
                    seal_forage_loc[seal, t], step = step, 
                    ymin = ymin, ymax = ymax, decay = decay, dead = seal %in% kill_list, baseline_y[seal])
      
      # update x and y and P_x and P_y
      x[seal, t+1] <- x[seal, t] + d_x
      if(seal %in% specialist_seals){
        P_x[seal, t+1] <- 1-(1/((1+buffer_Pxmin_specialist) + exp(-steepness * (threshold_x_specialist - x[seal, t+1]))))
      } else {P_x[seal, t+1] <- x[seal, t+1] * slope_x_val + intercept_x_val}
      
      y[seal, t+1] <- y[seal, t] + d_y
      P_y[seal, t+1] <- 1-(1/((1+buffer_Pymin[seal]) + exp(-steepness * (threshold[seal] - y[seal, t+1]))))
      
      seal_prob_gauntlet[seal, t+1] <- P_x[seal, t+1] * P_y[seal, t+1]
    } else {
      seal_prob_gauntlet[seal, t+1] <- NA
      seal_forage_loc[seal, t+1] <- NA
      x[seal, t+1] <- NA
      y[seal, t+1] <- NA
      C[seal, t] <- NA
      P_x[seal, t+1] <- NA
      P_y[seal, t+1] <- NA
    }
  }
  
} # days loop


