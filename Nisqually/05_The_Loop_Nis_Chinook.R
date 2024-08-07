## The model loop for Nisqually Chinook
# August 2024

for(t in 1:(days - 1)) {
  
  # salmon arrive at the gauntlet
  daily_update <- salmonSpeciesUpdate(day = t, gr_chinook = gauntlet_gr[t], ln_chinook = gauntlet_ln[t],
                                      data = Daily_Chinook)
  gauntlet_gr[t] <- daily_update$Chinook_GR[1]
  gauntlet_ln[t] <- daily_update$Chinook_LN[1]
  gauntlet_salmon[t] <- sum(c(gauntlet_gr[t], gauntlet_ln[t]))
  
  # decide where each seal goes that day
  seal_forage_loc[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = decideForagingDestination)
  zc_forage_loc[,t] <- sapply(X = zc_prob_gauntlet[,t], FUN = decideForagingDestination)
  ej_forage_loc[,t] <- sapply(X = ej_prob_gauntlet[,t], FUN = decideForagingDestination)
  
  # round of copying
  # seals
  if(num_seals_2_copy > 0){
    P_social[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = collusion, 
                           probs_list = seal_prob_gauntlet[,t], seals_2_copy = num_seals_2_copy, 
                           mean = mean, beta = beta)
    seal_forage_loc[,t] <- sapply(X = P_social[,t], FUN = decideForagingDestination)
  }
  # Zc
  if(num_zc_2_copy > 0){
    P_social_zc[,t] <- sapply(X = zc_prob_gauntlet[,t], FUN = collusion, 
                           probs_list = zc_prob_gauntlet[,t], seals_2_copy = num_zc_2_copy, 
                           mean = mean, beta = beta)
    zc_forage_loc[,t] <- sapply(X = P_social_zc[,t], FUN = decideForagingDestination)
  }
  #Ej
  if(num_ej_2_copy > 0){
    P_social_ej[,t] <- sapply(X = ej_prob_gauntlet[,t], FUN = collusion, 
                              probs_list = ej_prob_gauntlet[,t], seals_2_copy = num_ej_2_copy, 
                              mean = mean, beta = beta)
    ej_forage_loc[,t] <- sapply(X = P_social_ej[,t], FUN = decideForagingDestination)
  }
  
  # calculate salmon mortality 
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  zc_at_gauntlet <- which(zc_forage_loc[,t] == 1)
  ej_at_gauntlet <- which(ej_forage_loc[,t] == 1)
  
  seals_at_gauntlet_save[[t]] <- seals_at_gauntlet
  zc_at_gauntlet_save[[t]] <- zc_at_gauntlet
  ej_at_gauntlet_save[[t]] <- ej_at_gauntlet
  
  num_seals_at_gauntlet <- length(seals_at_gauntlet)
  num_zc_at_gauntlet <- length(zc_at_gauntlet)
  num_ej_at_gauntlet <- length(ej_at_gauntlet)
  
  salmon_result <- run_rungeKutta(Ns = c(gauntlet_gr[t], gauntlet_ln[t]),
                                  species_list = c("Chinook_GR", "Chinook_LN"), Cmax = Cmax, 
                                  Nseal = num_seals_at_gauntlet, alpha = alpha, gamma = gamma, Y = Y,
                                  NSSL = num_ej_at_gauntlet, NCSL = num_zc_at_gauntlet, Cmax_SSL = Cmax_ej, 
                                  alpha_SSL = alpha, gamma_SSL = gamma, Y_SSL = Y, Cmax_CSL = Cmax_zc, 
                                  alpha_CSL = alpha, gamma_CSL = gamma, Y_CSL = Y,
                                  F_catch = c(gr_catch_rate[t], ln_catch_rate[t]), 
                                  M = natural_mort, E = c(gr_escape_rate, ln_escape_rate), 
                                  deltat = deltat_val)
  
  # assign escape and gauntlet updates
  escape_gr[t+1] <- escape_gr[t] + salmon_result["Chinook_GR", "E"]
  escape_ln[t+1] <- escape_ln[t] + salmon_result["Chinook_LN", "E"]
  
  fished_gr[t] <- salmon_result["Chinook_GR", "Catch"]
  fished_ln[t] <- salmon_result["Chinook_LN", "Catch"]
  
  gauntlet_gr[t+1] <- salmon_result["Chinook_GR", "Ns"]
  gauntlet_ln[t+1] <- salmon_result["Chinook_LN", "Ns"]
  
  eaten_gr[t] <- sum(salmon_result["Chinook_GR", c("C", "C_CSL", "C_SSL")])
  eaten_ln[t] <- sum(salmon_result["Chinook_LN", c("C", "C_CSL", "C_SSL")])
  
  # assign consumed salmon to pinnipeds at gauntlet
  
  consumed_by_pv <- sum(salmon_result[,"C"])
  consumed_by_zc <- sum(salmon_result[,"C_CSL"])
  consumed_by_ej <- sum(salmon_result[,"C_SSL"])
  
  consumed_total[t] <- sum(c(consumed_by_pv, consumed_by_zc, consumed_by_ej))
  
  if(num_seals_at_gauntlet == 0 | consumed_by_pv == 0) {
    salmon_consumed_pv[,t] <- 0
  } else {
    salmon_consumed_pv[seals_at_gauntlet, t] <- consumed_by_pv/num_seals_at_gauntlet
  }
    
  if(num_zc_at_gauntlet == 0 | consumed_by_zc == 0){
    salmon_consumed_zc[,t] <- 0
  } else {
    salmon_consumed_zc[zc_at_gauntlet, t] <- consumed_by_zc/num_zc_at_gauntlet
  }
  
  if(num_ej_at_gauntlet == 0 | consumed_by_ej == 0){
    salmon_consumed_ej[,t] <- 0
  } else {
    salmon_consumed_ej[ej_at_gauntlet, t] <- consumed_by_ej/num_ej_at_gauntlet
  }
  
  # seal harvest
  num_harvesters <- sample(min_harvesters:max_harvesters, 1)
  H[t] <- getHarvested(day_plan = harvest_plan_pv[t], list_gauntlet_seals = seals_at_gauntlet, 
                       num_fishers = num_harvesters, zone_efficiency = zone_efficiency, 
                       efficiency = efficiency, steepness = steepness)
  H_ej[t] <- getHarvested(day_plan = harvest_plan_ej[t], list_gauntlet_seals = ej_at_gauntlet, 
                          num_fishers = num_harvesters, zone_efficiency = zone_efficiency, 
                          efficiency = efficiency, steepness = steepness)
  H_zc[t] <- getHarvested(day_plan = harvest_plan_zc[t], list_gauntlet_seals = zc_at_gauntlet, 
                          num_fishers = num_harvesters, zone_efficiency = zone_efficiency, 
                          efficiency = efficiency, steepness = steepness)
  
  
  if(H[t] > 0){
    killed <- sample(seals_at_gauntlet, H[t])
    kill_list <- c(kill_list, killed)
  }
  if(H_ej[t] > 0){
    killed <- sample(ej_at_gauntlet, H_ej[t])
    kill_list_ej <- c(kill_list_ej, killed)
  }
  if(H_zc[t] > 0){
    killed <- sample(zc_at_gauntlet, H_zc[t])
    kill_list_zc <- c(kill_list_zc, killed)
  }
  
  ## calculate x, y and prob_gauntlet for next time step
  
  # seals
  
  for(seal in 1:num_seals){
    bundle_y_shape_pars <- tibble(buffer = buffer_Pymin[seal],
           steepness = steepness, threshold = threshold[seal])

    update_output <- updateLearning(salmon_consumed = salmon_consumed_pv[seal, t], w = w, hunting = H[t],
                 x_t = x[seal, t], y_t = y[seal, t],
                 forage_loc = seal_forage_loc[seal, t], bundle_dx_pars = bundle_dx_pars,
                 bundle_dy_pars = bundle_dy_pars, dead = seal %in% kill_list,
                 baseline_x = baseline_x[seal], baseline_y = baseline_y[seal],
                 specialist = seal %in% specialist_seals, bundle_x_shape_pars = bundle_x_shape_pars, 
                 bundle_x_linear_pars = bundle_x_linear_pars, bundle_y_shape_pars = bundle_y_shape_pars)
    
    x[seal, t+1] <- as.numeric(update_output["x_t1"])
    y[seal, t+1] <- as.numeric(update_output["y_t1"])
    P_x[seal, t+1] <- as.numeric(update_output["P_x"])
    P_y[seal, t+1] <- as.numeric(update_output["P_y"])
    seal_prob_gauntlet[seal, t+1] <- P_x[seal, t+1] * P_y[seal, t+1]
    
    if(seal %in% kill_list){
      seal_prob_gauntlet[seal, t+1] <- NA
      seal_forage_loc[seal, t+1] <- NA
      x[seal, t+1] <- NA
      y[seal, t+1] <- NA
      C[seal, t] <- NA
      P_x[seal, t+1] <- NA
      P_y[seal, t+1] <- NA
    }
  }

  
  
  # stellers
  
  for(ssl in 1:num_ej){
    
    update_output <- updateLearning(salmon_consumed = salmon_consumed_ej[ssl, t], w = w, hunting = H_ej[t],
                                    x_t = x_ej[ssl, t], y_t = y_ej[ssl, t],
                                    forage_loc = ej_forage_loc[ssl, t], bundle_dx_pars = bundle_dx_pars,
                                    bundle_dy_pars = bundle_dy_pars, dead = ssl %in% kill_list_ej,
                                    baseline_x = baseline_x_val, baseline_y = specialist_baseline_y,
                                    specialist = T, bundle_x_shape_pars = bundle_x_shape_pars_sl, 
                                    bundle_x_linear_pars = bundle_x_linear_pars, 
                                    bundle_y_shape_pars = bundle_y_shape_pars_sl)
    x_ej[ssl, t+1] <- as.numeric(update_output["x_t1"])
    y_ej[ssl, t+1] <- as.numeric(update_output["y_t1"])
    P_x_ej[ssl, t+1] <- as.numeric(update_output["P_x"])
    P_y_ej[ssl, t+1] <- as.numeric(update_output["P_y"])
    ej_prob_gauntlet[ssl, t+1] <- P_x_ej[ssl, t+1] * P_y_ej[ssl, t+1]
    
    if(ssl %in% kill_list_ej){
      ej_prob_gauntlet[ssl, t+1] <- NA
      ej_forage_loc[ssl, t+1] <- NA
      x_ej[ssl, t+1] <- NA
      y_ej[ssl, t+1] <- NA
      C_ej[ssl, t] <- NA
      P_x_ej[ssl, t+1] <- NA
      P_y_ej[ssl, t+1] <- NA
    }
  }
  
  # californias
  
  for(csl in 1:num_zc){
    
    update_output <- updateLearning(salmon_consumed = salmon_consumed_zc[csl, t], w = w, hunting = H_zc[t],
                                    x_t = x_zc[csl, t], y_t = y_zc[csl, t],
                                    forage_loc = zc_forage_loc[csl, t], bundle_dx_pars = bundle_dx_pars,
                                    bundle_dy_pars = bundle_dy_pars, dead = csl %in% kill_list_zc,
                                    baseline_x = baseline_x_val, baseline_y = specialist_baseline_y,
                                    specialist = T, bundle_x_shape_pars = bundle_x_shape_pars_sl, 
                                    bundle_x_linear_pars = bundle_x_linear_pars, 
                                    bundle_y_shape_pars = bundle_y_shape_pars_sl)
    x_zc[csl, t+1] <- as.numeric(update_output["x_t1"])
    y_zc[csl, t+1] <- as.numeric(update_output["y_t1"])
    P_x_zc[csl, t+1] <- as.numeric(update_output["P_x"])
    P_y_zc[csl, t+1] <- as.numeric(update_output["P_y"])
    zc_prob_gauntlet[csl, t+1] <- P_x_zc[csl, t+1] * P_y_zc[csl, t+1]
    
    if(csl %in% kill_list_zc){
      zc_prob_gauntlet[csl, t+1] <- NA
      zc_forage_loc[csl, t+1] <- NA
      x_zc[csl, t+1] <- NA
      y_zc[csl, t+1] <- NA
      C_zc[csl, t] <- NA
      P_x_zc[csl, t+1] <- NA
      P_y_zc[csl, t+1] <- NA
    }
  }
  

} # days loop


