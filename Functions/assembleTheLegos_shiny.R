# version for shiny with specific parameter manipulation

assembleTheLegos_shiny <- function(num_seals_input, seals_copy_input, w_input, 
                                   x_intercept_input, step_input, decay_input, 
                                   cmax_input, alpha_input){
  ## Load Data Files and Setup Functions 
  source("Functions/Prep_data_for_Salmon_functions.R")
  source("Functions/Prep_data_for_Harvest_functions.R")
  
  source("Functions/makeArray.R")
  source("Functions/createHarvestPlan.R")
  
  ## Set Parameters and Create Variables
  source("Functions/Shiny_set_pars.R")
  
  step <- step_input
  decay <- decay_input
  Cmax <- cmax_input
  alpha <- alpha_input
  
  twoDzeroes <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  salmon_consumed <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  seal_prob_gauntlet <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  seal_forage_loc <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  
  seals_at_gauntlet_save <- list(rep(NA, days))
  
  x <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  y <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  C <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  P_x <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  P_y <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  
  P_social <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  
  source("Functions/Shiny_initialize_variables.R")
  
  ## Load Function Files
  source("Functions/salmonSpeciesUpdate.R")
  source("Functions/decideForagingDestination.R")
  source("Functions/collusion.R")
  source("Functions/rungeKutta.R")
  source("Functions/getHarvested.R")
  source("Functions/learnX.R")
  source("Functions/learnY.R")
  
  ## Run the Loop
  for(t in 1:(days-1)) {

    # salmon arrive at the gauntlet
    daily_update <- salmonSpeciesUpdate(day = t, data = Daily_fish)
    gauntlet_chinook[t] <- daily_update$Chinook[1]
    gauntlet_sockeye[t] <- daily_update$Sockeye[1]
    gauntlet_coho[t] <- daily_update$Coho[1]
    gauntlet_salmon[t] <- sum(c(gauntlet_chinook[t], gauntlet_sockeye[t], gauntlet_coho[t]))

    # decide where each seal goes that day
    seal_forage_loc[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = decideForagingDestination)

    # round of copying
    if(seals_copy_input > 0){
      P_social[,t] <- sapply(X = seal_prob_gauntlet[,t], FUN = collusion,
                             probs_list = seal_prob_gauntlet[,t], seals_2_copy = seals_copy_input,
                             mean = mean, beta = beta)
      seal_forage_loc[,t] <- sapply(X = P_social[,t], FUN = decideForagingDestination)
    }

    # calculate salmon mortality
    seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
    seals_at_gauntlet_save[[t]] <- seals_at_gauntlet
    num_seals_at_gauntlet <- length(seals_at_gauntlet)
    salmon_result <- run_rungeKutta(Cmax = Cmax, Nseal = num_seals_at_gauntlet,
                                    alpha = alpha, Ns = c(gauntlet_sockeye[t], gauntlet_chinook[t], gauntlet_coho[t]),
                                    gamma = gamma, Y = Y,
                                    E = c(sockeye_escape_rate, chinook_escape_rate, coho_escape_rate),
                                    F_catch = c(sockeye_catch_rate[t], chinook_catch_rate[t], coho_catch_rate[t]),
                                    M = natural_mort, deltat = 1/24)

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
    for(seal in 1:num_seals_input){
      if(!(seal %in% kill_list)){
        C[seal, t] <- salmon_consumed[seal, t] - w_input

        # calculate d_x and d_y
        d_x <- learnX(food = C[seal, t], x_t = x[seal, t],
                      forage_loc = seal_forage_loc[seal, t],  step = step,
                      xmin = xmin, xmax = xmax, decay = decay, dead = seal %in% kill_list)
        d_y <- learnY(hunting = H[t], y_t = y[seal, t],
                      seal_forage_loc[seal, t], step = step,
                      ymin = ymin, ymax = ymax, decay = decay, dead = seal %in% kill_list)

        # update x and y and P_x and P_y
        x[seal, t+1] <- x[seal, t] + d_x
        P_x[seal, t+1] <- x[seal, t+1] * slope_x + x_intercept_input

        y[seal, t+1] <- y[seal, t] + d_y
        P_y[seal, t+1] <- 1-(1/((1+buffer_Pymin) + exp(-steepness * (threshold - y[seal, t+1]))))

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


  ## Plots
  
  library(reshape2)
  library(ggplot2)
  library(patchwork)
  
  salmon.colors <- c("seagreen", "salmon", "goldenrod")
  salmon.names <- c("Chinook", "Sockeye", "Coho")
  names(salmon.colors) <- salmon.names
  
  # Daily Salmon at Gauntlet
  gauntlet.data <- data.frame(cbind(1:days, gauntlet_chinook, gauntlet_sockeye, gauntlet_coho))
  colnames(gauntlet.data) <- c("Day", "Chinook", "Sockeye", "Coho")
  gauntlet.data <- melt(gauntlet.data, "Day", variable.name = "Species", value.name = "Count")
  gauntlet_plot <- ggplot(data = gauntlet.data, aes(x = Day, y = Count)) + 
    geom_point(aes(color = Species)) +
    scale_color_manual(values = salmon.colors) +
    labs(y = "Daily Salmon at Gauntlet") + 
    theme(legend.position = "bottom")
  
  # Daily Seals at Gauntlet
  seal.data <- data.frame(cbind(1:days, colSums(seal_forage_loc)))
  colnames(seal.data) <- c("Day", "Count")
  plot_seals <- ggplot(data = seal.data, aes(x = Day, y = Count)) +
    geom_point(color = "dodgerblue") +
    labs(y = "Num Seals at the Gauntlet")
  
  # Daily Salmon Eaten by Species
  eaten.sp.data <- data.frame(cbind(1:days, eaten_chinook, eaten_sockeye, eaten_coho))
  colnames(eaten.sp.data) <- c("Day", "Chinook", "Sockeye", "Coho")
  eaten.sp.data <- melt(eaten.sp.data, "Day", variable.name = "Species", value.name = "Count")
  eaten_sp_plot <- ggplot(data = eaten.sp.data, aes(x = Day, y = Count)) + 
    geom_point(aes(color = Species)) +
    scale_color_manual(values = salmon.colors) +
    labs(y = "Daily Salmon Eaten") + 
    theme(legend.position = "none")
  
  # Prob Gauntlet Plot
  prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")
  plot_probs <- ggplot(data = prob_gauntlet_plot, aes(x = Day, y = Prob_G, color = Seal)) +
    geom_point() +
    labs(y = "Prob_Gauntlet") + 
    theme(legend.position = "none")
  
  # Daily Salmon Eaten per Seal
  eaten_plot <- prepForPlots(salmon_consumed, value.col = "eaten")
  plot_eaten <- ggplot(data = eaten_plot, aes(x = Day, y = eaten, color = Seal)) +
    geom_point() +
    labs(y = "Salmon Eaten per Seal") + 
    theme(legend.position = "none")
  
  # Prob Social Plot
  Psoc_plot <- prepForPlots(P_social, value.col = "P_social")
  plot_Psoc <- ggplot(data = Psoc_plot, aes(x = Day, y = P_social, color = Seal)) +
    geom_point() + 
    theme(legend.position = "none")
  
  # x Plot
  x_plot <- prepForPlots(x, value.col = "x")
  plot_x <- ggplot(data = x_plot, aes(x = Day, y = x, color = Seal)) + 
    geom_point() + 
    labs(y = "x") + 
    theme(legend.position = "none")
  
  ## Composite Plots
  
  plot_num_seals <- plot_probs / plot_Psoc / plot_seals / eaten_sp_plot + 
    plot_layout(guides = "collect", axes = "collect")
  
  # plot_x <- plot_eaten / plot_x + 
  #   plot_layout(guides = "collect", axes = "collect")
  
  ## Plots to Return
  
  plot.list <- list("Salmon_G" = gauntlet_plot,
                    "Salmon_Eaten" = plot_num_seals,
                    "X" = plot_x)
  
  
  return(plot.list)
  
}

# assembleTheLegos_shiny(num_seals_input = 20,
#                        seals_copy_input = 10,
#                        w_input = 0.9,
#                        x_intercept_input = 0.03,
#                        step_input = 0.25,
#                        decay_input = 0.05,
#                        cmax_input = 1, 
#                        alpha_input = 0.1)[["X"]]

