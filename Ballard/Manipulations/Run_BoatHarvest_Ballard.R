# Run the model for all three pinnipeds and salmon runs
# and save relevant variables and parameters for comparison

# clear environment
# rm(list=ls())
# setwd("/Users/lizallyn/Documents/GitHub/Pinniped-Case-Studies")

# 01 Set-Up Functions
source("Functions/01_predictFish.R")
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

# 02 Prep Data
source("Ballard/02_Prep_Ballard_salmon_data.R")

# 03 Set Pars
source("Ballard/03_set_pars.R")

# adjust parameters for management scenario
steepness_H <- 5 # how quick does it saturate (higher = slower)
efficiency <- 0.125 # what prop of seals are they capable of taking

# 04 Initialize Variables
source("Ballard/04_initialize_variables.R")

# 05 Loop Functions
source("Ballard/05_salmonSpeciesUpdate.R")
source("Functions/decideForagingDestination.R")
source("Functions/collusion.R")
source("Functions/rungeKutta_3.R")
source("Functions/getHarvested.R")
source("Functions/learnX.R")
source("Functions/learnY.R")
source("Functions/type3FuncRes.R")
source("Functions/linearFuncRes.R")
source("Functions/updateLearning.R")

# 06 Run The Loop
source("Ballard/06_The_Loop.R")

# 07 Plots
source("Functions/makePlots.R")
source("Functions/Plots_Pv.R")
source("Functions/Plots_Ej.R")
source("Functions/Plots_Zc.R")
source("Ballard/07_Plots_salmon.R")
source("Functions/Plots_responses.R")

## save relevant variables and responses

# parameters
boat_parameters <- data.frame(rbind(start_loop, end_loop, num_seals, 
                                    prop_specialists, num_zc, 
                                    num_ej, min_fishers, max_fishers, fishery_open, 
                                    fishery_close, sockeye_residence, 
                                    chinook_residence, coho_residence,
                                    zone_efficiency, zone_steepness, steepness_H, 
                                    efficiency, scenario, scenario_sealion))

# salmon
boat_salmon <- rbind(salmon_escapement, salmon_eaten, salmon_catch)
rownames(boat_salmon) <- c("Escapement", "Eaten", "Catch")

boat_salmon_vars <- data.frame("Day" = 1:days + (start_loop - 1),
                               "Gauntlet_Sockeye" = gauntlet_sockeye,
                               "Gauntlet_Chinook" = gauntlet_chinook,
                               "Gauntlet_Coho" = gauntlet_coho,
                               "Escape_Sockeye" = escape_sockeye,
                               "Escape_Chinook" = escape_chinook,
                               "Escape_Coho" = escape_coho,
                               "Eaten_Sockeye" = eaten_sockeye,
                               "Eaten_Chinook" = eaten_chinook,
                               "Eaten_Coho" = eaten_coho,
                               "Fished_Sockeye" = fished_sockeye,
                               "Fished_Chinook" = fished_chinook,
                               "Fished_Coho" = fished_coho,
                               "Gauntlet_salmon" = gauntlet_salmon)
boat_salmon_vars$Eaten_salmon <- boat_salmon_vars$Eaten_Sockeye + boat_salmon_vars$Eaten_Chinook + 
  boat_salmon_vars$Eaten_Coho

# pinnipeds
boat_pinniped_harvest <- c(Pv = sum(H), Zc = sum(H_zc), Ej = sum(H_ej))

boat_pinniped_vars <- data.frame("Day" = 1:days, 
                                 "Gauntlet_Pv" = colSums(seal_forage_loc, na.rm = T),
                                 "Gauntlet_Ej" = colSums(ej_forage_loc, na.rm = T),
                                 "Gauntlet_Zc" = colSums(zc_forage_loc, na.rm = T),
                                 "H_Pv" = H,
                                 "H_Ej" = H_ej,
                                 "H_Zc" = H_zc,
                                 "Eaten_Pv" = colSums(salmon_consumed_pv, na.rm = T),
                                 "Eaten_Ej" = colSums(salmon_consumed_ej, na.rm = T),
                                 "Eaten_Zc" = colSums(salmon_consumed_zc, na.rm = T))
boat_pinniped_vars$H_sl <- boat_pinniped_vars$H_Ej + boat_pinniped_vars$H_Zc
boat_pinniped_vars <- boat_pinniped_vars %>% mutate(cum_H_sl = cumsum(H_sl))
boat_pinniped_vars <- boat_pinniped_vars %>% mutate(cum_H_Pv = cumsum(H_Pv))

boat_pinniped_vars$Gauntlet_sl <- boat_pinniped_vars$Gauntlet_Ej + boat_pinniped_vars$Gauntlet_Zc