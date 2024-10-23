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
base_parameters <- data.frame(rbind(num_seals, num_zc, 
                                    num_ej, min_fishers, max_fishers, min_chum_boats, max_chum_boats,
                                    zone_efficiency, zone_steepness, steepness_H, 
                                    efficiency, scenario, scenario_sealion))

# salmon
base_salmon <- rbind(salmon_escapement, salmon_consumed, salmon_catch)
rownames(base_salmon) <- c("Escapement", "Eaten", "Catch")

base_salmon_vars <- data.frame("Day" = 1:days,
                               "Gauntlet_Chum" = gauntlet_chum,
                               "Gauntlet_GR" = gauntlet_gr,
                               "Gauntlet_LN" = gauntlet_ln,
                               "Escape_Chum" = escape_chum,
                               "Escape_GR" = escape_gr,
                               "Escape_LN" = escape_ln,
                               "Eaten_Chum" = eaten_chum,
                               "Eaten_GR" = eaten_gr,
                               "Eaten_LN" = eaten_ln,
                               "Fished_Chum" = fished_chum,
                               "Fished_GR" = fished_gr,
                               "Fished_LN" = fished_ln,
                               "Gauntlet_salmon" = gauntlet_salmon)
base_salmon_vars$Eaten_salmon <- base_salmon_vars$Eaten_Chum + base_salmon_vars$Eaten_GR + 
  base_salmon_vars$Eaten_LN

# pinnipeds
base_pinniped_harvest <- c(Pv = sum(H), Zc = sum(H_zc), Ej = sum(H_ej))

base_pinniped_vars <- data.frame("Day" = 1:days, 
                                 "Gauntlet_Pv" = colSums(seal_forage_loc, na.rm = T),
                                 "Gauntlet_Ej" = colSums(ej_forage_loc, na.rm = T),
                                 "Gauntlet_Zc" = colSums(zc_forage_loc, na.rm = T),
                                 "H_Pv" = H,
                                 "H_Ej" = H_ej,
                                 "H_Zc" = H_zc,
                                 "Eaten_Pv" = colSums(salmon_consumed_pv, na.rm = T),
                                 "Eaten_Ej" = colSums(salmon_consumed_ej, na.rm = T),
                                 "Eaten_Zc" = colSums(salmon_consumed_zc, na.rm = T))
base_pinniped_vars$H_sl <- base_pinniped_vars$H_Ej + base_pinniped_vars$H_Zc
base_pinniped_vars <- base_pinniped_vars %>% mutate(cum_H_sl = cumsum(H_sl))
base_pinniped_vars <- base_pinniped_vars %>% mutate(cum_H_Pv = cumsum(H_Pv))
