# Run the haulout reduction scenario and save relevant outputs

# clear environment
# rm(list=ls())
# setwd("/Users/lizallyn/Documents/GitHub/Pinniped-Case-Studies")

# 01 Set-Up Functions
source("Functions/01_predictFish.R")
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

# 02 Prep Data
source("Nisqually/02_Prep_Nisqually_Salmon_Data.R")

# 03 Set Model Parameters
source("Nisqually/03_Nis_set_pars_All.R")

# ~~ adjust parameters for management scenario
# source pool reduction factor:
reduction <- 0.5
# adjust parameters for management scenario
num_seals <- num_seals * reduction
num_ej <- num_ej * reduction
num_zc <- num_zc * reduction

# 04 Initialize Variables
source("Nisqually/04_Nis_initialize_variables_All.R")

# 05 Loop Functions
source("Nisqually/05_salmonSpeciesUpdate_All.R")
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
source("Nisqually/06_The_Loop_Nis_All_fishing.R")

# 07 Plots
source("Functions/makePlots.R")
source("Functions/Plots_Pv.R")
source("Functions/Plots_Ej.R")
source("Functions/Plots_Zc.R")
source("Nisqually/07_Plots_All.R")
source("Functions/Plots_responses.R")

## Save relevant variables and responses

# parameters
haulout_parameters <- data.frame(rbind(num_seals, num_zc, 
                               num_ej, chin_boats, min_chum_boats, max_chum_boats,
                               zone_efficiency, zone_steepness, steepness_H, efficiency, scenario))

# salmon
haulout_salmon <- rbind(salmon_escapement, salmon_consumed, salmon_catch)
rownames(haulout_salmon) <- c("Escapement", "Eaten", "Catch")

haulout_salmon_vars <- data.frame("Day" = 1:days,
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
haulout_salmon_vars$Eaten_salmon <- haulout_salmon_vars$Eaten_Chum + haulout_salmon_vars$Eaten_GR + 
  haulout_salmon_vars$Eaten_LN

# pinnipeds
haulout_pinniped_harvest <- c(Pv = sum(H), Zc = sum(H_zc), Ej = sum(H_ej))
haulout_pinniped_vars <- data.frame("Day" = 1:days, 
                                 "Gauntlet_Pv" = colSums(seal_forage_loc, na.rm = T),
                                 "Gauntlet_Ej" = colSums(ej_forage_loc, na.rm = T),
                                 "Gauntlet_Zc" = colSums(zc_forage_loc, na.rm = T),
                                 "H_Pv" = H,
                                 "H_Ej" = H_ej,
                                 "H_Zc" = H_zc,
                                 "Eaten_Pv" = colSums(salmon_consumed_pv, na.rm = T),
                                 "Eaten_Ej" = colSums(salmon_consumed_ej, na.rm = T),
                                 "Eaten_Zc" = colSums(salmon_consumed_zc, na.rm = T))
haulout_pinniped_vars$H_sl <- haulout_pinniped_vars$H_Ej + haulout_pinniped_vars$H_Zc
haulout_pinniped_vars <- haulout_pinniped_vars %>% mutate(cum_H_sl = cumsum(H_sl))
haulout_pinniped_vars <- haulout_pinniped_vars %>% mutate(cum_H_Pv = cumsum(H_Pv))
