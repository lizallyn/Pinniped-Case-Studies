# Run the Nisqually Model for all three runs
# tracks Chum, Green River and LocNis separately


# clear environment
rm(list=ls())
# setwd("/Users/lizallyn/Documents/GitHub/Pinniped-Case-Studies")

# 01 Set-Up Functions
source("Nisqually/01_predictFish.R")
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

# 02 Prep Data
source("Nisqually/02_Prep_Nisqually_Salmon_Data.R")

# 03 Set Model Parameters
source("Nisqually/03_Nis_set_pars_All.R")

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

# check that it worked?
harvested
arrival_plot
gauntlet_plot
fished_plot
salmon_catch
salmon_escapement
salmon_consumed
plot_seals + plot_ej + plot_zc + plot_layout(guides = "collect")
plot_H + plot_H_ej + plot_H_zc
escape_plot
eaten_sp_plot
plot_eaten + plot_eaten_ej + plot_eaten_zc  + plot_layout(guides = "collect")

plot_Px + plot_probs + plot_seals + plot_layout(guides = "collect")
plot_Px_ej + plot_probs_ej + plot_ej + plot_layout(guides = "collect")
plot_Px_zc + plot_probs_zc + plot_zc + plot_layout(guides = "collect")



