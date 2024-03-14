## Load Data Files
source("Functions/Prep_data_for_Salmon_functions.R")
source("Functions/Prep_data_for_Harvest_functions.R")

## Load Function Files
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")
source("Functions/salmonSpeciesUpdate.R")
source("Functions/decideForagingDestination.R")
source("Functions/collusion.R")
source("Functions/rungeKutta.R")
source("Functions/getHarvested.R")
source("Functions/learnX.R")
source("Functions/learnY.R")

## Load Set Up Files
source("Functions/BaseRun_set_pars.R")
source("Functions/BaseRun_initialize_variables.R")

## Run the Loop
source("Models/Run_The_Loop.R")

## Plots
source("Functions/Plots.R")

gauntlet_plot / eaten_sp_plot / escape_plot + plot_layout(guides = "collect")

eaten_sp_plot/plot_eaten/plot_C/plot_x/plot_Px + plot_layout(guides = "collect")

plot_seals/plot_H/plot_y/plot_Py + plot_layout(guides = "collect")

plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")
