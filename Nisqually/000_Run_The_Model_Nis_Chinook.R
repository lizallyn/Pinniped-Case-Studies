# Run the Nisqually Chinook Model
# tracks Green River and LocNis separately


# clear environment
rm(list=ls())

# 00 Set-Up Functions
source("Nisqually/00_predictFish.R")
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

# 01 Set Parameters
source("Nisqually/01_Nis_set_pars.R")

# 02 Prep Data
source("Nisqually/02_Prep_Nisqually_Salmon_Data.R")

# 03 Initialize Variables
source("Nisqually/03_Nis_set_pars_Chinook.R")
source("Nisqually/03_Nis_initialize_variables_Chinook.R")

# 04 Loop Functions
source("Nisqually/04_salmonSpeciesUpdate_Chinook.R")
source("Functions/decideForagingDestination.R")
source("Functions/collusion.R")
source("Functions/rungeKutta_3.R")
source("Functions/getHarvested.R")
source("Functions/learnX.R")
source("Functions/learnY.R")
source("Functions/type3FuncRes.R")
source("Functions/linearFuncRes.R")
source("Functions/updateLearning.R")

# 05 Run The Loop
source("Nisqually/05_The_Loop_Nis_Chinook.R")

# 06 Plots
source("Functions/makePlots.R")
source("Functions/Plots_Pv.R")
source("Functions/Plots_Ej.R")
source("Functions/Plots_Zc.R")
source("Nisqually/06_Plots_Chinook.R")
source("Functions/Plots_responses.R")

# check that it worked?
gauntlet_plot
plot_Px + plot_probs + plot_seals + plot_layout(guides = "collect")
plot_Px_ej + plot_probs_ej + plot_ej + plot_layout(guides = "collect")
plot_Px_zc + plot_probs_zc + plot_zc + plot_layout(guides = "collect")

escape_plot
eaten_sp_plot
plot_eaten + plot_eaten_ej + plot_eaten_zc  + plot_layout(guides = "collect")
