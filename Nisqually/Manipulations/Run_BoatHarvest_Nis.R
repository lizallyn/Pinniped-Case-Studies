# Run the boat harvest scenario and save relevant outputs

# clear environment
rm(list=ls())
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
steepness_H <- 5 # how quick does it saturate (higher = slower)
efficiency <- 0.25 # what prop of seals are they capable of taking

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