# Run the Nisqually Chum Model


# clear environment
rm(list=ls())

# 00 Set-Up Functions
source("Nisqually/00_predictFish.R")
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

# 01 Set Parameters
source("Nisqually/01_Nis_set_pars_Chum.R")

# 02 Prep Data
source("Nisqually/02_Prep_Nisqually_Salmon_Data.R")

# 03 Initialize Variables
source("Nisqually/03_Nis_set_pars_Chum.R")
source("Nisqually/03_Nis_initialize_variables_Chum.R")

# 04 Loop Functions
source("Nisqually/04_salmonSpeciesUpdate_Chum.R")
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
source("~/GitHub/PinnipedCaseStudies/Nisqually/05_The_Loop_Nis_Chum.R")

# 06 Plots
source("Functions/makePlots.R")
source("Functions/Plots_Pv.R")
source("Functions/Plots_Ej.R")
source("Functions/Plots_Zc.R")
source("Functions/06_Plots_chum.R")
source("Functions/Plots_responses.R")
