# Run the model for all three pinnipeds and salmon runs

# clear environment
rm(list=ls())
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
