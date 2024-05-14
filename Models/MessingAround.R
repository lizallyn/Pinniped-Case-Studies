# Need to mess with some stuff today


source("Functions/Prep_data_for_Salmon_functions.R")
source("Functions/Prep_data_for_Harvest_functions.R")

source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

source("Functions/BaseRun_set_pars.R")

# make changes
num_seals <- 150
intercept_x <- 0.01

source("Functions/BaseRun_initialize_variables.R")

# make changes


source("Functions/salmonSpeciesUpdate.R")
source("Functions/decideForagingDestination.R")
source("Functions/collusion.R")
source("Functions/rungeKutta.R")
source("Functions/getHarvested.R")
source("Functions/learnX.R")
source("Functions/learnY.R")

## Run the Loop
source("Models/The_Loop.R")

## Plots
source("Functions/Plots.R")

plot_seals
plot_eaten/plot_C/plot_x/plot_Px + plot_layout(guides = "collect", axes = "collect")
plot_probs/plot_Psoc + plot_layout(guides = "collect", axes = "collect")
