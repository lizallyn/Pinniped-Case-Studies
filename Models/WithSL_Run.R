# this runs the model with the capacity for three pinniped species
# created July 2024


source("Functions/Prep_data_for_Salmon_functions.R")

source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

source("Functions/WithSL_set_pars.R")
source("Functions/WithSL_initialize_variables.R")

source("Functions/salmonSpeciesUpdate.R")
source("Functions/decideForagingDestination.R")
source("Functions/collusion.R")
source("Functions/rungeKutta_3.R")
source("Functions/getHarvested.R")
source("Functions/learnX.R")
source("Functions/learnY.R")

## Run the Loop
source("Models/The_Loop.R")

source("Functions/makePlots.R")
source("Functions/Plots_Pv.R")
source("Functions/Plots_Ej.R")
source("Functions/Plots_Zc.R")

plot_Px + plot_probs + plot_seals + plot_layout(guides = "collect")
