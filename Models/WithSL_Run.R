# this runs the model with the capacity for three pinniped species
# created July 2024

# clear environment
rm(list=ls())
# data input
source("Functions/Prep_data_for_Salmon_functions.R")

# prep functions
source("Functions/makeArray.R")
source("Functions/createHarvestPlan.R")

# parameters
source("Functions/WithSL_set_pars.R")
source("Functions/WithSL_initialize_variables.R")

# loop functions
source("Functions/salmonSpeciesUpdate.R")
source("Functions/decideForagingDestination.R")
source("Functions/collusion.R")
source("Functions/rungeKutta_3.R")
source("Functions/getHarvested.R")
source("Functions/learnX.R")
source("Functions/learnY.R")
source("Functions/type3FuncRes.R")
source("Functions/linearFuncRes.R")
source("Functions/updateLearning.R")

# run the loop
source("Models/The_Loop.R")

# plotting
source("Functions/makePlots.R")
source("Functions/Plots_Pv.R")
source("Functions/Plots_Ej.R")
source("Functions/Plots_Zc.R")

# check that it worked?
plot_x + plot_Px + plot_probs + plot_layout(guides = "collect")
plot_Py
plot_Px_ej + plot_probs_ej + plot_ej + plot_layout(guides = "collect")
