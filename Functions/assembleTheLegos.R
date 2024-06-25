### This function runs the whole thing and can be setup to run different types of runs

assembleTheLegos <- function(path_to_pars, path_to_vars, override){
  ## Load Data Files and Setup Functions 
  source("Functions/Prep_data_for_Salmon_functions.R")
  
  source("Functions/makeArray.R")
  source("Functions/createHarvestPlan.R")
  
  ## Set Parameters and Create Variables
  source(path_to_pars)
  source(path_to_vars)
  
  ## Load Function Files
  source("Functions/salmonSpeciesUpdate.R")
  source("Functions/decideForagingDestination.R")
  source("Functions/collusion.R")
  source("Functions/rungeKutta_2.R")
  source("Functions/getHarvested.R")
  source("Functions/learnX.R")
  source("Functions/learnY.R")
  
  ## Run the Loop
  source("Models/The_Loop.R")
  
  ## Plots
  source("Functions/Plots.R")
  
}

assembleFirstHalf <- function(path_to_pars){
  ## Load Data Files and Setup Functions 
  source("Functions/Prep_data_for_Salmon_functions.R")
  
  source("Functions/makeArray.R")
  source("Functions/createHarvestPlan.R")
  
  ## Set Parameters and Create Variables
  source(path_to_pars)
}

assembleSecondHalf <- function(path_to_vars){
  source(path_to_vars)
  
  ## Load Function Files
  source("Functions/salmonSpeciesUpdate.R")
  source("Functions/decideForagingDestination.R")
  source("Functions/collusion.R")
  source("Functions/rungeKutta_2.R")
  source("Functions/getHarvested.R")
  source("Functions/learnX.R")
  source("Functions/learnY.R")
  
  ## Run the Loop
  source("Models/The_Loop.R")
  
  ## Plots
  source("Functions/Plots.R")
}


# assembleFirstHalf(path_to_pars = "Functions/BaseRun_set_pars.R")
# num_seals <- 2
# assembleSecondHalf(path_to_vars = "Functions/BaseRun_initialize_variables.R")

