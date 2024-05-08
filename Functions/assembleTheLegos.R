### This function runs the whole thing and can be setup to run different types of runs

assembleTheLegos <- function(path_to_pars, path_to_vars){
  ## Load Data Files and Setup Functions 
  source("Functions/Prep_data_for_Salmon_functions.R")
  source("Functions/Prep_data_for_Harvest_functions.R")
  
  source("Functions/makeArray.R")
  source("Functions/createHarvestPlan.R")
  
  ## Set Parameters and Create Variables
  source(path_to_pars)
  source(path_to_vars)
  
  ## Load Function Files
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
  
}


