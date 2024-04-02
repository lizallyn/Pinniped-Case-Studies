### This function runs the whole thing and can be setup to run different types of runs

setwd("C:/Users/Elizabeth Allyn/Documents/GitHub/PinnipedCaseStudies")

assembleTheLegos <- function(run, path_to_pars, path_to_vars){
  ## Load Data Files and Setup Functions 
  source("Functions/Prep_data_for_Salmon_functions.R")
  source("Functions/Prep_data_for_Harvest_functions.R")
  
  source("Functions/makeArray.R")
  source("Functions/createHarvestPlan.R")
  
  
## Set Parameters and Create Variables
  if(run == "Base"){
    source("Functions/BaseRun_set_pars.R")
    source("Functions/BaseRun_initialize_variables.R")
  } else if(run == "Experiment") {
    source("Functions/set_pars.R")
    source("Functions/initialize_variables.R")
  } else {
    print("Not a valid run type. Try Base or Experiment")
  }
  
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

assembleTheLegos("Experiment", path_to_pars = "Functions/set_pars.R", path_to_vars = "Functions/initialize_variables.R")

