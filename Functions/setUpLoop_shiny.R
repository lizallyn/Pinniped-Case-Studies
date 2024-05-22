setUpLoop_shiny <- function(num_seals_input){
  ## Load Data Files and Setup Functions 
  source("Functions/Prep_data_for_Salmon_functions.R")
  source("Functions/Prep_data_for_Harvest_functions.R")
  
  source("Functions/makeArray.R")
  source("Functions/createHarvestPlan.R")
  
  ## Set Parameters and Create Variables
  source("Functions/Shiny_set_pars.R")
  num_seals <- num_seals_input
  twoDzeroes <- makeArray(c(num_seals_input, days), start.val = 0, names = c("Seal", "Day"))
  source("Functions/Shiny_initialize_variables.R")
  
  ## Load Function Files
  source("Functions/salmonSpeciesUpdate.R")
  source("Functions/decideForagingDestination.R")
  source("Functions/collusion.R")
  source("Functions/rungeKutta.R")
  source("Functions/getHarvested.R")
  source("Functions/learnX.R")
  source("Functions/learnY.R")
}

