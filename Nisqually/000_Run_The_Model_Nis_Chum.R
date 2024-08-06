# Run the Nisqually Chum Model


# clear environment
rm(list=ls())

# 00 Functions
source("~/GitHub/PinnipedCaseStudies/Nisqually/00_predictFish.R")
source("~/GitHub/PinnipedCaseStudies/Functions/makeArray.R")
source("~/GitHub/PinnipedCaseStudies/Functions/createHarvestPlan.R")


# 01 Set Parameters
source("~/GitHub/PinnipedCaseStudies/Nisqually/01_Nis_set_pars.R")

# 02 Prep Data
source("~/GitHub/PinnipedCaseStudies/Nisqually/02_Prep_Nisqually_Salmon_Data.R")

# 03 Initialize Variables


# 04 Run The Loop
source("~/GitHub/PinnipedCaseStudies/Nisqually/04_The_Loop_Nis_Chum.R")