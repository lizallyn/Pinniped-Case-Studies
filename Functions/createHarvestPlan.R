# Create some harvest_plan scenarios

source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Prep_data_for_Harvest_functions.R")

### No harvest
createHarvestPlan <- function(scenario = c("None", "Zone", "Boat"), days, years, zone_rate = NA, salmon_days = NA, boat_days = NA, boat_rate = NA) {
  harvest_plan <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
  if(scenario == "None") {
    harvest_plan <- harvest_plan
  } else if(scenario == "Zone") {
    harvest_plan[salmon_days,] <- zone_rate
  } else if(scenario == "Boat") {
    harvest_plan[boat_days,] <- boat_rate
  } else {
    return("Not a valid scenario option. Try: None, Zone, Boat.")
  }
  return(harvest_plan)
}

