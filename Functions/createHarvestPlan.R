# Create some harvest_plan scenarios


createHarvestPlan <- function(scenario = c("None", "Zone", "Boat"), harvest_days, empty.array = oneDzeroes) {
  harvest_plan <- empty.array
  if(scenario == "None") {
    harvest_plan <- harvest_plan
  } else if(scenario == "Zone") {
    harvest_plan[harvest_days] <- scenario
  } else if(scenario == "Boat") {
    harvest_plan[harvest_days] <- scenario
  } else {
    return("Not a valid scenario option. Try: None, Zone, Boat.")
  }
  return(harvest_plan)
}

