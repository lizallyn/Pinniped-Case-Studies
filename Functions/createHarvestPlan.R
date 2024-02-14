# Create some harvest_plan scenarios


createHarvestPlan <- function(scenario = c("None", "Zone", "Boat"), days, years, salmon_days = NA, boat_days = NA) {
  harvest_plan <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
  if(scenario == "None") {
    harvest_plan <- harvest_plan
  } else if(scenario == "Zone") {
    harvest_plan[salmon_days,] <- scenario
  } else if(scenario == "Boat") {
    harvest_plan[boat_days,] <- scenario
  } else {
    return("Not a valid scenario option. Try: None, Zone, Boat.")
  }
  return(harvest_plan)
}

