# Create some harvest_plan scenarios

source()

### No harvest
createHarvestPlan <- function(scenario = c("None", "Zone", "Boat"), days, years, zone_rate = NA, salmon_days = NA, boat_data, boat_rate = NA) {
  if(scenario == "None") {
    harvest_plan <- makeArray(days, years, 0, "Day", "Year")
  } else if(scenario == "Zone") {
    harvest_plan <- makeArray(days, years, 0, "Day", "Year")
    harvest_plan[salmon_days,] <- zone_rate
  } else if(scenario == "Boat") {
    
  } else {
    return("Not a valid scenario option. Try: None, Zone, Boat.")
  }
  }
  
}