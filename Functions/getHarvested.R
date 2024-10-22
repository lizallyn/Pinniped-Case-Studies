# harvest according to a harvest plan matrix (?)

getHarvested <- function(day_plan = 0, list_gauntlet_seals, zone_efficiency = NA, zone_steepness = NA,
                         num_fishers = NA, steepness = NA, efficiency = NA, round = "floor"){
  num_gauntlet_seals <- length(list_gauntlet_seals)
  if(day_plan == "Zone"){
    harvested <- num_gauntlet_seals * (zone_efficiency * num_gauntlet_seals/(zone_steepness + num_gauntlet_seals))
  } else if(day_plan == "Boat"){
    harvested <- efficiency * num_fishers * num_gauntlet_seals/(steepness + num_fishers)
  } else if(day_plan == 0){
    harvested <- 0
  } else {
    print("No harvest plan submitted")
  }
  if(round == "floor") {
    return(floor(harvested))
  } else {
    return(harvested)
  }
}

# efficiency <- seq(0, 1, 0.0001)
# steepness <- seq(0, 1000, 1)
# num_fishers <- 0:25
# plot(num_fishers, getHarvested(day_plan = "Boat", list_gauntlet_seals = 1:150, zone_efficiency = 0.8,
#                                num_fishers = 15, efficiency = 0.8, steepness = 5, round = F))
# 
# zone_efficiency <- 0.25
# num_gauntlet_seals <- 1:150
# steepness <- 10
# plot(num_gauntlet_seals, num_gauntlet_seals * (zone_efficiency * num_gauntlet_seals/(steepness + num_gauntlet_seals)))

# getHarvested <- function(day_plan, num_gauntlet_seals, zone_efficiency, Hmax, processing, min_fishers, max_fishers, gamma, Y) {
#   num_fishers <- sample(min_fishers:max_fishers, 1)
#   if(day_plan == "Zone") {
#     harvested <- num_gauntlet_seals * zone_efficiency
#   } else if(day_plan == "Boat") {
#     harvested <- (Hmax * processing * num_fishers^(1+gamma) * num_gauntlet_seals)/
#       (Hmax + processing * num_gauntlet_seals * num_fishers^gamma + Y)
#   } else {harvested <- 0}
#   return(harvested)
# }
# 

# getHarvested <- function(day_plan, list_gauntlet_seals, zone_efficiency, num_fishers, quota, efficiency){
#   if(day_plan == "Zone"){
#     harvested <- num_gauntlet_seals * zone_efficiency
#   } else if(day_plan == "Boat"){
#     harvested <- (quota * efficiency * num_fishers * num_gauntlet_seals)/
#       (1 + quota * efficiency * num_gauntlet_seals * num_fishers)
#   } else if(day_plan == "None"){
#     harvested <- 0
#   } else {
#     print("No harvest plan submitted")
#   }
#   return(harvested)
# }



# num_fishers <- 0:50
# efficiency <- seq(0, 1, 0.0001)
# steepness <- seq(0, 10, 0.1)
# 
# plot(num_fishers, getHarvested(day_plan = "Boat", list_gauntlet_seals = 1:20, zone_efficiency = NA,
#                                num_fishers = num_fishers, efficiency = 0.08, steepness = 10))


# getHarvested(day_plan = "Boat", list_gauntlet_seals = 1:20, zone_efficiency = 0.8, num_fishers = num_fishers, efficiency = 0.5, steepness = 5)
