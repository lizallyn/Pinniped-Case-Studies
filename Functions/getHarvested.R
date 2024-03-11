# harvest according to a harvest plan matrix (?)

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
# getHarvested <- function(day_plan, list_gauntlet_seals, zone_efficiency, min_fishers, max_fishers, quota, efficiency){
#   num_fishers <- sample(min_fishers:max_fishers, 1)
#   if(day_plan == "Zone"){
#     harvested <- num_gauntlet_seals * zone_efficiency
#   } else if(day_plan == "Boat"){
#     harvested <- (quota * num_fishers * num_gauntlet_seals)/
#       (quota + efficiency * num_gauntlet_seals * num_fishers)
#   } else if(day_plan == "None"){
#     harvested <- 0
#   } else {
#     print("No harvest plan submitted")
#   }
#   return(harvested)
# }

getHarvested <- function(day_plan, list_gauntlet_seals, zone_efficiency, num_fishers, quota, efficiency){
  if(day_plan == "Zone"){
    harvested <- num_gauntlet_seals * zone_efficiency
  } else if(day_plan == "Boat"){
    harvested <- (efficiency * num_fishers * num_gauntlet_seals)/
      (1 + (1/quota) * efficiency * num_gauntlet_seals * num_fishers)
  } else if(day_plan == "None"){
    harvested <- 0
  } else {
    print("No harvest plan submitted")
  }
  return(harvested)
}

# num_fishers <- 0:25
# getHarvested(day_plan = "Boat", list_gauntlet_seals = 1:5, zone_efficiency = 0.8, 
#              num_fishers = num_fishers, quota = 20, efficiency = 0.01)
# plot(num_fishers, getHarvested(day_plan = "Boat", list_gauntlet_seals = 1:5, zone_efficiency = 0.8, 
#                                num_fishers = num_fishers, quota = 10, efficiency = 0.05))

