# harvest according to a harvest plan matrix (?)

get_killed <- function(day_plan, num_gauntlet_seals, zone_efficiency, Hmax, handling_time, num_fishers, gamma, Y) {
  if(day_plan == 1) {
    harvested <- num_gauntlet_seals * zone_efficiency
  } else if(day_plan == 2) {
    harvested <- (Hmax * handling_time * num_fishers^(1+gamma) * num_gauntlet_seals)/
      (Hmax + handling_time * num_gauntlet_seals * num_fishers^gamma + Y)
  }
}
  
