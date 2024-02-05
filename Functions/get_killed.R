# harvest according to a harvest plan matrix (?)

get_killed <- function(day_plan, num_gauntlet_seals, zone_efficiency, Hmax, processing, num_fishers, gamma, Y) {
  if(day_plan == 1) {
    harvested <- num_gauntlet_seals * zone_efficiency
  } else if(day_plan == 2) {
    harvested <- (Hmax * processing * num_fishers^(1+gamma) * num_gauntlet_seals)/
      (Hmax + processing * num_gauntlet_seals * num_fishers^gamma + Y)
  } else {harvested <- 0}
  return(harvested)
}

# plot(get_killed(2, 10, 0.8, 1, 0.05, 1:10, 0, 0))
