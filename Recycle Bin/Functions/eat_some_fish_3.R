# new version of consumption with satiation etc.

eat_some_fish <- function(num_gauntlet_salmon, num_seals_at_gauntlet, handling_time, satiation, pd, Y) {
  if(num_gauntlet_salmon == 0 | num_seals_at_gauntlet == 0){
    theoretical_predation_rate <- 0
  } else {
    theoretical_salmon_to_eat <- (satiation*handling_time*num_seals_at_gauntlet^(1+pd)*num_gauntlet_salmon)/
      (satiation + handling_time*num_gauntlet_salmon*num_seals_at_gauntlet^pd+Y)
    theoretical_predation_rate <- theoretical_salmon_to_eat/num_gauntlet_salmon
  }
  return(theoretical_salmon_to_eat/num_seals_at_gauntlet)
}

# satiation <- seq(0, 2, 0.1)
# plot(satiation, eat_some_fish(500, 20, 0.1, satiation, 0, 0))
