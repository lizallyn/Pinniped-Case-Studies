# new version of consumption with satiation etc.

eat_some_fish <- function(gauntlet_salmon, num_seals_at_gauntlet, handling_time, satiation, pd, Y) {
  if(gauntlet_salmon == 0 | num_seals_at_gauntlet == 0){
    theoretical_salmon_to_eat <- 0
  } else {
    theoretical_salmon_to_eat <- (satiation*handling_time*num_seals_at_gauntlet^(1+pd)*gauntlet_salmon)/
      (satiation + handling_time*gauntlet_salmon*num_seals_at_gauntlet^pd+Y)
    salmon_per_seal <- theoretical_salmon_to_eat / num_seals_at_gauntlet
  }
  return(theoretical_salmon_to_eat)
}
