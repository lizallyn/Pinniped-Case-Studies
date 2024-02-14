# eat some fish, biomass version

eat_some_fish_bm <- function(mt_gauntlet_salmon, mt_seals_at_gauntlet, handling_time, satiation, pd, Y) {
  if(mt_gauntlet_salmon == 0 | mt_seals_at_gauntlet == 0){
    theoretical_salmon_to_eat <- 0
  } else {
    theoretical_salmon_to_eat <- (satiation*handling_time*mt_seals_at_gauntlet^(1+pd)*mt_gauntlet_salmon)/
      (satiation + handling_time*mt_gauntlet_salmon*mt_seals_at_gauntlet^pd+Y)
  }
  return(theoretical_salmon_to_eat)
}
