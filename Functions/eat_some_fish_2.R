# create eat_some_fish function

eat_some_fish_2 <- function(gauntlet_salmon, num_seals_at_gauntlet, handling_time) {
  if(gauntlet_salmon == 0 | num_seals_at_gauntlet == 0){
    theoretical_salmon_to_eat <- 0
  } else {
    theoretical_salmon_to_eat <- gauntlet_salmon *
      (num_seals_at_gauntlet / (1 + num_seals_at_gauntlet + 
                                  handling_time * gauntlet_salmon))
    salmon_per_seal <- theoretical_salmon_to_eat / num_seals_at_gauntlet
  }
  return(theoretical_salmon_to_eat)
}

eat_some_fish_2(100, 5, 0.1)
