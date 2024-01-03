# create eat_some_fish function

eat_some_fish <- function(gauntlet_salmon, num_seals_at_gauntlet, handling_time) {
  if(gauntlet_salmon == 0 | num_seals_at_gauntlet == 0){
    theoretical_salmon_to_eat <- rep(0, num_seals_at_gauntlet)
  } else {
    theoretical_salmon_to_eat <- gauntlet_salmon * 
      (num_seals_at_gauntlet / (1 + num_seals_at_gauntlet + 
                                      handling_time * gauntlet_salmon))
    salmon_per_seal <- theoretical_salmon_to_eat / num_seals_at_gauntlet
    salmon_consumed <- round(runif(num_seals_at_gauntlet, min = 0, max = salmon_per_seal))
  }
  return(theoretical_salmon_to_eat)
}

