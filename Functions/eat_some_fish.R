# create eat_some_fish function

eat_some_fish <- function(gauntlet_salmon, num_seals_at_gauntlet, handling_time) {
  if(gauntlet_salmon == 0 | num_seals_at_gauntlet == 0){
    salmon_consumed <- rep(0, num_seals_at_gauntlet)
  } else {
    salmon_to_be_eaten <- gauntlet_salmon * 
      (num_seals_at_gauntlet / (1 + num_seals_at_gauntlet + 
                                      seal_handling_time * gauntlet_salmon))
    salmon_per_seal <- salmon_to_be_eaten / num_seals_at_gauntlet
    salmon_consumed <- rpois(num_seals_at_gauntlet, salmon_per_seal)
  }
  return(salmon_consumed)
}

# eat_some_fish(0, 7, 0.1)

