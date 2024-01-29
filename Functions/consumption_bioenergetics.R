# Bioenergetics of consumption
# from conversation with Isaac on Dec 7, 2023
# and the need to use weight in Tim's consumption equation

# average Pv kg
female <- 66
male <- 89

sockeye <- 6
chinook <- 8
coho <- 4

mean_salmon_mt <- mean(c(sockeye, chinook, coho)) * 0.001

mean_seal_mt <- mean(c(female, male)) * 0.001

eat_some_fish_bm <- function(mt_gauntlet_salmon, mt_seals_at_gauntlet, handling_time, satiation, pd, Y) {
  if(mt_gauntlet_salmon == 0 | mt_seals_at_gauntlet == 0){
    theoretical_salmon_to_eat <- 0
  } else {
    theoretical_salmon_to_eat <- (satiation*handling_time*mt_seals_at_gauntlet^(1+pd)*mt_gauntlet_salmon)/
      (satiation + handling_time*mt_gauntlet_salmon*mt_seals_at_gauntlet^pd+Y)
  }
  return(theoretical_salmon_to_eat)
}


eat_some_fish_bm(mt_gauntlet_salmon = 6, mt_seals_at_gauntlet = 0.7, 
                 handling_time = 0.001, satiation = 0.006, pd = -0.5, Y = 0.001)

