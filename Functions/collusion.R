# community collusion

collusion <- function(forage_locs_list, prob_gauntlet_seal_i, mean, beta) {
  alpha <- (-beta*mean)/(mean-1)
  receptivity <- dbeta(x = prob_gauntlet_seal_i, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
  max <- dbeta(x = mean, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
  min <- 0
  scaled_rec <- (receptivity - 0)/(max - min)
  P_social <- (1-scaled_rec) * prob_gauntlet_seal_i + scaled_rec * mean(forage_locs_list)
  return(P_social)
}


