# community collusion

collusion <- function(forage_locs_list, prob_gauntlet_seal_i, mean, beta) {
  alpha <- (-beta*mean)/(mean-1)
  receptivity <- dbeta(x = prob_gauntlet_seal_i, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
  max <- dbeta(x = mean, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
  min <- 0
  scaled_rec <- (receptivity - 0)/(max - min)
  P_social <- (1-scaled_rec) * prob_gauntlet_seal_i + scaled_rec * mean(forage_locs_list[,t,j])
  return(P_social)
}

collusion(forage_locs_list = seal_prob_gauntlet, prob_gauntlet_seal_i = 0.2, mean = 0.5, beta = 15)

receptivity <- dbeta(x = 0, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
(1-receptivity) * 0.5 + receptivity * 0.1
