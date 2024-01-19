# community collusion

collusion <- function(forage_locs_list, prob_gauntlet_seal_i, receptivity, mean, beta) {
  alpha <- (-beta*mean)/(mean-1)
  receptivity <- dbeta(x = prob_gauntlet_seal_i, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
  P_social <- (1-alpha) * prob_gauntlet_seal_i + alpha * mean(forage_locs_list[,t,j])
  return(P_social)
}

collusion(seal_prob_gauntlet, 0.1, )

mean <- 0.5
beta <- 15
alpha <- (-beta*mean)/(mean-1)
dbeta(x = 0.2, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
