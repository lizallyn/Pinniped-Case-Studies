# community collusion

collusion <- function(probs_list, prob_gauntlet_of_seal, seals_2_copy, mean, beta) {
  if(length(probs_list) < 2){
    P_social <- prob_gauntlet_of_seal
    return(P_social)
  } else {
    if (is.na(prob_gauntlet_of_seal)){
      P_social <- prob_gauntlet_of_seal
    } else {
      alpha_c <- (-beta*mean)/(mean-1)
      receptivity <- dbeta(x = prob_gauntlet_of_seal, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0)
      max <- dbeta(x = mean, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0)
      min <- 0
      scaled_rec <- (receptivity - min)/(max - min)
      
      social_info <- sample(probs_list, seals_2_copy, replace = F)
      mean_social_info <- mean(social_info, na.rm = T)
      if(all(is.na(social_info))){
        P_social <- prob_gauntlet_of_seal
      } else {
        P_social <- (1-scaled_rec) * prob_gauntlet_of_seal + scaled_rec * mean_social_info
      }
    }
    return(P_social)
  }
}

# prob <- seq(0, 1, 0.01)
# mean <- 0.5
# beta <- 100
# alpha_c <- (-beta*mean)/(mean-1)
# plot(prob, dbeta(x = prob, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0))

