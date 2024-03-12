# community collusion

collusion <- function(probs_list, prob_gauntlet_of_seal, seals_2_copy, mean, beta) {
  if(seals_2_copy == 0){
    print("No seals to copy :(")
  } else {
    if (is.na(prob_gauntlet_of_seal)){
      P_social <- prob_gauntlet_of_seal
    } else {
      alpha <- (-beta*mean)/(mean-1)
      receptivity <- dbeta(x = prob_gauntlet_of_seal, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
      max <- dbeta(x = mean, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
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

