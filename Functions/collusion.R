# community collusion

collusion <- function(probs_list, prob_gauntlet_of_seal, seals_2_copy, mean, beta) {
  if(seals_2_copy == 0){
    print("No seals to copy :(")
  } else {
    alpha <- (-beta*mean)/(mean-1)
    receptivity <- dbeta(x = prob_gauntlet_of_seal, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
    max <- dbeta(x = mean, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
    min <- 0
    scaled_rec <- (receptivity - min)/(max - min) # fix magic number
    social_info <- mean(probs_list[
      sample(1:length(probs_list), seals_2_copy, replace = F)], na.rm = T)
    if(is.nan(social_info)){
      P_social <- NA
    } else {
      P_social <- (1-scaled_rec) * prob_gauntlet_of_seal + scaled_rec * social_info
    }
    
    return(P_social)
  }
  
}

