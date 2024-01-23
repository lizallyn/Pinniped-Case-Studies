for(seal in 1:num_seals){
  C[seal, t, j] <- salmon_consumed[seal, t, j]/seal_satiation - w
  if(C[seal, t, j] > 0){
    d_x <- 0.25*(xmax - x[seal, t, j])
  } else if(C[seal, t, j] < 0){
    d_x <- 0.25*(xmin - x[seal, t, j])
  } else {d_x <- 0}
  x[seal, t+1, j] <- x[seal, t+1, j] + d_x
  P_x[seal, t+1, j] <- x[seal, t+1, j] * 0.1 + 0.1
  
  if(sum(H[t, j]) == 0){
    d_y <- 0.25*(ymax - y[seal, t, j])
  } else if(sum(H[t, j]) > 0){
    d_y <- 0.25*(ymin - y[seal, t, j])
  }
  y[seal, t+1, j] <- y[seal, t, j] + d_y
  P_y[seal, t+1, j] <- 1-(1/(1.1 + exp(-steepness * (threshold - y[seal, t+1, j]))))
  
  seal_prob_gauntlet[seal, t+1, j] <- P_x[seal, t+1, j] * P_y[seal, t+1, j]
}