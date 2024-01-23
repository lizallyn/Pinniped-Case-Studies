# individual learning function with x and y

get_smart <- function(salmon_eaten, hunt_exposure, satiation, baseline, constraints, x, y, steepness, threshold, step){
  xmin <- constraints[1]
  xmax <- constraints[2]
  ymin <- constraints[3]
  ymax <- constraints[4]
  
  C <- salmon_eaten/satiation - baseline
  
  # calculate x and P_x
  if(C > 0){
    d_x <- 0.25*(xmax - x)
  } else if(C < 0){
    d_x <- 0.25*(xmin - x)
  } else {d_x <- 0}
  x_next <- x + d_x
  P_x <- x_next * 0.1 + 0.1
  
  # calculate y and P_y
  if(forage_loc == 0) {
    
    if(y > 0.9){
      d_y <- 0
    } else {d_y <- decay}
    
  } else if(hunt_exposure == 0){
      d_y <- step*(ymax - y)
  } else if(hunt_exposure > 0){
    d_y <- step*(ymin - y)
  }
  y_next <- y + d_y
  P_y <- 1-(1/(1.1 + exp(-steepness * (threshold - y_next))))
  
  P_gauntlet <- P_x * P_y
  return(P_gauntlet)
}

setp = 0.25
decay = 0.05