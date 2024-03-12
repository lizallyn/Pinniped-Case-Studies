# function for calculating d_x

learnX <- function(food, x_t, forage_loc, step, xmin, xmax, decay, dead) {
  if(dead == TRUE){
    d_x <- NA
  } else if(forage_loc == 0){
    if(x_t == 0){
      d_x <- 0
      } else {
      d_x <- decay * (0-x_t)/abs(x_t)
    }
  } else {
    if(food > 0){
      d_x <- step*(xmax - x_t)
    } else if(food < 0){
      d_x <- step*(xmin - x_t)
    } else {d_x <- 0}
  }
  
  return(d_x)
}

