# function for updating x

learn_x <- function(food, x_t, step, xmin, xmax) {
  
  if(food > 0){
    d_x <- step*(xmax - x_t)
  } else if(food < 0){
    d_x <- step*(xmin - x_t)
  } else {d_x <- 0}
  
  x_next <- x_t + d_x
  return(x_next)
}

  