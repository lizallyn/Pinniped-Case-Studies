# function for calculating d_y if they go to the gauntlet

learnY <- function(hunting, y_t, forage_loc, step, ymin, ymax, decay) {
  if(forage_loc == 0){
    d_y <- decay
  } else {
    if(hunting == 0){
      d_y <- step*(ymax - y_t)
    } else if(hunting > 0){
      d_y <- step*(ymin - y_t)
    }
  }
  
  return(d_y)
}
