# function for calculating d_y if they go to the gauntlet

learnY <- function(hunting, y_t, forage_loc, bundle_dy_pars, dead, baseline = baseline_y) {
  step <- bundle_dy_pars["step"]
  ymin <- bundle_dy_pars["ymin"]
  ymax <- bundle_dy_pars["ymax"]
  decay <- bundle_dy_pars["decay"]
  

  if(dead == TRUE){
    d_y <- NA
  } else if(forage_loc == 0){
    if(y_t >= baseline) {
      d_y <- 0
    } else {
      d_y <- decay
      }
  } else {
    if(hunting == 0){
      d_y <- step*(ymax - y_t)
    } else if(hunting > 0){
      d_y <- step*(ymin - y_t)
    }
  }
  
  return(as.numeric(d_y))
}
