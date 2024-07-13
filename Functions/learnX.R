# function for calculating d_x

learnX <- function(food, x_t, forage_loc, bundle_dx_pars, dead, baseline) {
  step <- bundle_dx_pars["step"]
  xmin <- bundle_dx_pars["xmin"]
  xmax <- bundle_dx_pars["xmax"]
  decay <- bundle_dx_pars["decay"]
  
  if(dead == TRUE){
    d_x <- NA
  } else {
    if(forage_loc == 0){
      if(x_t == baseline){
        d_x <- 0
      } else {
        d_x <- decay * (baseline - x_t)
      }
    } else {
      if(food > 0){
        d_x <- step * (xmax - x_t)
      } else {
        d_x <- step * (xmin - x_t)
      }
    }
  }
  
  return(as.numeric(d_x))
}
