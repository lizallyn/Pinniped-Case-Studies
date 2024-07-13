# creates a type III functional response

type3FuncRes <- function(bundle_shape_pars, val){
  buffer <- bundle_shape_pars["buffer"]
  steepness <- bundle_shape_pars["steepness"]
  threshold <- bundle_shape_pars["threshold"]
  
  y <- 1-(1/((1+buffer) + exp(-steepness * (threshold - val))))
  
  return(as.numeric(y))
}
