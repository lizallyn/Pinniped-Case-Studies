# unpack and use linear response pars

linearFuncRes <- function(bundle_pars, val) {
  slope <- bundle_pars["slope"]
  intercept <- bundle_pars["intercept"]
  
  y <- val * slope + intercept
  
  return(as.numeric(y))
}
