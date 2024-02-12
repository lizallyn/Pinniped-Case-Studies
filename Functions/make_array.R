# function to create arrays for loop variables

make.array <- function(x, y, z=NA, start.val, namex, namey, namez=NA) {
  if(is.na(z)){
    new.array <- array(dim = c(x, y), 
                       data = rep(start.val, times = x * y),
                       dimnames = list(namex = 1:x, namey = 1:y))
    names(dimnames(new.array)) <- c(namex, namey)
    return(new.array)
    
  } else {
    new.array <- array(dim = c(x, y, z), 
                       data = rep(start.val, times = prod(c(x, y, z), na.rm = T)),
                       dimnames = list(namex = 1:x, namey = 1:y, namez = 1:z))
    names(dimnames(new.array)) <- c(namex, namey, namez)
    return(new.array)
  }
}
