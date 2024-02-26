# function to create arrays for loop variables

makeArray <- function(dims, start.val, names) {
  namex <- names[1]
  namey <- names[2]
  namez <- names[3]
  
  if(length(dims) == 1){
    new.array <- rep(start.val, dims[1])
    
  } else if(length(dims) == 2){
    new.array <- array(dim = c(dims[1], dims[2]), 
                       data = rep(start.val, times = dims[1] * dims[2]),
                       dimnames = list(namex = 1:dims[1], namey = 1:dims[2]))
    names(dimnames(new.array)) <- c(namex, namey)
    
  } else  if(length(dims) == 3){
    new.array <- array(dim = c(dims[1], dims[2], dims[3]), 
                       data = rep(start.val, times = prod(c(dims[1], dims[2], dims[3]), na.rm = T)),
                       dimnames = list(namex = 1:dims[1], namey = 1:dims[2], namez = 1:dims[3]))
    names(dimnames(new.array)) <- c(namex, namey, namez)
  }
  return(new.array)
}


