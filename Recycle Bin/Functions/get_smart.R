# individual learning function with x and y

get_smart <- function(salmon_eaten, hunt_exposure, forage_loc, satiation, baseline, 
                      constraints, x, y, steepness, threshold, step, decay){
  xmin <- constraints[1]
  xmax <- constraints[2]
  ymin <- constraints[3]
  ymax <- constraints[4]
  
  C <- salmon_eaten/satiation - baseline
  
  # calculate x and P_x
  if(forage_loc == 0) {
    
    if(x == 0){
      d_x <- 0
    } else {d_x <- (x-0)/abs(x)}
    
  } else if(C > 0){
    d_x <- step*(xmax - x)
  } else if(C < 0){
    d_x <- step*(xmin - x)
  } else {d_x <- 0}
  x_next <- x + d_x
  P_x <- x_next * 0.1 + 0.1
  
  # calculate y and P_y
  if(forage_loc == 0) {
    
    if(y == 0){
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

## Testing space

# step = 0.25
# decay = 0.25
# 
# z <- -10:10
# plot(-10:10, z - (z-0)/abs(z))

salmon <- 0:19
hunting <- c(rep(0, 5), 1:5, 5:1, rep(0, 5))
data <- rep(NA, 20)
for(i in 1:20) {
  data[i] <- get_smart(salmon_eaten = salmon[i], hunt_exposure = hunting[i], forage_loc = 1, 
                    satiation = 3, baseline = 1, constraints = c(-1, 9, -10, 0), 
                    x = 0, y = -5, steepness = 5, threshold = -5, step = 0.25, decay = 0.25)
}
par(mfrow = c(3, 1))
plot(1:20, data)
plot(1:20, salmon, main = "salmon")
plot(1:20, hunting, main = "hunting")
