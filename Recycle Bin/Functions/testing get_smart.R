# testing get_smart function using "separate x terms for fish and hunt" bits
# Jan 2024

seals <- 5
days <- 100

w <- 0.1
ymin <- -10
ymax <- 0
xmin <- -1
xmax <- 9
steepness <- 5
threshold <- -3

Pf <- array(dim = c(seals, days), data = rep(0.1, seals * days))
Pd <- array(dim = c(seals, days), data = rep(0.1, seals * days))
x <- array(dim = c(seals, days), data = rep(0, seals * days))
y <- array(dim = c(seals, days), data = rep(0, seals * days))
C <- array(dim = c(seals, days), data = rep(NA, seals * days))
B <- array(dim = c(seals, days), data = rep(NA, seals * days))
P_x <- array(dim = c(seals, days), data = rep(NA, seals * days))
P_y <- array(dim = c(seals, days), data = rep(NA, seals * days))
P <- array(dim = c(seals, days), data = rep(NA, seals * days))

salmon <- array(dim = c(days), data = c(rep(0, days/4), seq_len(days/4), rev(seq_len(days/4)), rep(0, days/4)))
hunting <- array(dim = c(days), data = c(rep(0, days/2), rep(1, days/2)))
satiation <- max(salmon)

for(i in 1:(days-1)){
  for(seal in 1:seals){
    C[seal, i] <- salmon[i]/satiation - w
    if(C[seal, i] > 0){
      d_x <- 0.25*(xmax - x[seal, i])
    } else if(C[seal, i] < 0){
      d_x <- 0.25*(xmin - x[seal, i])
    } else {d_x <- 0}
    x[seal, i+1] <- x[seal, i] + d_x
    P_x[seal, i+1] <- x[seal, i+1] * 0.1 + 0.1
    
    B[seal, i] <- hunting[i]
    if(B[seal, i] == 0){
      d_y <- 0.25*(ymax - y[seal, i])
    } else if(B[seal, i] > 0){
      d_y <- 0.25*(ymin - y[seal, i])
    }
    y[seal, i+1] <- y[seal, i] + d_y
    
    P_y[seal, i+1] <- 1-(1/(1.1 + exp(-steepness * (threshold - y[seal, i+1]))))
    
    P[seal, i+1] <- P_y[seal, i+1] * P_x[seal, i+1]
  }
}

par(mfrow = c(2, 1))
plot(1:days, colSums(C), main = "C")
plot(1:days, colSums(B), main = "B")
plot(1:days, colSums(x), main = "x")
plot(1:days, colSums(y), main = "y")
plot(1:days, colSums(P_x), main = "P_x")
plot(y, P_y)
plot(x, P_x)
plot(1:days, colMeans(P), main = "P")

