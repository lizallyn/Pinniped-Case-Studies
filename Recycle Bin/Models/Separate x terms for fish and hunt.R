# testing out having 2 separate x terms for fish and hunt
# Jan 2024

seals <- 5
days <- 20
xmax <- 10
a <- 1
b <- 2
w <- 0.1
ymax <- 0
xmin <- -2
ymin <- -10


Pf <- array(dim = c(seals, days), data = rep(0.1, seals * days))
Pd <- array(dim = c(seals, days), data = rep(0.1, seals * days))
x <- array(dim = c(seals, days), data = rep(0, seals * days))
y <- array(dim = c(seals, days), data = rep(0, seals * days))
C <- array(dim = c(seals, days), data = rep(NA, seals * days))
B <- array(dim = c(seals, days), data = rep(NA, seals * days))

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
    
    B[seal, i] <- hunting[i]
    if(B[seal, i] == 0){
      d_y <- 0.25*(ymax - y[seal, i])
    } else if(B[seal, i] > 0){
      d_y <- 0.25*(ymin - y[seal, i])
    }
    y[seal, i+1] <- y[seal, i] + d_y
  }
}

plot(1:days, colSums(C))
plot(1:days, colSums(B))
plot(1:days, colSums(x))
plot(1:days, colSums(y))

