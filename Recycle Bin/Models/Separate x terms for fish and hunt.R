# testing out having 2 separate x terms for fish and hunt
# Jan 2024

seals <- 5
days <- 20
xmax <- 10
a <- 1
b <- 2
w <- 0.1
xfmax <- 10
xfmin <- 2
xdmin <- -10


Pf <- array(dim = c(seals, days), data = rep(0.1, seals * days))
Pd <- array(dim = c(seals, days), data = rep(0.1, seals * days))
xf <- array(dim = c(seals, days), data = rep(0, seals * days))
xd <- array(dim = c(seals, days), data = rep(0, seals * days))
C <- array(dim = c(seals, days), data = rep(NA, seals * days))
B <- array(dim = c(seals, days), data = rep(NA, seals * days))

salmon <- array(dim = c(days), data = c(rep(0, days/4), seq_len(days/4), rev(seq_len(days/4)), rep(0, days/4)))
hunting <- array(dim = c(days), data = c(rep(0, days/2), rep(1, days/2)))
satiation <- max(salmon)

for(i in 1:(days-1)){
  for(seal in 1:seals){
    C[seal, i] <- salmon[i]/satiation - w
    if(C[seal, i] > 0){
      d_xf <- 0.25*(xfmax - xf[seal, i])
    } else if(C[seal, i] < 0){
      d_xf <- 0.25*(xfmin - xf[seal, i])
    } else {d_xf <- 0}
    xf[seal, i+1] <- xf[seal, i] + d_xf
    
    B[seal, i] <- hunting[i]
    if(B[seal, i] == 0){
      d_xd <- 0.25*(0 - xd[seal, i])
    } else if(B[seal, i] > 0){
      d_xd <- 0.25*(xdmin - xd[seal, i])
    }
    xd[seal, i+1] <- xd[seal, i] + d_xd
  }
}

plot(1:days, colSums(C))
plot(1:days, colSums(B))
plot(1:days, colSums(xf))
plot(1:days, colSums(xd))
