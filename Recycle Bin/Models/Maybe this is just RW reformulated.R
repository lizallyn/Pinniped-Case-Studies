## Testing out the x thing

seals <- 5
days <- 100
xmax <- 10
a <- 1
b <- 2

P <- array(dim = c(seals, days), data = rep(0.1, seals * days))
x <- array(dim = c(seals, days), data = rep(0, seals * days))
C <- array(dim = c(seals, days), data = rep(NA, seals * days))
B <- array(dim = c(seals, days), data = rep(NA, seals * days))

salmon <- array(dim = c(days), data = c(rep(0, days/4), seq_len(days/4), rev(seq_len(days/4)), rep(0, days/4)))
hunting <- array(dim = c(days), data = c(rep(0, days/2), rep(1, days/2)))

for(i in 1:(days-1)){
  for(seal in 1:seals){
    C[seal, i] <- salmon[i]
    B[seal,i] <- hunting[i]
    delta <- a*C[seal,i] - b*B[seal,i]
    x[seal,(i+1)] <- x[seal,i] + delta
    if(x[seal,i+1]>xmax){
      x[seal,i+1] <- xmax
    }
  }
}

plot(1:days, x[1,])
