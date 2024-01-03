## Testing out the x thing

seals <- 5
days <- 10
xmax <- 10
s <- 1


P <- array(dim = c(seals, days), data = rep(0.1, seals * days))
x <- array(dim = c(seals, days), data = rep(0, seals * days))
C <- array(dim = c(seals, days), data = rep(NA, seals * days))
B <- array(dim = c(seals, days), data = rep(NA, seals * days))

salmon <- array(dim = c(days), data = c(rep(0, days/2), seq_len(days/2)))

for(i in 1:days){
  for(seal in 1:seals){
    C[seal, i] <- salmon[i]
    B[seal,i] <- 
    delta <- a*C[seal,i] - b*B[seal,i]
    x[seal,i+1] <- x[seal,i] + delta*(xmax - x[seal,i])
  }
}