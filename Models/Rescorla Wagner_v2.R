# Rescorla-Wagner via Miller and Shettleworth 2007, 2013
# December 2023

# parameters
days <- 100
salience <- 0.15 # seems to be standard for this model? Can think about later
beta <- 1 # added back but unclear if we'll ever mess with it?

# variables (for now this matches the rectangle example in the 2007 paper)
V_G <- array(dim = days) # geography of gauntlet
V_W <- array(dim = days) # geography of open water
V_F <- array(dim = days, data = rep(0, days)) # presence of salmon
# V_B <- array(dim = days) # some element(s) in common between both
V_H <- array(dim = days) # presence of hunt activity

V_gauntlet <- array(dim = days)
V_open <- array(dim = days)

P_gauntlet <- array(dim = days)
P_open <- array(dim = days)

# stupid little salmon presence/absence by day
# salmon everyday - works great
# salmon <- array(dim = days, data = rep(1, days))
# two salmon pulses
salmon <- array(dim = days, data = c(rep(0, days/4), rep(1, days/4), rep(1, days/4), rep(0, days/4)))
regime <- array(dim = days, data = c(rep(1, days/4), rep(2, days/4), rep(3, days/4), rep(4, days/4)))
lambda_gs <- array(dim = days, data = c(rep(0, days/4), rep(1, days/4), rep(0, days/2)))
lambda_os <- array(dim = days, data = c(rep(1, days/4), rep(0, days/4), rep(0, days/4), rep(1, days/4)))

# start values
V_G[1] <- 0.1
V_W[1] <- 0.1
V_F[1] <- 0
# V_B[1] <- 0.1 # bc something needed to be non-0 I think
V_H[1] <- 0

# loop
for(day in 1:(days-1)) {
  V_gauntlet[day] <- V_G[day] + V_F[day]
  V_open[day] <- V_W[day]
  P_gauntlet[day] <- V_gauntlet[day]/(V_gauntlet[day] + V_open[day])
  P_open[day] <- V_open[day]/(V_gauntlet[day] + V_open[day])
  
  if(regime[day] == 1){ # no fish no hunting
    lambda_g <- 0
    lambda_o <- 1
    V_G[day+1] <- V_G[day] + salience * beta * (lambda_g - (V_G[day] + V_H[day])) * P_gauntlet[day]
    V_W[day+1] <- V_W[day] + salience * beta * (lambda_o - (V_W[day])) * P_open[day]
    V_F[day+1] <- V_F[day]
    V_H[day+1] <- V_H[day]
    
  } else {
    if(regime[day] == 2) { # yes fish no hunting
      lambda_g <- 1
      lambda_o <- 0
      V_G[day+1] <- V_G[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day])) * P_gauntlet[day]
      V_W[day+1] <- V_W[day] + salience * beta * (lambda_o - (V_W[day])) * P_open[day]
      V_F[day+1] <- V_F[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day])) * P_gauntlet[day]
      V_H[day+1] <- V_H[day]
    } else {
      if(regime[day] == 3) { # yes fish yes hunting
        lambda_g <- 0
        lambda_o <- 0
        V_G[day+1] <- V_G[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day] + V_H[day])) * P_gauntlet[day]
        V_W[day+1] <- V_W[day] + salience * beta * (lambda_o - (V_W[day])) * P_open[day]
        V_F[day+1] <- V_F[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day] + V_H[day])) * P_gauntlet[day]
        V_H[day+1] <- V_H[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day] + V_H[day])) * P_gauntlet[day]
      } else { # no fish yes hunting
        lambda_g <- 0
        lambda_o <- 1
        V_G[day+1] <- V_G[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day] + V_H[day])) * P_gauntlet[day]
        V_W[day+1] <- V_W[day] + salience * beta * (lambda_o - (V_W[day])) * P_open[day]
        V_F[day+1] <- V_F[day]
        V_H[day+1] <- V_H[day] + salience * beta * (lambda_g - (V_G[day] + V_F[day] + V_H[day])) * P_gauntlet[day]
      }
    }
  }
  
  # correct P_Ls to 0 or 1 if out of range (MS 2013)
  if(P_gauntlet[day] > 1) {
    P_gauntlet[day] <- 1
  } else {
    if(P_gauntlet[day] < 0) {
      P_gauntlet[day] <- 0
    }
  }
  if(P_open[day] > 1) {
    P_open[day] <- 1
  } else {
    if(P_open[day] < 0) {
      P_open[day] <- 0
    }
  }
  
}
par(mfrow = c(2, 4))
# plot(1:days, salmon, main = "salmon")
plot(1:days, P_gauntlet, main = "P_gauntlet", ylim = c(0, 1))
lines((days/4):(days/4*3), rep(0.5, (days/2) + 1), col = "salmon", lwd = 2) # salmon presence
lines((days/2):days, rep(0.45, (days/2) + 1), col = "dodgerblue", lwd = 2) # hunt presence
lines(1:days, lambda_gs, lwd = 2, col = "goldenrod") # lambda for gauntlet
plot(1:days, P_open, main = "P_open", ylim = c(0, 1))
lines((days/4):(days/4*3), rep(0.5, (days/2) + 1), col = "salmon", lwd = 2)
lines((days/2):days, rep(0.45, (days/2) + 1), col = "dodgerblue", lwd = 2)
lines(1:days, lambda_os, lwd = 2, col = "turquoise") # lambda for open water
plot(1:days, V_gauntlet, main = "V_gauntlet", ylim = c(0, 1))
plot(1:days, V_open, main = "V_open", ylim = c(0, 1))
plot(1:days, V_G, main = "V_G: unique traits of gauntlet", ylim = c(-0.5, 1))
plot(1:days, V_W, main = "V_W: unique traits of open water", ylim = c(-0.5, 1))
plot(1:days, V_F, main = "V_F: presence of fish", ylim = c(-0.5, 1))
plot(1:days, V_H, main = "V_H: presence of hunt activity", ylim = c(-0.5, 1))
#plot(1:days, V_B, main = "V_B: common trait")

