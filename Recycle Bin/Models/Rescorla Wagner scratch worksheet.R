# Rescorla-Wagner via Miller and Shettleworth 2007, 2013
# December 2023

# parameters
days <- 60
salience <- 0.15 # seems to be standard for this model? Can think about later

# variables (for now this matches the rectangle example in the 2007 paper)
V_G <- array(dim = days) # geography of gauntlet
V_W <- array(dim = days) # geography of open water
V_F <- array(dim = days, data = rep(0, days)) # presence of salmon
V_B <- array(dim = days) # some element(s) in common between both

V_gauntlet <- array(dim = days)
V_open <- array(dim = days)

P_gauntlet <- array(dim = days)
P_open <- array(dim = days)

# stupid little salmon presence/absence by day
# salmon everyday - works great
# salmon <- array(dim = days, data = rep(1, days))
# two salmon pulses
salmon <- array(dim = days, data = c(rep(0, days/4), rep(1, days/4), rep(0, days/4), rep(1, days/4)))

# start values
V_G[1] <- 0
V_W[1] <- 0
V_F[1] <- 0
V_B[1] <- 0.1 # bc something needed to be non-0 I think

# loop
for(day in 1:(days-1)) {
  V_gauntlet[day] <- V_G[day] + V_F[day] + V_B[day]
  V_open[day] <- V_W[day] + V_B[day]
  P_gauntlet[day] <- V_gauntlet[day]/(V_gauntlet[day] + V_open[day])
  P_open[day] <- V_open[day]/(V_gauntlet[day] + V_open[day])
  
  
  if(salmon[day] == 0){ #V_F not presented so no change, open water rewarded
    lambda_g <- 0
    lambda_o <- 1
    V_G[day+1] <- V_G[day] + salience * (lambda_g - (V_G[day] + V_B[day])) * P_gauntlet[day]
    V_W[day+1] <- V_W[day] + salience * (lambda_o - (V_W[day] + V_B[day])) * P_open[day]
    V_F[day+1] <- V_F[day]
    V_B[day+1] <- V_B[day] + 
      (salience * (lambda_o - (V_B[day] + V_W[day])) * P_open[day]) + 
      (salience * (lambda_g - (V_B[day] + V_G[day])) * P_gauntlet[day])
  } else { # gauntlet rewarded and V_F presented
    lambda_g <- 1
    lambda_o <- 0
    V_G[day+1] <- V_G[day] + salience * (lambda_g - (V_G[day] + V_F[day] + V_B[day])) * P_gauntlet[day]
    V_W[day+1] <- V_W[day] + salience * (lambda_o - (V_W[day] + V_B[day])) * P_open[day]
    V_F[day+1] <- V_F[day] + salience * (lambda_g - (V_G[day] + V_F[day] + V_B[day])) * P_gauntlet[day]
    V_B[day+1] <- V_B[day] + 
      (salience * (lambda_o - (V_B[day] + V_W[day])) * P_open[day]) + 
      (salience * (lambda_g - (V_B[day] + V_F[day] + V_G[day])) * P_gauntlet[day])
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

plot(1:days, salmon, main = "salmon")
plot(1:days, P_gauntlet, main = "P_gauntlet")
plot(1:days, P_open, main = "P_open")
plot(1:days, V_gauntlet, main = "V_gauntlet")
plot(1:days, V_open, main = "V_open")
plot(1:days, V_G, main = "V_G: unique traits of gauntlet")
plot(1:days, V_W, main = "V_W: unique traits of open water")
plot(1:days, V_F, main = "V_F: presence of fish")
plot(1:days, V_B, main = "V_B: common trait")

