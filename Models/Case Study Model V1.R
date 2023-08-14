### Case Study Model 1
### Migrated from .Rmd doc on July 5, 2023

# This used to be in "Model building SIN, Ops Ol, day step.Rmd" and was migrated to a .R on July 5, 2023.

## Packages, Functions, etc.
# these only work on the MacBook!
source("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped Case Studies/Functions/functions.R")
source("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped Case Studies/Functions/Sockeye arrival function creation.R")

# work desktop
source("/Users/Elizabeth Allyn/Documents/GitHub/Pinniped Case Studies/Functions/functions.R")
source("/Users/Elizabeth Allyn/Documents/GitHub/Pinniped Case Studies/Functions/Sockeye arrival function creation.R")

## Parameters

# for leaving and switch function
leave_threshold <- 50 # this we can estimate from diet data I bet - how much does a salmon specialist eat? If anything less than that is available, then they would leave the S group?
leave_steepness <- 0.2 # this is probably very steep - almost a yes or no there is/isn't enough salmon

switch_threshold <- 500 # again I bet we can find a good starting point from diet data
switch_steepness <- 0.05

# removals
Hunt.N <- 0
Hunt.I <- 0
Hunt.S <- 0

# discovery
d <- 0.00000001 # total BS right now, need a better approach that isn't a total guess

# predation
dailyconsumption <- 5 # also BS right now

# movement rate above the locks (basically 1/residence time)
ladder <- 0.4

# for loop
nruns <- 365
daysofyear <- 1:365

## Starting Values

# starting values

day <- 1

S <- rep(NA, nruns)
I <- rep(NA, nruns)
N <- rep(NA, nruns)

Ops <- rep(NA, nruns)
Ol <- rep(NA, nruns)
Osafe <- rep(NA, nruns)
salmon.arrive <- rep(NA, nruns)

S.switch <- rep(NA, nruns)
S.leave <- rep(NA, nruns)
S.arrive <- rep(NA, nruns)

discovery <- rep(NA, nruns)

removals.N <- rep(NA, nruns)
removals.I <- rep(NA, nruns)
removals.S <- rep(NA, nruns)

S[1] <- 0
I[1] <- 150
N[1] <- 13500

Ops[1] <- 1000000
Ol[1] <- 0
Osafe[1] <- 0
salmon.arrive[1] <- 0

S.switch[1] <- 0
S.leave[1] <- 0

discovery[1] <- NA

removals.N[1] <- 0
removals.I[1] <- 0
removals.S[1] <- 0

## The 1 Year Loop

for (t in 2:nruns) {
  day[t] <- day[t-1] + 1
  
  salmon.arrive[t] <- predict.fish(day = t, params = fish.fit.optim$par, start.day = 163)
  Ops[t] <- Ops[t-1] - salmon.arrive[t]
  
  #predation <- min(Ol[t-1], predation.rate(seals = S[t-1], consumption = dailyconsumption))
  predation <- 0
  salmon.leave <- ladder * Ol[t-1]
  Ol[t] <-  salmon.arrive[t] - predation
  Osafe[t] <- Osafe[t-1] + salmon.leave
  
  discovery[t] <- discovery.rate(seals = N[t-1], salmon = Ol[t-1], d = d)
  removals.N[t] <- Hunt.N*N[t-1]
  N[t] <- N[t-1] - discovery[t] - removals.N[t]
  
  S.switch[t] <- I[t-1] *
    switch.rate(salmon = Ol[t-1], 
                threshold = switch_threshold, 
                steepness = switch_steepness)
  S.leave[t] <- S[t-1] * 
    leaving.rate(salmon = Ol[t-1], 
                 seals = S[t-1], 
                 threshold = leave_threshold, 
                 steepness = leave_steepness)
  S[t] <- S[t-1] + S.switch[t] + discovery[t] - S.leave[t]
  I[t] <- I[t-1] + S.leave[t] - S.switch[t]
}

## Look at it

par(mfrow = c(1,2))
plot(day, N, type = "l", col = "dodgerblue", main = "normal")
plot(day, S, col = "dodgerblue", main = "specialists", type = "l")
plot(day, I, col = "dodgerblue", main = "inactive", type = "l")
plot(day, S.leave, col = "turquoise3", main = "leave", type = "l")
plot(day, discovery, col = "turquoise3", main = "discovery", type = "l")
plot(day, Ol, col = "salmon3", main = "Locks salmon", type = "l")
plot(day, Ops, col = "salmon3", main = "PS salmon", type = "l")
plot(day, salmon.arrive, col = "salmon3", main = "Arriving salmon", type = "l")
plot(day, Osafe, col = "salmon3", main = "Safe salmon", type = "l")
