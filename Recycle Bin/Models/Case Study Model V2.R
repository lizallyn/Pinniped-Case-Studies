### Case Study Model with updated Equations
# started 10/16/2023

# Load function files
# Macbook
source("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped Case Studies/Functions/functions.R")
source("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped-Case-Studies/Functions/Sockeye arrival function creation.R")

# Github
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Sockeye%20arrival%20function%20creation.R")

# work desktop
source("/Users/Elizabeth Allyn/Documents/GitHub/Pinniped Case Studies/Functions/functions.R")
source("/Users/Elizabeth Allyn/Documents/GitHub/Pinniped Case Studies/Functions/Sockeye arrival function creation.R")

# Parameter values

nruns <- 365

# discovery rate
d <- 0.00000001

# decay I - N
decay <- 0.000000001

# mortality - fishing
M.F.g <- 0
M.F.e <- 0

# mortality - predation
M.C.g <- 0
M.C.e <- 0.1

# mortality - hunt
M.H.A <- 0
M.H.I <- 0
M.H.N <- 0

# mortality - natural
M.N <- 0.0001

# escape rate
E <- 0.3

leave_threshold <- 50 # this we can estimate from diet data I bet - how much does a salmon specialist eat? If anything less than that is available, then they would leave the S group?
leave_steepness <- 0.2 # this is probably very steep - almost a yes or no there is/isn't enough salmon

switch_threshold <- 500 # again I bet we can find a good starting point from diet data
switch_steepness <- 0.05

N <- rep(NA, nruns)
A <- rep(NA, nruns)
I <- rep(NA, nruns)
O.g <- rep(NA, nruns)
O.e <- rep(NA, nruns)

D <- rep(NA, nruns)
X.g <- rep(NA, nruns)
X.e <- rep(NA, nruns)
Mig.g <- rep(NA, nruns)
Mig.e <- rep(NA, nruns)

T.IN <- rep(NA, nruns)
T.IA <- rep(NA, nruns)
T.AI <- rep(NA, nruns)
H.N <- rep(NA, nruns)
H.I <- rep(NA, nruns)
H.A <- rep(NA, nruns)
Z.I <- rep(NA, nruns)
Z.A <- rep(NA, nruns)
C.g <- rep(NA, nruns)

phi.AI <- rep(NA, nruns)
phi.IA <- rep(NA, nruns)

# Starting Values

day <- 1

N[1] <- 13500
A[1] <- 0
I[1] <- 150
O.g[1] <- 0
O.e[1] <- 0

D[1] <- 0
X.g[1] <- 0
X.e[1] <- 0
Mig.g[1] <-0
Mig.e[1] <- 0
C.g[1] <- 0

T.IN[1] <- 0
T.IA[1] <- 0
T.AI[1] <- 0
H.N[1] <- 0
H.I[1] <- 0
H.A[1] <- 0
Z.I[1] <- 0
Z.A[1] <- 0

phi.AI <- 0
phi.IA <- 0

# The Loop
for (t in 2:nruns) {
  day[t] <- day[t-1] + 1
  
  O.g[t] <- X.g[t-1] + Mig.g[t-1]
  O.e[t] <- X.e[t-1] + Mig.e[t-1]
  
  D[t] <- N[t-1] * d
  X.g[t] <- O.g[t-1] * exp(-M.F.g - M.C.g - M.N - E)
  X.e[t] <- O.e[t-1] * exp(-M.F.e - M.N)
  Mig.g[t] <- predict.fish(day = t, params = fish.fit.optim$par, start.day = 163)
  Mig.e[t] <- O.g[t-1] * E
  C.g[t] <- (O.g[t-1] * M.C.g) / (E + M.N + M.F.g + M.C.g) * (1 - exp(-M.N - M.F.g - M.C.g - E))
  
  N[t] <- N[t-1] - D[t-1] - H.N[t-1] + T.IN[t-1]
  A[t] <- A[t-1] - T.AI[t-1] - Z.A[t-1] - H.A[t-1] + T.IA[t-1] + D[t-1]
  I[t] <- I[t-1] - T.IA[t-1] - Z.I[t-1] - H.I[t-1] - T.IN[t-1] + T.AI[t-1]
  
  T.IN[t] <- I[t-1] * decay
  T.IA[t] <- (I[t-1] * phi.IA[t-1]) / (phi.IA[t-1] + M.N + M.H.I) * (1 - exp(-M.N - M.H.I - phi.IA[t-1]))
  T.AI[t] <- (A[t-1] * phi.AI[t-1]) / (phi.AI[t-1] + M.N + M.H.A) * (1 - exp(-M.N - M.H.A - phi.AI[t-1]))
  H.N[t] <- N[t-1] * M.H.N
  H.I[t] <- (I[t-1] * M.H.I) / (phi.IA[t-1] + M.N + M.H.I) * (1 - exp(-M.N - M.H.I - phi.IA[t-1]))
  H.A[t] <- (A[t-1] * M.H.A) / (phi.AI[t-1] + M.N + M.H.A) * (1 - exp(-M.N - M.H.A - phi.AI[t-1]))
  Z.I[t] <- (I[t-1] * M.N) / (phi.IA[t-1] + M.N + M.H.I) * (1 - exp(-M.N - M.H.I - phi.IA[t-1]))
  Z.A[t] <- (A[t-1] * M.N) / (phi.AI[t-1] + M.N + M.H.A) * (1 - exp(-M.N - M.H.A - phi.AI[t-1]))
  
  avg.seals <- mean(c(A[t-1], A[t-2], A[t-3]))
  
  phi.AI[t] <- leaving.rate(salmon = O.g[t-1], 
                            seals = avg.seals, 
                            threshold = leave_threshold, 
                            steepness = leave_steepness)
  phi.IA[t] <- switch.rate(salmon = O.g[t-1], 
                           threshold = switch_threshold, 
                           steepness = switch_steepness)
  
}

plot(1:nruns, A)
