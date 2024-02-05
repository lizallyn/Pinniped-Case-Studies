# continuous time salmon from Tim
# Feb 2024

Ns <- 1000
Nseal <- 10
Cmax <- 5
alpha <- 1
Y <- 0
F <- 10 / 365
M <- 0.2 / 365
E <- 0.2
gamma <- 0

# function to calculate derivative expressions.
get_dXdt <- function(Cmax, Nseal, alpha, Ns, gamma, Y, F, M, E) {
  dNdt <- -Cmax *alpha * Ns * Nseal^(1 + gamma) / (Cmax + alpha * Ns * Nseal^gamma + Y) - F * Ns - M * Ns - E * Ns
  dCdt <-   Cmax * Nseal *(alpha * Ns * Nseal^gamma + Y) / (Cmax + alpha * Ns * Nseal^gamma + Y)
  dCatchdt <- F * Ns
  dEdt <- E * Ns
  return(c(dNdt, dCdt, dCatchdt, dEdt))
}

# apply 4th order runge-kutta to integrate over time period deltat
runge_kutta <- function(Cmax, Nseal, alpha, Ns, gamma, Y, F, M, E, deltat = 1){
  X <- c(Ns, 0, 0, 0)
  K1s <- get_dXdt(Cmax, Nseal, alpha, Ns = X[1], gamma, Y, F, M, E)
  midX <- X + deltat* 0.5 * K1s
  K2s <- get_dXdt(Cmax, Nseal, alpha, Ns= midX[1], gamma, Y, F, M, E)
  midX <- X + deltat * 0.5 * K2s
  K3s <- get_dXdt(Cmax, Nseal, alpha, Ns= midX[1], gamma, Y, F, M, E)
  endX <- X + deltat * K3s
  K4s <- get_dXdt(Cmax, Nseal, alpha, Ns= endX[1], gamma, Y, F, M, E)
  Xsim <- X + deltat * (K1s / 6 + K2s / 3 + K3s / 3 + K4s / 6)
  return(c(Ns = Xsim[1],
           C = Xsim[2],
           Catch = Xsim[3],
           E = Xsim[4]))
}

# example call - returns an array that has # Salmon remaining, the total number of salmon eaten, the number of salmon caught, and the number of salmon the escaped the gauntlet
day_result <- runge_kutta(Cmax, Nseal, alpha, Ns, gamma, Y, F, M, E)
