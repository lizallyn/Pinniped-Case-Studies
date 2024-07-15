# calculate derivative expressions. apply 4th order runge-kutta to integrate over time period deltat
# From Tim Feb 2024

get_dXdt <- function(Ns, Cmax, Nseal, alpha, gamma, Y, F_catch, M, E) {
  dNdt <- -Cmax *alpha * Ns * Nseal^(1 + gamma) / (Cmax + alpha * Ns * Nseal^gamma + Y) - 
    F_catch * Ns - M * Ns - E * Ns
  dCdt <-   Cmax * Nseal *(alpha * Ns * Nseal^gamma + Y) / (Cmax + alpha * Ns * Nseal^gamma + Y)
  dCatchdt <- F_catch * Ns
  dEdt <- E * Ns
  return(c(dNdt, dCdt, dCatchdt, dEdt))
}

rungeKutta <- function(X, Cmax, Nseal, alpha, gamma, Y, F_catch, M, E, n_species, deltat = deltat){
  K1s <- get_dXdt(Ns = X[1:n_species], Cmax, Nseal, alpha, gamma, Y, F_catch, M, E)
  midX <- X + deltat* 0.5 * K1s
  K2s <- get_dXdt(Ns = midX[1:n_species],Cmax, Nseal, alpha, gamma, Y, F_catch, M, E)
  midX <- X + deltat * 0.5 * K2s
  K3s <- get_dXdt(Ns = midX[1:n_species], Cmax, Nseal, alpha, gamma, Y, F_catch, M, E)
  endX <- X + deltat * K3s
  K4s <- get_dXdt(Ns = endX[1:n_species],Cmax, Nseal, alpha, gamma, Y, F_catch, M, E)
  Xsim <- X + deltat * (K1s / 6 + K2s / 3 + K3s / 3 + K4s / 6)
  return(c(Xsim))
}

run_rungeKutta <- function(Ns, Cmax, Nseal, alpha, gamma, Y, F_catch, M, E, deltat = deltat_val) {
  times <- seq(0, 1, by = deltat)
  if (times[length(times)]!= 1) {
    stop("deltat must be a division of 1")
  }
  n_species <- length(Ns)
  X <- c(Ns, rep(0, n_species), rep(0, n_species), rep(0, n_species))
  for (i in 1:length(times)) {
    X <- rungeKutta(X, Cmax, Nseal, alpha, gamma, Y, F_catch, M, E, n_species, deltat = deltat)
  }
  X.res <- matrix(X, nrow = n_species, ncol = length(X)/n_species, byrow = F)
  colnames(X.res) <- c("Ns", "C", "Catch", "E")
  rownames(X.res) <- c("Sockeye", "Chinook", "Coho")
  return(X.res)
}

Ns <- c(0, 0, 0)
E <- c(0.3, 0.003, 0.1)
F_catch <- c(0, 0, 0.1)
gamma <- -0.5

run_rungeKutta(Ns = Ns, Cmax = Cmax, Nseal = 0, alpha = alpha, gamma = gamma, Y = Y,
               F_catch = F_catch, E = E, M = natural_mort)



