# calculate derivative expressions. apply 4th order runge-kutta to integrate over time period deltat
# From Tim Feb 2024
# Edited to add second predator pop June 2024

get_dXdt <- function(Ns, Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, F_catch, M, E) {
  Nseal <- max(Nseal, 1E-20)
  Nsealion <- max(Nsealion, 1E-20)
  dNdt <- ((-Cmax *alpha * Ns * Nseal^(1 + gamma)) / (Cmax + alpha * Ns * Nseal ^ gamma + Y)) - 
    ((Cmax_SL * alpha_SL * Ns * Nsealion ^ (1 + gamma_SL)) / (Cmax_SL + alpha_SL * Ns * Nsealion ^ gamma_SL + Y_SL)) - 
    F_catch * Ns - M * Ns - E * Ns
  dC2dt <- Cmax_SL * Nsealion *(alpha_SL * Ns * Nsealion ^ gamma_SL + Y_SL) / (Cmax_SL + alpha_SL * Ns * Nsealion ^ gamma_SL + Y_SL)
  dCdt <- Cmax * Nseal *(alpha * Ns * Nseal^gamma + Y) / (Cmax + alpha * Ns * Nseal^gamma + Y)
  dCatchdt <- F_catch * Ns
  dEdt <- E * Ns
  return(c(dNdt, dCdt, dC2dt, dCatchdt, dEdt))
}

rungeKutta <- function(X, Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, 
                       F_catch, M, E, n_species, deltat = deltat_val){
  K1s <- get_dXdt(Ns = X[1:n_species], Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, F_catch, M, E)
  midX <- X + deltat * 0.5 * K1s
  K2s <- get_dXdt(Ns = midX[1:n_species],Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, F_catch, M, E)
  midX <- X + deltat * 0.5 * K2s
  K3s <- get_dXdt(Ns = midX[1:n_species], Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, F_catch, M, E)
  endX <- X + deltat * K3s
  K4s <- get_dXdt(Ns = endX[1:n_species],Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, F_catch, M, E)
  Xsim <- X + deltat * (K1s / 6 + K2s / 3 + K3s / 3 + K4s / 6)
  return(c(Xsim))
}

run_rungeKutta <- function(Ns, n_species, Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, F_catch, M, E, deltat = deltat_val) {
  times <- seq(0, 1, by = deltat)
  if (times[length(times)]!= 1) {
    stop("deltat must be a division of 1")
  }
  n_species <- length(Ns)
  X <- c(Ns, rep(0, n_species), rep(0, n_species), rep(0, n_species), rep(0, n_species))
  for (i in 1:length(times)) {
    X <- rungeKutta(X, Cmax, Nseal, alpha, gamma, Y, Nsealion, Cmax_SL, alpha_SL, gamma_SL, Y_SL, 
                    F_catch, M, E, n_species, deltat = deltat)
  }
  X.res <- matrix(X, nrow = n_species, ncol = length(X)/n_species, byrow = F)
  colnames(X.res) <- c("Ns", "C", "C_SL", "Catch", "E")
  rownames(X.res) <- c("Sockeye", "Chinook", "Coho")
  return(X.res)
}

### TESTING SPACE

# seal consumption parameters
# deltat_val <- 1/24
# alpha <- 0.05
# Cmax <- 5
# gamma <- -1 # pred dep, this expects something between -1, 0
# Y <- 0 # other prey contribution
# 
# natural_mort <- 0.0005
# 
# # Ns <- c(0, 0, 0)
# # Ns <- c(5, 5, 5)
# Ns <- c(500, 10, 50)
# E <- c(0.3, 0.003, 0.1)
# F_catch <- c(0, 0, 0.1)
# gamma <- -0.5
# Nseal <- 10
# 
# run_rungeKutta(Ns = Ns, Cmax = Cmax, Nseal = Nseal, alpha = alpha, gamma = gamma, Y = Y,
#                  Nsealion = 0, Cmax_SL = 15, alpha_SL = alpha, gamma_SL = gamma, Y_SL = Y,
#                F_catch = F_catch, E = E, M = natural_mort, n_species = length(Ns))

# get_dXdt(Ns = Ns, Cmax = Cmax, Nseal = 0, alpha = alpha, gamma = gamma, Y = Y,
#                Nsealion = 0, Cmax_SL = 15, alpha_SL = alpha, gamma_SL = gamma, Y_SL = Y,
#                F_catch = F_catch, E = E, M = natural_mort)
