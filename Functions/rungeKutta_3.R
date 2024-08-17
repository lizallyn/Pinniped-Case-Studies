# calculate derivative expressions. apply 4th order runge-kutta to integrate over time period deltat
# From Tim Feb 2024
# Edited to add second predator pop June 2024
# third added July 2024

get_dXdt <- function(Ns, Cmax, Nseal, alpha, gamma, Y=0, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL=0, Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL=0, F_catch, M, E) {
  Nseal <- max(Nseal, 1E-20)
  NSSL <- max(NSSL, 1E-20)
  NCSL <- max(NCSL, 1E-20)
  dNdt <- ((-Cmax *alpha * Ns * Nseal^(1 + gamma)) / (Cmax + alpha * Ns * Nseal ^ gamma + Y)) - 
    ((Cmax_SSL * alpha_SSL * Ns * NSSL ^ (1 + gamma_SSL)) / (Cmax_SSL + alpha_SSL * Ns * NSSL ^ gamma_SSL + Y_SSL)) - 
    ((Cmax_CSL * alpha_CSL * Ns * NCSL ^ (1 + gamma_CSL)) / (Cmax_CSL + alpha_CSL * Ns * NCSL ^ gamma_CSL + Y_CSL)) - 
    F_catch * Ns - M * Ns - E * Ns
  dCdt <- Cmax * Nseal *(alpha * Ns * Nseal^gamma + Y) / (Cmax + alpha * Ns * Nseal^gamma + Y)
  dC2dt <- Cmax_SSL * NSSL *(alpha_SSL * Ns * NSSL ^ gamma_SSL + Y_SSL) / (Cmax_SSL + alpha_SSL * Ns * NSSL ^ gamma_SSL + Y_SSL)
  dC3dt <- Cmax_CSL * NCSL *(alpha_CSL * Ns * NCSL ^ gamma_CSL + Y_CSL) / (Cmax_CSL + alpha_CSL * Ns * NCSL ^ gamma_CSL + Y_CSL)
  dCatchdt <- F_catch * Ns
  dEdt <- E * Ns
  return(c(dNdt, dCdt, dC2dt, dC3dt, dCatchdt, dEdt))
}

rungeKutta <- function(X, Cmax, Nseal, alpha, gamma, Y=0, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL=0, Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL=0, 
                       F_catch, M, E, n_species, deltat){
  K1s <- get_dXdt(Ns = X[1:n_species], Cmax, Nseal, alpha, gamma, Y, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL, 
                  Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL, F_catch, M, E)
  midX <- X + deltat * 0.5 * K1s
  K2s <- get_dXdt(Ns = midX[1:n_species],Cmax, Nseal, alpha, gamma, Y, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL, 
                  Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL, F_catch, M, E)
  midX <- X + deltat * 0.5 * K2s
  K3s <- get_dXdt(Ns = midX[1:n_species], Cmax, Nseal, alpha, gamma, Y, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL, 
                  Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL, F_catch, M, E)
  endX <- X + deltat * K3s
  K4s <- get_dXdt(Ns = endX[1:n_species],Cmax, Nseal, alpha, gamma, Y, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL, 
                  Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL, F_catch, M, E)
  Xsim <- X + deltat * (K1s / 6 + K2s / 3 + K3s / 3 + K4s / 6)
  return(c(Xsim))
}

run_rungeKutta <- function(Ns, species_list, Cmax, Nseal, alpha, gamma, Y=0, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL=0, 
                           Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL=0, F_catch, M, E, deltat) {
  times <- seq(0, 1, by = deltat)
  if (times[length(times)]!= 1) {
    stop("deltat must be a division of 1")
  }
  n_species <- length(species_list)
  X <- c(Ns, rep(0, n_species), rep(0, n_species), rep(0, n_species), rep(0, n_species), rep(0, n_species))
  for (i in 1:length(times)) {
    X <- rungeKutta(X, Cmax, Nseal, alpha, gamma, Y, NSSL, NCSL, Cmax_SSL, alpha_SSL, gamma_SSL, Y_SSL, 
                    Cmax_CSL, alpha_CSL, gamma_CSL, Y_CSL, 
                    F_catch, M, E, n_species, deltat)
  }
  X.res <- matrix(X, nrow = n_species, ncol = length(X)/n_species, byrow = F)
  colnames(X.res) <- c("Ns", "C", "C_SSL", "C_CSL", "Catch", "E")
  rownames(X.res) <- species_list
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
# Nseal <- 100
# NSSL <- 5
# NCSL <- 20
# 
# run_rungeKutta(Ns = Ns, species_list = c("Sockeye", "Chinook", "Coho"), Cmax = Cmax, Nseal = Nseal, alpha = alpha, gamma = gamma, Y = Y,
#                  NSSL = NSSL, NCSL = NCSL, Cmax_SSL = 20, alpha_SSL = alpha, gamma_SSL = gamma, Y_SSL = Y,
#                Cmax_CSL = 15, alpha_CSL = alpha, gamma_CSL = gamma, Y_CSL = Y,
#                F_catch = F_catch, M = natural_mort, E = E, deltat = deltat_val)
# 
# get_dXdt(Ns = Ns, Cmax = Cmax, Nseal = 0, alpha = alpha, gamma = gamma, Y = Y,
#                Nsealion = 0, Cmax_SL = 15, alpha_SL = alpha, gamma_SL = gamma, Y_SL = Y,
#                F_catch = F_catch, E = E, M = natural_mort)
