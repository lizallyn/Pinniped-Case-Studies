# apply 4th order runge-kutta to integrate over time period deltat
# From Tim Feb 2024

runge_kutta <- function(Cmax, Nseal, alpha, Ns, gamma, Y, F_catch, M, E, deltat = 1){
  X <- c(Ns, 0, 0, 0)
  K1s <- get_dXdt(Cmax, Nseal, alpha, Ns = X[1], gamma, Y, F_catch, M, E)
  midX <- X + deltat* 0.5 * K1s
  K2s <- get_dXdt(Cmax, Nseal, alpha, Ns= midX[1], gamma, Y, F_catch, M, E)
  midX <- X + deltat * 0.5 * K2s
  K3s <- get_dXdt(Cmax, Nseal, alpha, Ns= midX[1], gamma, Y, F_catch, M, E)
  endX <- X + deltat * K3s
  K4s <- get_dXdt(Cmax, Nseal, alpha, Ns= endX[1], gamma, Y, F_catch, M, E)
  Xsim <- X + deltat * (K1s / 6 + K2s / 3 + K3s / 3 + K4s / 6)
  return(c(Ns = Xsim[1],
           C = Xsim[2],
           Catch = Xsim[3],
           E = Xsim[4]))
}

