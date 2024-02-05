## function to calculate derivative expressions.
# From Tim Feb 2024

get_dXdt <- function(Cmax, Nseal, alpha, Ns, gamma, Y, F_catch, M, E) {
  dNdt <- -Cmax *alpha * Ns * Nseal^(1 + gamma) / (Cmax + alpha * Ns * Nseal^gamma + Y) - F_catch * Ns - M * Ns - E * Ns
  dCdt <-   Cmax * Nseal *(alpha * Ns * Nseal^gamma + Y) / (Cmax + alpha * Ns * Nseal^gamma + Y)
  dCatchdt <- F_catch * Ns
  dEdt <- E * Ns
  return(c(dNdt, dCdt, dCatchdt, dEdt))
}

