## function to calculate derivative expressions.
# From Tim Feb 2024

get_dXdt <- function(Cmax, Nseal, alpha, Ns, gamma, Y, F, M, E) {
  dNdt <- -Cmax *alpha * Ns * Nseal^(1 + gamma) / (Cmax + alpha * Ns * Nseal^gamma + Y) - F * Ns - M * Ns - E * Ns
  dCdt <-   Cmax * Nseal *(alpha * Ns * Nseal^gamma + Y) / (Cmax + alpha * Ns * Nseal^gamma + Y)
  dCatchdt <- F * Ns
  dEdt <- E * Ns
  return(c(dNdt, dCdt, dCatchdt, dEdt))
}