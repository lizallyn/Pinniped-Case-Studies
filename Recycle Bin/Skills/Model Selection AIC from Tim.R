# From tim email October 9, 2024

dNLL <- seq(1, 3, length.out = 100) # difference in NLL between complex and simple model
pvalues <- 1 - pchisq(2*dNLL, df = 1) # assume 1 additional parameter in complex model
DAIC <- 2 * dNLL - 2 # assume 1 additional parameter in complex model
plot(DAIC, pvalues,type = "l", lwd = 2)
abline(v = 0.66, lwd = 2, lty = "dashed")