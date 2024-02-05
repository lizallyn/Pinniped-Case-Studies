# thinking about how to code up harvest scenarios
# Feb 2024

## Exclusion Zone

if(gauntlet_salmon[t, j] > 0) {
  H[t, j] <- length(seals_at_gauntlet) * zone_efficiency_rate
} else {H[t, j] <- 0}


## Fishers Take as Observed

