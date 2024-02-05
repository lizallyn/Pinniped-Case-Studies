# thinking about how to code up harvest scenarios
# Feb 2024

## Exclusion Zone

H[t, j] <- length(seals_at_gauntlet) * zone_efficiency_rate


## Fishers Take as Observed

# if like consumption:
Hmax <- 2
handling_time <- 0.05
num_gillnetters <- 10
gamma <- 0
num_gauntlet_seals <- 20
Y <- 0

H <- (Hmax * handling_time * num_gillnetters^(1+gamma) * num_gauntlet_seals)/
  (Hmax + handling_time * num_gauntlet_seals * num_gillnetters^gamma + Y)
H
seals_harvested <- sample(seals_at_gauntlet, 0)
seal_prob_gauntlet[seals_harvested, t, j] <- NA
