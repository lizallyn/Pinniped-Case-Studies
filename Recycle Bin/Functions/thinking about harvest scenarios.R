# thinking about how to code up harvest scenarios
# Feb 2024

## Exclusion Zone

if(gauntlet_salmon[t, j] > 0) {
  H[t, j] <- length(seals_at_gauntlet) * zone_efficiency_rate
} else {H[t, j] <- 0}


## Fishers Take as Observed

# if like consumption:
H[t, j] <- (Hmax * handling_time * num_gillnetters^(1+gamma) * num_gauntlet_seals)/
  (Hmax + handling_time * num_gauntlet_seals * num_gillnetters^gamma + Y)

Hmax <- 2
handling_time <- 0.05
num_gillnetters <- 10
gamma <- 0
num_gauntlet_seals <- 20
Y <- 0

(Hmax * handling_time * num_gillnetters^(1+gamma) * num_gauntlet_seals)/
  (Hmax + handling_time * num_gauntlet_seals * num_gillnetters^gamma + Y)
