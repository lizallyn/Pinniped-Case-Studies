# make sure instantaneous mortality is working

predation_rate <- 0.6
escape_rate <- 0.3
catch_rate <- 0.3

# calculate salmon inst mortality
inst_predation <- predation_rate / (predation_rate + catch_rate + escape_rate) *
  (1 - exp(-predation_rate - catch_rate - escape_rate))
inst_catch_rate <- catch_rate / (predation_rate + catch_rate + escape_rate) *
  (1 - exp(-predation_rate - catch_rate - escape_rate))
inst_escape <- escape_rate / (predation_rate + catch_rate + escape_rate) *
  (1 - exp(-predation_rate - catch_rate - escape_rate))
gauntlet_salmon[t, j] <- gauntlet_salmon[t, j] * exp(-inst_predation - inst_catch_rate - inst_escape)