## Set Up Parameters ----

# loop parameters
days <- 365

# seal parameters
num_seals <- 10

# seal consumption parameters
alpha <- 0.1
Cmax <- 5
gamma <- 0
Y <- 0

# seal learning parameters
w <- 1
ymin <- -10
ymax <- 0
xmin <- -1
xmax <- 9
steepness <- 1
threshold <- -5
slope_x <- 0.1
intercept_x <- 0.1
step <- 0.25
decay <- 0.1
buffer_Pymin <- 0.1 # Need to rememmber why this felt necessary?

# seal social learning parameters
num_seals_2_copy <- 3
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

# salmon parameters
sockeye_escape_rate <- 0.3
chinook_escape_rate <- 0.03
coho_escape_rate <- 0.1
natural_mort <- 0.0005

# hunting parameters
zone_efficiency <- 0.8
steepness_H <- 5
efficiency <- 0.5
min_fishers <- 13
max_fishers <- 25
coho_fish_rate <- 0.1
salmon_days <- which(Daily_fish$total > 0)

