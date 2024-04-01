## Set Up Parameters ----

library(lubridate)

# loop parameters
days <- 365

# seal parameters
num_seals <- 20

# seal consumption parameters
alpha <- 0.1 
Cmax <- 1 # this was initially made up but actually makes some sense
gamma <- 0
Y <- 0 # this freaks out when I make it > 0

# seal learning parameters
w <- 0.2
# v <- 0.1
ymin <- -10
ymax <- 0
xmin <- -1
xmax <- 9
steepness <- 1
threshold <- -5
slope_x <- 0.1
intercept_x <- 0.1
step <- 0.25
decay <- 0.05
buffer_Pymin <- 0

# seal social learning parameters
num_seals_2_copy <- 2
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

# salmon parameters
sockeye_escape_rate <- 0.3
chinook_escape_rate <- 0.03
coho_escape_rate <- 0.1
natural_mort <- 0.0005

# hunting parameters
zone_efficiency <- 0.8
steepness_H <- 10 # how quick does it saturate (higher = slower)
efficiency <- 0.05 # what prop of seals are they capable of taking
min_fishers <- 13
max_fishers <- 25
fishery_open <- yday(as.Date("2023-09-10"))
fishery_close <- yday(as.Date("2023-11-17"))
boat_days <- fishery_open:fishery_close

coho_fish_rate <- 0.1 # see "estFishingRate.R"
chinook_fish_rate <- 0.01
sockeye_fish_rate <- 0.01
salmon_days <- which(Daily_fish$total > 0)

