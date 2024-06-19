## Set Up Parameters ----

library(lubridate)

# loop parameters
dates_buffer <- 10
start_loop <- which(Daily_fish$total > 0)[1] - dates_buffer
end_loop <- which(Daily_fish$total > 0)[length(which(Daily_fish$total > 0))] + dates_buffer
days <- 365
day_range <- start_loop:end_loop

# seal parameters
num_seals <- 20
prop_specialists <- 0.1
num_specialists <- round(num_seals * prop_specialists)

# seal consumption parameters
deltat <- 1/24
alpha <- 0.05 
Cmax <- 5 # this was initially made up but actually makes some sense
gamma <- 0.5
Y <- 0 # this freaks out when I make it > 0

# seal learning parameters
specialist_prob <- 0.5
baseline_x_val <- 0
baseline_y_val <- 0
specialist_baseline_y <- 0
w <- 0.5
ymin <- -10
ymax <- 0
xmin <- -0.1
xmax <- 10
steepness <- 1
threshold_val <- -5
threshold_specialist <- -10
intercept_x_val <- 0.01
slope_x_val <- (1 - intercept_x_val)/(xmax + abs(xmin))
steepness_x_specialist <- 0.1
threshold_x_specialist <- 0.1
step <- 0.25
decay <- 0.15
buffer_Pymin_val <- 0
buffer_Pymin_specialist <- 0.5
buffer_Pxmin_specialist <- 0

# seal social learning parameters
num_seals_2_copy <- 0
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

# salmon parameters
sockeye_escape_rate <- 0.3
chinook_escape_rate <- 0.015
coho_escape_rate <- 0.1
natural_mort <- 0.0005

coho_fish_rate <- 0.1 # see "estFishingRate.R"
chinook_fish_rate <- 0.01
sockeye_fish_rate <- 0.01
salmon_days <- which(Daily_fish$total > 0)

# hunting parameters
zone_efficiency <- NA
steepness_H <- 10 # how quick does it saturate (higher = slower)
efficiency <- 0.075 # what prop of seals are they capable of taking
min_fishers <- 13
max_fishers <- 25
fishery_open <- yday(as.Date("2023-09-10"))
fishery_close <- yday(as.Date("2023-11-17"))
fishery_range <- fishery_open:fishery_close
boat_days <- fishery_range[which(fishery_range %in% day_range)]



