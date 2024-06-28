## Set Up Parameters ----

library(lubridate)

# loop parameters
start_loop <- data_start
end_loop <- data_end
day_range <- start_loop:end_loop
days <- length(day_range)


# seal parameters
num_seals <- 50
prop_specialists <- 0.1

# seal consumption parameters
deltat_val <- 1/24
alpha <- 0.05 
Cmax <- 5 # this was initially made up but actually makes some sense
gamma <- -1 # pred dep, this expects something between -1, 0
Y <- 0 # this freaks out when I make it > 0

# seal learning parameters
specialist_prob <- 0.5
baseline_x_val <- 0
baseline_y_val <- 0
specialist_baseline_y <- 0
w <- 0.5
ymin <- -10
ymax <- 0
intercept_x_val <- 0.01
xmax <- 10

steepness <- 1
threshold_val <- -5
threshold_specialist <- -10

steepness_x_specialist <- 0.1
threshold_x_specialist <- 0.1
step <- 0.5
decay <- 0.5
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

# hunting parameters (same as fishery opening in base run realm)
min_fishers <- 13
max_fishers <- 25
fishery_open <- yday(as.Date("2023-09-10"))
fishery_close <- yday(as.Date("2023-11-17"))

zone_efficiency <- 0.8
steepness_H <- NA # how quick does it saturate (higher = slower)
efficiency <- NA # what prop of seals are they capable of taking
min_harvesters <- NA
max_harvesters <- NA
harvest_open <- start_loop
harvest_close <- end_loop
# start and end of effort/season open
harvest_range <- harvest_open:harvest_close
# create vector of no-effort days (weekends)
weekends <- c(seq(from = harvest_open, to = harvest_close, by = 7), seq(from = harvest_open+1, to = harvest_close, by = 7))
# remove "weekends"
harvest_range <- harvest_range[! harvest_range %in% weekends]
# days as index not dayofyear (start_loop is day 1 even if not January 1)
harvest_days <- harvest_range[harvest_range %in% day_range] - (start_loop - 1)
scenario <- "Zone"




