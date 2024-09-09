# Set Parameters
# after data 

# loop parameters
start_loop <- fish_start
end_loop <- fish_end
day_range <- start_loop:end_loop
days <- length(day_range)

# seal parameters
num_seals <- 150
prop_specialists <- 0.1

# sea lions
num_zc <- 80
num_ej <- 10

# seal consumption parameters
deltat_val <- 1/24
alpha <- 0.05 
Cmax <- 5 # this was initially made up but actually makes some sense
gamma <- -1 # pred dep, this expects something between -1 and 0

# sea lion consumption
Cmax_zc <- 20
Cmax_ej <- 20

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

# steepness_x_specialist <- 0.1
threshold_x_specialist <- 0.1
step <- 0.5
decay <- 0.5
buffer_Pymin_val <- 0
buffer_Pymin_specialist <- 0.5
buffer_Pxmin_specialist <- 0

# social learning parameters
num_seals_2_copy <- 25 # general haulout size? total guess rn
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

# salmon parameters
chum_escape_rate <- 1/chum_residence
gr_escape_rate <- 1/gr_residence
ln_escape_rate <- 1/locnis_residence
natural_mort <- 0.0005

sealion_arrival_buffer <- 10 # days arrival before chum arrive

# hunting parameters (same as fishery opening in base run realm)

zone_efficiency <- NA
steepness_H <- 200 # how quick does it saturate (higher = slower)
efficiency <- 0.075 # what prop of seals are they capable of taking

scenario <- "Boat"


