## Set Up Variables ----

### Parameters that are derived from other parameters----

num_specialists <- round(num_seals * prop_specialists)

slope_x_val <- (1 - intercept_x_val)/(xmax - baseline_x_val)
xmin <- (0 - intercept_x_val)/slope_x_val

bundle_dx_pars <- tibble(step = step, xmin = xmin, xmax = xmax, decay = decay)
bundle_dy_pars <- tibble(step = step, ymin = ymin, ymax = ymax, decay = decay)
bundle_x_shape_pars <- tibble(buffer = buffer_Pxmin_specialist, steepness = steepness, 
                              threshold = threshold_x_specialist)
bundle_x_linear_pars <- tibble(slope = slope_x_val, intercept = intercept_x_val)

bundle_x_shape_pars_sl <- tibble(buffer = buffer_Pxmin_specialist, steepness = steepness, 
                                 threshold = threshold_x_specialist)
bundle_y_shape_pars_sl <- tibble(buffer = buffer_Pymin_specialist, steepness = steepness, 
                                 threshold = threshold_specialist)

### Blank arrays----

oneDzeroes <- makeArray(days, start.val = 0, names = "Day")
twoDzeroes <- makeArray(c(num_seals, days), start.val = 0, names = c("Seal", "Day"))
twoDzeroes_zc <- makeArray(c(num_zc, days), start.val = 0, names = c("CSL", "Day"))
twoDzeroes_ej <- makeArray(c(num_ej, days), start.val = 0, names = c("SSL", "Day"))

### Create Salmon Data----

The_Fish <- data.frame(DayofYear = start_loop:end_loop)
The_Fish$Sockeye <- floor(predict_new_fish(sockeye_params, start_loop:end_loop))
The_Fish$Chinook <- floor(predict_new_fish(chinook_params, start_loop:end_loop))
The_Fish$Coho <- floor(predict_new_fish(coho_params, start_loop:end_loop))

### Individual Values ----
salmon_consumed_pv <- twoDzeroes
salmon_consumed_zc <- twoDzeroes_zc
salmon_consumed_ej <- twoDzeroes_ej

seal_prob_gauntlet <- twoDzeroes
zc_prob_gauntlet <- twoDzeroes_zc
ej_prob_gauntlet <- twoDzeroes_ej

if(num_specialists == 0){
  specialist_seals <- NA
} else {
  specialist_seals <- sample(1:num_seals, num_specialists)
}
seal_forage_loc <- twoDzeroes
zc_forage_loc <- twoDzeroes_zc
ej_forage_loc <- twoDzeroes_ej

baseline_x <- makeArray(num_seals, start.val = baseline_x_val, names = "Seal")
baseline_y <- makeArray(num_seals, start.val = baseline_y_val, names = "Seal")
baseline_y[specialist_seals] <- specialist_baseline_y

seals_at_gauntlet_save <- list(rep(NA, days))
zc_at_gauntlet_save <- list(rep(NA, days))
ej_at_gauntlet_save <- list(rep(NA, days))

x <- twoDzeroes
y <- twoDzeroes
C <- twoDzeroes
P_x <- twoDzeroes
P_y <- twoDzeroes

x_zc <- twoDzeroes_zc
y_zc <- twoDzeroes_zc
C_zc <- twoDzeroes_zc
P_x_zc <- twoDzeroes_zc
P_y_zc <- twoDzeroes_zc

x_ej <- twoDzeroes_ej
y_ej <- twoDzeroes_ej
C_ej <- twoDzeroes_ej
P_x_ej <- twoDzeroes_ej
P_y_ej <- twoDzeroes_ej

buffer_Pymin <- makeArray(num_seals, start.val = buffer_Pymin_val, names = "Seal")
buffer_Pymin[specialist_seals] <- buffer_Pymin_specialist

threshold <- makeArray(num_seals, start.val = threshold_val, names = "Seal")
threshold[specialist_seals] <- threshold_specialist

P_social <- twoDzeroes
P_social_zc <- twoDzeroes_zc
P_social_ej <- twoDzeroes_ej

kill_list <- list()
kill_list_zc <- list()
kill_list_ej <- list()

# harvest
fishery_range <- fishery_open:fishery_close
fishery_days <- fishery_range[which(fishery_range %in% day_range)] - (start_loop - 1)

harvest_plan_pv <- createHarvestPlan(scenario = scenario, 
                                  harvest_days = harvest_days_pv,
                                  empty.array = oneDzeroes)
harvest_plan_ej <- createHarvestPlan(scenario = scenario, 
                                     harvest_days = harvest_days_ej,
                                     empty.array = oneDzeroes)
harvest_plan_zc <- createHarvestPlan(scenario = scenario, 
                                     harvest_days = harvest_days_zc,
                                     empty.array = oneDzeroes)

### Actual States that I Need ----
escape_chinook <- oneDzeroes
escape_sockeye <- oneDzeroes
escape_coho <- oneDzeroes

gauntlet_chinook <- oneDzeroes
gauntlet_sockeye <- oneDzeroes
gauntlet_coho <- oneDzeroes

fished_chinook <- oneDzeroes
fished_sockeye <- oneDzeroes
fished_coho <- oneDzeroes

H <- oneDzeroes
H_zc <- oneDzeroes
H_ej <- oneDzeroes

### Variable Rates ----
coho_catch_rate <- oneDzeroes
coho_catch_rate[fishery_days] <- coho_fish_rate
chinook_catch_rate <- oneDzeroes
chinook_catch_rate[fishery_days] <- chinook_fish_rate
sockeye_catch_rate <- oneDzeroes
sockeye_catch_rate[fishery_days] <- sockeye_fish_rate



### Troubleshooting & BS ----
# for that one plot
gauntlet_salmon <- oneDzeroes
# troubleshooting ghost salmons
screwy <- c(species = NA, day = NA, gauntlet_t = NA, Ns = NA, C = NA, Catch = NA, E = NA)

# If you're using these it means you didn't clear out variables between runs!!!
eaten_chinook <- oneDzeroes
eaten_sockeye <- oneDzeroes
eaten_coho <- oneDzeroes
consumed_total <- oneDzeroes


