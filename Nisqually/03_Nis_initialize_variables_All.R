## Initialize variables for Nisqually Chum model

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
fishery_range_chum <- fishery_open_chum:fishery_close_chum
fishery_range_chinook <- fishery_open_chinook:fishery_close_chinook
fishery_days_chum <- fishery_range_chum[which(fishery_range_chum %in% day_range)] - (start_loop - 1)
fishery_days_chinook <- fishery_range_chinook[which(fishery_range_chinook %in% day_range)] - (start_loop - 1)



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
escape_chum <- oneDzeroes
escape_gr <- oneDzeroes
escape_ln <- oneDzeroes
gauntlet_chum <- oneDzeroes
gauntlet_gr <- oneDzeroes
gauntlet_ln <- oneDzeroes
gauntlet_salmon <- oneDzeroes
fished_chum <- oneDzeroes
fished_gr <- oneDzeroes
fished_ln <- oneDzeroes
eaten_chum <- oneDzeroes
eaten_gr <- oneDzeroes
eaten_ln <- oneDzeroes

consumed_total <- oneDzeroes

H <- oneDzeroes
H_zc <- oneDzeroes
H_ej <- oneDzeroes

### Variable Rates ----
chum_catch_rate <- Daily_Fish$Chum_rate
gr_catch_rate <- Daily_Fish$GR_rate
ln_catch_rate <- Daily_Fish$LocNis_rate

