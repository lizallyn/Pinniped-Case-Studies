## Set Up Variables ----

oneDzeroes <- makeArray(days, start.val = 0, names = "Day")
twoDzeroes <- makeArray(c(num_seals, days), start.val = 0, names = c("Seal", "Day"))

### Individual Values ----
salmon_consumed <- twoDzeroes
seal_prob_gauntlet <- twoDzeroes
seal_forage_loc <- twoDzeroes

x <- twoDzeroes
y <- twoDzeroes
C <- twoDzeroes
P_x <- twoDzeroes
P_y <- twoDzeroes

P_social <- twoDzeroes

kill_list <- c()

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

### Variable Rates ----
coho_catch_rate <- oneDzeroes
coho_catch_rate[boat_days] <- coho_fish_rate
chinook_catch_rate <- oneDzeroes
chinook_catch_rate[boat_days] <- chinook_fish_rate
sockeye_catch_rate <- oneDzeroes
sockeye_catch_rate[boat_days] <- sockeye_fish_rate

Y_smolts <- oneDzeroes
Y_smolts[91:244] <- Y_smolts_est #????
# 0.005 = 5 smolts if 1 adult = 1000 smolts
# 0.01 = 5 smolts if 1 adult = 500 smolts
# 0.3 produces an uptick in C when only smolts present
# Chasco 2017 Table 4
# April 1 - Sept 1 right now

harvest_plan <- createHarvestPlan(scenario = "Boat", days = days, years = years, 
                                  boat_days = boat_days, salmon_days = salmon_days)

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


