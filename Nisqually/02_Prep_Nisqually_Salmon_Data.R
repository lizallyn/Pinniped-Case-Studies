# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)
library(anytime)
library(lubridate)

# source("Nisqually/01_predictFish.R")
# Not necessary unless running this script as a standalone

fish.wide <- read.csv("Data/Nisqually/Adjusted_Nisqually_Data_from_Craig.csv")

dates_buffer <- 20

fish.wide$Dates <- anydate(fish.wide$Dates)
fish.wide$DayofYear <- yday(fish.wide$Dates)
fish.wide$DayofYear[fish.wide$DayofYear<100] <- fish.wide$DayofYear[fish.wide$DayofYear<100] + 366


check_dates_row <- fish.wide[dates_buffer, c("GR", "LocNis", "Chum")]
if(any(check_dates_row > 0)){
  add_blanks <- data.frame(matrix(data = 0, nrow = dates_buffer, ncol = ncol(fish.wide)))
  colnames(add_blanks) <- colnames(fish.wide)
  add_blanks$DayofYear <- (fish.wide$DayofYear[1] - dates_buffer):(fish.wide$DayofYear[1] - 1)
  fish.wide <- rbind(add_blanks, fish.wide)
}

## CHUM ----
chum_start <- min(fish.wide$DayofYear[which(fish.wide$Chum > 0)]) - dates_buffer
chum_end <- max(fish.wide$DayofYear[which(fish.wide$Chum > 0)]) + dates_buffer
chum_residence <- 21

WChum <- data.frame(fish.wide[fish.wide$DayofYear %in% chum_start:chum_end, c("Dates", "Chum", "DayofYear")])
WChum$DailyEst <- WChum$Chum
WChum$DailyEst_int <- round(WChum$DailyEst)

# plot(WChum$DayofYear, WChum$DailyEst)
#looks kinda normal enough for a rough estimate I think

params <- c(25800, 54, 12)
fish.fit.optim.chum <- fish.fit.optim(params = params, fn = fit.to.fish, data = WChum$DailyEst_int)
fish.fit.optim.chum
chum_params <- fish.fit.optim.chum$par

# # check fit!
# plot(WChum$DayofYear, WChum$DailyEst)
# lines(chum_start:chum_end, predictFish(chum_params, day = chum_start:chum_end, start.day = chum_start))
# # looks ok to me!

Daily_Chum <- data.frame(DayofYear = chum_start:chum_end, 
                         Chum = round(predictFish(chum_params, day = chum_start:chum_end, chum_start)))

## GREEN RIVER CHINOOK ----

gr_start <- min(fish.wide$DayofYear[which(fish.wide$GR > 0)]) - dates_buffer
gr_end <- max(fish.wide$DayofYear[which(fish.wide$GR > 0)]) + dates_buffer

gr_residence <- 14

GRiver_Chinook <- data.frame(fish.wide[fish.wide$DayofYear %in% gr_start:gr_end, 
                                c("Dates", "DayofYear", "GR")])
GRiver_Chinook$DailyEst <- GRiver_Chinook$GR
GRiver_Chinook$DailyEst_int <- round(GRiver_Chinook$DailyEst)

params <- c(20900, 73, 15)
fish.fit.optim.gr <- fish.fit.optim(params, fit.to.fish, GRiver_Chinook$DailyEst_int)
fish.fit.optim.gr
gr_params <- fish.fit.optim.gr$par

# # check fit!
# plot(GRiver_Chinook$DayofYear, GRiver_Chinook$DailyEst)
# lines(gr_start:gr_end, predictFish(gr_params, day = gr_start:gr_end, start.day = gr_start))
# # looks good!


## LocNis CHINOOK ----

locnis_start <- min(fish.wide$DayofYear[which(fish.wide$LocNis > 0)]) - dates_buffer
locnis_end <- max(fish.wide$DayofYear[which(fish.wide$LocNis > 0)]) + dates_buffer

locnis_residence <- 7

LocNis_Chinook <- data.frame(fish.wide[fish.wide$DayofYear %in% locnis_start:locnis_end, 
                                       c("Dates", "DayofYear", "LocNis")])
LocNis_Chinook$DailyEst <- LocNis_Chinook$LocNis
LocNis_Chinook$DailyEst_int <- round(LocNis_Chinook$DailyEst)

params <- c(804, 93, 27)
fish.fit.optim.ln <- fish.fit.optim(params, fit.to.fish, LocNis_Chinook$DailyEst_int)
fish.fit.optim.ln
ln_params <- fish.fit.optim.ln$par

# plot(LocNis_Chinook$DayofYear, LocNis_Chinook$DailyEst)
# lines(locnis_start:locnis_end, predictFish(ln_params, day = locnis_start:locnis_end, start.day = locnis_start))
# # ehh looks not great but maybe workable for now.


chinook_start <- min(locnis_start, gr_start)
chinook_end <- max(locnis_end, gr_end)
chinook_days <- chinook_start:chinook_end


# All Runs

fish_start <- min(chum_start, chinook_start)
fish_end <- max(chum_end, chinook_end)
fish_days <- fish_start:fish_end

Daily_Fish <- data.frame(DayofYear = fish_days,
                         Chum = round(predictFish(chum_params, day = fish_days, chum_start)),
                         GR_Chinook = round(predictFish(gr_params, day = fish_days, start.day = gr_start)),
                         LN_Chinook = round(predictFish(ln_params, day = fish_days, start.day = locnis_start)))
Daily_Fish$Total <- Daily_Fish$Chum + Daily_Fish$GR_Chinook + Daily_Fish$LN_Chinook


### Fishing Rates ----
# from catch data from Craig

Daily_Fish$GR_catch <- fish.wide$GR_catch[fish.wide$DayofYear %in% Daily_Fish$DayofYear]
Daily_Fish$LocNis_catch <- fish.wide$LocNis_catch[fish.wide$DayofYear %in% Daily_Fish$DayofYear]
Daily_Fish$Chum_catch <- fish.wide$Chum_catch[fish.wide$DayofYear %in% Daily_Fish$DayofYear]

# days that boats are on the water for hunting purposes (any day that catch is landed)
# estimated boats on water per day from Craig
Daily_Fish$harvesters <- 0
min_chum_boats <- 1
max_chum_boats <- 25
chin_boats <- 14
Daily_Fish$harvesters[(Daily_Fish$GR_catch + Daily_Fish$LocNis_catch) > 0] <- chin_boats
num_harvesters <- sample(min_chum_boats:max_chum_boats, length(Daily_Fish$harvesters[Daily_Fish$Chum_catch > 0]), replace = TRUE)
Daily_Fish$harvesters[Daily_Fish$Chum_catch > 0] <- num_harvesters


