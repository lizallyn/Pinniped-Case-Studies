# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)
library(lubridate)

source("Nisqually/00_predictFish.R")

fish.wide <- read.csv("Data/Nisqually/Adjusted_Nisqually_Data_from_Craig.csv")

fish.wide$DayofYear <- yday(fish.wide$Dates)
fish.wide$DayofYear[fish.wide$DayofYear<100] <- fish.wide$DayofYear[fish.wide$DayofYear<100] + 365

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

plot(WChum$DayofYear, WChum$DailyEst)
#looks kinda normal enough for a rough estimate I think

params <- c(25800, 68, 15)
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
GRiver_Chinook$DailyEst <- (GRiver_Chinook$GreenRiver_per * gr.run.avg)/7
GRiver_Chinook$DailyEst_int <- round(GRiver_Chinook$DailyEst)

params <- c(22000, 73, 15)
fish.fit.optim.gr <- fish.fit.optim(params, fit.to.fish, GRiver_Chinook$DailyEst_int)
fish.fit.optim.gr
gr_params <- fish.fit.optim.gr$par

# # check fit!
# plot(GRiver_Chinook$DayofYear, GRiver_Chinook$DailyEst)
# lines(gr_start:gr_end, predictFish(gr_params, day = gr_start:gr_end, start.day = gr_start))
# # looks good!
# 
# plot(gr_start:gr_end, predictNewFish(gr_params, day = gr_start:gr_end, start.day = gr_start))


## LocNis CHINOOK ----

locnis_start <- min(which(fish.wide$LocNis_per_corr > 0)) - dates_buffer
locnis_end <- max(which(fish.wide$LocNis_per_corr > 0)) + dates_buffer

locnis_residence <- 7

LocNis_Chinook <- data.frame(fish.wide[locnis_start:locnis_end, 
                                       c("Date", "DayofYear", "LocNis_per_corr")])
LocNis_Chinook$DailyEst <- (LocNis_Chinook$LocNis_per_corr * ln.run.avg)/7
LocNis_Chinook$DailyEst_int <- round(LocNis_Chinook$DailyEst)

params <- c(645.8, 86, 27)
fish.fit.optim.ln <- fish.fit.optim(params, fit.to.fish, LocNis_Chinook$DailyEst_int)
fish.fit.optim.ln
ln_params <- fish.fit.optim.ln$par

# plot(LocNis_Chinook$DayofYear, LocNis_Chinook$DailyEst)
# lines(locnis_start:locnis_end, predictFish(ln_params, day = locnis_start:locnis_end, start.day = locnis_start))
# # ehh looks not great but maybe workable for now.

# plot(locnis_start:locnis_end, predictNewFish(ln_params, day = locnis_start:locnis_end, start.day = locnis_start))

chinook_start <- min(locnis_start, gr_start)
chinook_end <- max(locnis_end, gr_end)
chinook_days <- chinook_start:chinook_end
Daily_Chinook <- data.frame(DayofYear = chinook_days, 
                            GR_Chinook = round(predictNewFish(gr_params, day = chinook_days, start.day = gr_start)),
                            LN_Chinook = round(predictNewFish(ln_params, day = chinook_days, start.day = locnis_start)))
Daily_Chinook$Total <- Daily_Chinook$GR_Chinook + Daily_Chinook$LN_Chinook

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
# see "Nisqually_Fishery_Data_from_Craig.xlsx" for process

catch <- read.csv("Data/Nisqually/Summarized_Nisqually_Fishery_Data_from_Craig.csv")
catch$Dates <- as.Date(catch$Day, format = "%j", origin = "1.1.2024")
# add days before/after fisheries
if(fish_start<catch$Day[1]){
  add_days <- fish_start:catch$Day[1]
  add_dates <- as.Date(add_days, format = "%j", origin = "1.1.2024")
  add_start <- data.frame(Week = rep(NA, length(add_days)), 
                          Dates = add_dates,
                          Day = add_days)
  add_start <- cbind(add_start, matrix(data = 0, nrow = length(add_days), ncol = (ncol(catch)-3)))
}
colnames(add_start) <- colnames(catch)
if(fish_end>catch$Day[nrow(catch)]){
  add_days <- catch$Day[nrow(catch)]:fish_end
  add_dates <- as.Date(add_days, format = "%j", origin = "1.1.2024")
  add_end <- data.frame(Week = rep(NA, length(add_days)), 
                          Dates = add_dates,
                          Day = add_days)
  add_end <- cbind(add_end, matrix(data = 0, nrow = length(add_days), ncol = (ncol(catch)-3)))
}
colnames(add_end) <- colnames(catch)
catch <- rbind(add_start, catch, add_end)
