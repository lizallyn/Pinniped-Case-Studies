# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)
library(lubridate)

source("~/GitHub/PinnipedCaseStudies/Nisqually/00_predictFish.R")

fish.wide <- read.csv("Data/Nisqually/Nisqually_Chinook_and_Chum_July2024.csv")

## CHUM ----
chum_start <- min(which(fish.wide$Chum_per > 0)) - dates_buffer
chum_end <- max(which(fish.wide$Chum_per > 0)) + dates_buffer
chum_residence <- 21

WChum <- data.frame(fish.wide[chum_start:chum_end, c("Date", "DayofYear", "Chum_per")])
WChum$DailyEst <- (WChum$Chum_per * chum.run.avg)/7
WChum$DailyEst_int <- round(WChum$DailyEst)

# plot(Chum$DayofYear, Chum$DailyEst)
#looks kinda normal enough for a rough estimate I think

params <- c(27800, 68.769, 14.59)
fish.fit.optim.chum <- fish.fit.optim(params, fit.to.fish, WChum$DailyEst_int)
fish.fit.optim.chum
chum_params <- fish.fit.optim.chum$par

# check fit!
# plot(WChum$DayofYear, WChum$DailyEst)
# lines(chum_start:chum_end, predictFish(chum_params, day = chum_start:chum_end, start.day = chum_start))
# # looks ok to me!
# 
# plot(chum_start:chum_end, predictNewFish(chum_params, day = chum_start:chum_end, chum_start))

Daily_Chum <- data.frame(DayofYear = chum_start:chum_end, 
                         Chum = round(predictFish(chum_params, day = chum_start:chum_end, chum_start)))

## GREEN RIVER CHINOOK ----

gr_start <- min(which(fish.wide$GreenRiver_per > 0)) - dates_buffer
gr_end <- max(which(fish.wide$GreenRiver_per > 0)) + dates_buffer

gr_residence <- 14

GRiver_Chinook <- data.frame(fish.wide[gr_start:gr_end, 
                                c("Date", "DayofYear", "GreenRiver_per")])
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
