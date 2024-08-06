# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)
library(lubridate)

source("~/GitHub/PinnipedCaseStudies/Nisqually/predictFish.R")

fish.wide <- read.csv("Data/Nisqually/Nisqually_Chinook_and_Chum_July2024.csv")

## CHUM ----
chum_start <- min(which(fish.wide$Chum_per > 0)) - dates_buffer
chum_end <- max(which(fish.wide$Chum_per > 0)) + dates_buffer
chum_residence <- 21

Chum <- data.frame(fish.wide[chum_start:chum_end, c("Date", "DayofYear", "Chum_per")])
Chum$DailyEst <- Chum$Chum_per * chum.run.avg
Chum$DailyEst_int <- floor(Chum$DailyEst)

# plot(Chum$DayofYear, Chum$DailyEst)
#looks kinda normal enough for a rough estimate I think

params <- c(194999, 68.769, 14.57)
fish.fit.optim.chum <- fish.fit.optim(params, fit.to.fish, Chum$DailyEst_int)
fish.fit.optim.chum
chum_params <- fish.fit.optim.chum$par

# plot(Chum$DayofYear, Chum$DailyEst)
# lines(chum_start:chum_end, predictFish(chum_params, day = chum_start:chum_end, start.day = chum_start))
# # looks ok to me!
# 
# plot(chum_start:chum_end, predictNewFish(chum_params, day = chum_start:chum_end, chum_start))

Daily_Chum <- data.frame(DayofYear = chum_start:chum_end, 
                         Chum = predictNewFish(chum_params, day = chum_start:chum_end, chum_start))

## GREEN RIVER CHINOOK ----

gr_start <- min(which(fish.wide$GreenRiver_per > 0)) - dates_buffer
gr_end <- max(which(fish.wide$GreenRiver_per > 0)) + dates_buffer

gr_residence <- 14

GR_Chinook <- data.frame(fish.wide[gr_start:gr_end, 
                                c("Date", "DayofYear", "GreenRiver_per")])
GR_Chinook$DailyEst <- GR_Chinook$GreenRiver_per * gr.run.avg
GR_Chinook$DailyEst_int <- floor(GR_Chinook$DailyEst)

params <- c(173431.97006, 73.53288, 15.18786)
fish.fit.optim.gr <- fish.fit.optim(params, fit.to.fish, GR_Chinook$DailyEst_int)
gr_params <- fish.fit.optim.gr$par

# plot(GR_Chinook$DayofYear, GR_Chinook$DailyEst)
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
LocNis_Chinook$DailyEst <- (LocNis_Chinook$LocNis_per_corr * ln.run.avg)
LocNis_Chinook$DailyEst_int <- floor(LocNis_Chinook$DailyEst)

params <- c(5104.17195, 84.91841, 26.77771)
fish.fit.optim.ln <- fish.fit.optim(params, fit.to.fish, LocNis_Chinook$DailyEst_int)
fish.fit.optim.ln
ln_params <- fish.fit.optim.ln$par

# plot(LocNis_Chinook$DayofYear, LocNis_Chinook$DailyEst)
# lines(locnis_start:locnis_end, predictFish(ln_params, day = locnis_start:locnis_end, start.day = locnis_start))
# # ehh looks not great but maybe workable for now.
# 
# plot(locnis_start:locnis_end, predictNewFish(ln_params, day = locnis_start:locnis_end, start.day = locnis_start))

Daily_Chinook <- data.frame(DayofYear = min(locnis_start, gr_start):max(locnis_end, gr_end), 
                            GR_Chinook = predictNewFish(gr_params, day = gr_start:gr_end, start.day = gr_start),
                            LN_Chinook = predictNewFish(ln_params, day = locnis_start:locnis_end, start.day = locnis_start))
