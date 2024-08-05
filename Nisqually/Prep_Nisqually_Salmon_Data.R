# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)
library(lubridate)

fish.wide <- read.csv("Data/Nisqually/Nisqually_Chinook_and_Chum_July2024.csv")

## CHUM ----
chum_start <- min(which(fish.wide$Chum_per > 0)) - dates_buffer
chum_end <- max(which(fish.wide$Chum_per > 0)) + dates_buffer

Chum <- data.frame(fish.wide[chum_start:chum_end, c("Date", "DayofYear", "Chum_per")])
Chum$DailyEst <- Chum$Chum_per * chum.run.size
Chum$DailyEst_int <- floor(Chum$DailyEst)

# plot(Chum$DayofYear, Chum$DailyEst)
#looks kinda normal enough for a rough estimate I think

params <- c(90196.94, 68.76944, 14.56296)
fish.fit.optim.chum <- optim(par = params,
                                fn = fit.to.fish,
                                data = Chum$DailyEst_int,
                                method = "BFGS")
fish.fit.optim.chum <- optim(par = fish.fit.optim.chum$par,
                             fn = fit.to.fish,
                             data = Chum$DailyEst_int,
                             method = "BFGS")
chum_params <- fish.fit.optim.chum$par

# plot(Chum$DayofYear, Chum$DailyEst)
# lines(chum_start:chum_end, predictFish(chum_params, day = chum_start:chum_end, start.day = chum_start))
# looks ok to me!

# plot(chum_start:chum_end, predictNewFish(chum_params, day = chum_start:chum_end, chum_start))


## GREEN RIVER CHINOOK ----

gr_start <- min(which(fish.wide$GreenRiver_per > 0)) - dates_buffer
gr_end <- max(which(fish.wide$GreenRiver_per > 0)) + dates_buffer

GR_Chinook <- data.frame(fish.wide[gr_start:gr_end, 
                                c("Date", "DayofYear", "GreenRiver_per")])
GR_Chinook$DailyEst <- GR_Chinook$GreenRiver_per * gr.run.size
GR_Chinook$DailyEst_int <- floor(GR_Chinook$GreenRiver_per * gr.run.size)

params <- c(178398.40890, 73.54254, 15.21292)
fish.fit.optim <- function(params, fn, data, method = "BFGS") {
  old.val <- 1
  delta_val <- 1
  while(delta_val > 0){
    output <- optim(par = params,
                               fn = fn,
                               data = data,
                               method = method)
    delta_val <- output$value - old.val
    old.val <- output$value
  }
  return(output)
}

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

LocNis_Chinook <- data.frame(fish.wide[locnis_start:locnis_end, 
                                       c("Date", "DayofYear", "LocNis_per_corr")])
LocNis_Chinook$DailyEst <- LocNis_Chinook$LocNis_per_corr * ln.run.size




