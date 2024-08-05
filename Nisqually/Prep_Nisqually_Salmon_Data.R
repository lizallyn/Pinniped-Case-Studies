# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)
library(lubridate)

fish.wide <- read.csv("Data/Nisqually/Nisqually_Chinook_and_Chum_July2024.csv")

## CHUM ----
chum_start <- min(which(fish.wide$Chum_per > 0)) - dates_buffer
chum_end <- max(which(fish.wide$Chum_per > 0)) + dates_buffer

Chum <- data.frame(fish.wide[chum_start:chum_end, c("Date", "DayofYear", "Chum_per")])
Chum$DailyEst <- Chum$Chum_per * run.size
Chum$DailyEst_int <- floor(Chum$DailyEst)

plot(Chum$DayofYear, Chum$DailyEst)
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

plot(Chum$DayofYear, Chum$DailyEst)
lines(chum_start:chum_end, predictFish(chum_params, day = chum_start:chum_end, start.day = chum_start))
# looks ok to me!

plot(chum_start:chum_end, predictNewFish(chum_params, day = chum_start:chum_end, chum_start))


## CHINOOK ----

Chinook <- data.frame(fish.wide[fish.wide$GreenRiver_per + fish.wide$LocNis_per > 0, 
                                c("Date", "DayofYear", "GreenRiver_per", "LocNis_per")])






Daily_fish <- fish %>% 
  group_by(DayofYear) %>%
  summarize(AvgChinook = mean(Chinook, na.rm = T),
            AvgSockeye = mean(Sockeye, na.rm = T),
            AvgCoho = mean(Coho, na.rm = T))
Daily_fish$AvgChinook[is.nan(Daily_fish$AvgChinook)] <- 0
Daily_fish$AvgSockeye[is.nan(Daily_fish$AvgSockeye)] <- 0
Daily_fish$AvgCoho[is.nan(Daily_fish$AvgCoho)] <- 0
Daily_fish$AvgChum <- 0
Daily_fish$AvgPink <- 0
Daily_fish$AvgSteelhead <- 0
Daily_fish$total <- rowSums(Daily_fish[,-1])

dates_buffer <- 20
data_start <- which(Daily_fish$total > 0)[1] - dates_buffer
data_end <- which(Daily_fish$total > 0)[length(which(Daily_fish$total > 0))] + dates_buffer
Chosen_fish <- Daily_fish[data_start:data_end,]
Chosen_fish_int <- round(Chosen_fish)

# plot(Chosen_fish_int$AvgCoho)



