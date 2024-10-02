# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)

fish.long <- read.csv("Data/BallardLocks/Ballard_Locks_Summarize_all_species_all_years.csv")

fish <- spread(fish.long, Species, Count)

colnames(fish) <- c("DayofYear", "Date", "Year", "Comment", "Chinook", "Coho", "Sockeye")

Daily_fish <- fish %>% 
  group_by(DayofYear) %>%
  summarize(AvgChinook = mean(Chinook, na.rm = T),
            AvgSockeye = mean(Sockeye, na.rm = T),
            AvgCoho = mean(Coho, na.rm = T))
Daily_fish$AvgChinook[is.nan(Daily_fish$AvgChinook)] <- 0
Daily_fish$AvgSockeye[is.nan(Daily_fish$AvgSockeye)] <- 0
Daily_fish$AvgCoho[is.nan(Daily_fish$AvgCoho)] <- 0
Daily_fish$total <- rowSums(Daily_fish[,-1])

dates_buffer <- 20
data_start <- which(Daily_fish$total > 0)[1] - dates_buffer
data_end <- which(Daily_fish$total > 0)[length(which(Daily_fish$total > 0))] + dates_buffer
Chosen_fish <- Daily_fish[data_start:data_end,]
Chosen_fish_int <- round(Chosen_fish)

#fit a curve to the data

params <- c(69000, 36, 12.48)
fish.fit.optim.sockeye <- fish.fit.optim(params = params, 
                                         fn = fit.to.fish, 
                                         data = Chosen_fish_int$AvgSockeye)
sockeye_params <- fish.fit.optim.sockeye$par

params <- c(9365.43, 99, 13.8)
fish.fit.optim.chinook <- fish.fit.optim(par = params,
                        fn = fit.to.fish,
                        data = Chosen_fish_int$AvgChinook)
chinook_params <- fish.fit.optim.chinook$par

params <- c(17225, 128.26, 7.8)
fish.fit.optim.coho <- fish.fit.optim(par = params,
                                fn = fit.to.fish,
                                data = Chosen_fish_int$AvgCoho)
coho_params <- fish.fit.optim.coho$par

# Check fit

# plot(Chosen_fish_int$DayofYear, Chosen_fish_int$AvgSockeye)
# lines(Chosen_fish_int$DayofYear, predictFish(sockeye_params, data_start:data_end, data_start))
# 
# plot(Chosen_fish_int$DayofYear, Chosen_fish_int$AvgChinook)
# lines(Chosen_fish_int$DayofYear, predictFish(chinook_params, data_start:data_end, data_start))
# 
# plot(Chosen_fish_int$DayofYear, Chosen_fish_int$AvgCoho)
# lines(Chosen_fish_int$DayofYear, predictFish(coho_params, data_start:data_end, data_start))

# plot(predict_new_fish(sockeye_params, 130:290, data_start))
# lines(predict_new_fish(coho_params, 130:290, data_start))
# lines(predict_new_fish(chinook_params, 130:290, data_start))

## call fishing data

catch <- read.csv("Data/BallardLocks/FisheryCatchNumbers2023ShipCanal.csv")

Chosen_catch <- catch[data_start:data_end,]
