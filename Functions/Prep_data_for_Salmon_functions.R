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
Daily_fish$AvgChum <- 0
Daily_fish$AvgPink <- 0
Daily_fish$AvgSteelhead <- 0
Daily_fish$total <- rowSums(Daily_fish[,-1])

Daily_fish_offset <- rbind(rep(0, 8), Daily_fish[1:(nrow(Daily_fish)-1),])
Daily_fish_offset[,1] <- 1:366

Arrive_fish <- Daily_fish - Daily_fish_offset
Arrive_fish[ Arrive_fish < 0 ] <- 0

### PIT data - Chinook from Bear and Cedar
# start_date <- yday("2023-05-12")
# end_date <- yday("2023-06-28")
# beta_2 <- 100
# mean_2 <- yday(x = "2023-06-22")
# alpha_2 <- (-beta_2*mean_2)/(mean_2-1)
# dpois(x = 173, lambda = mean_2)
# curve(dbeta(x,4,2),xlim=c(0, 1))
# x_list <- seq(0, 1, 1/(length(start_date:end_date)-1))
# dbeta(x = x_list, 4, 2)
# max_smolt <- 464/7
# scale <- max_smolt/max(dbeta(x = x_list, 4, 2))
# 
# smolt_passage <- data.frame(dayofyear = start_date:end_date, smolt = dbeta(x = x_list, 4, 2) * scale)
# plot(smolt_passage)
