# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)

fish.long <- read.csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/Ballard%20Locks%20Fish%20Counts/Ballard%20Locks%20Summarize%20all%20species%20all%20years.csv")

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

