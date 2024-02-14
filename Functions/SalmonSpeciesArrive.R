# function that returns the arriving salmon of each species for a given day of the year

library(tidyr)

fish.long <- read.csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/Ballard%20Locks%20Fish%20Counts/Ballard%20Locks%20Summarize%20all%20species%20all%20years.csv")

# fish.long$Year <- as.factor(fish$Year)
# fish.long$Residence[fish$Species == "Sockeye"] <- 3
# fish.long$Residence[fish$Species == "Chinook"] <- 30
# fish.long$Residence[fish$Species == "Coho"] <- 10
# fish.long$Escape.Rate <- 1/fish$Residence

fish <- spread(fish.long, Species, Count)

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

SalmonSpeciesArrive <- function(day) {
  Chinook_arrive <- Daily_fish %>% slice(day) %>% pull(AvgChinook)
  Sockeye_arrive <- Daily_fish %>% slice(day) %>% pull(AvgSockeye)
  Coho_arrive <- Daily_fish %>% slice(day) %>% pull(AvgCoho)
  return(c(Chinook = Chinook_arrive, 
           Sockeye = Sockeye_arrive, 
           Coho = Coho_arrive))
}

SalmonSpeciesUpdate <- function(day, sockeye = 0, chinook = 0, coho = 0, chum = 0, pink = 0, steelhead = 0, data) {
  Chinook_day <- chinook + (data %>% slice(day) %>% pull(AvgChinook))
  Sockeye_day <- sockeye + (data %>% slice(day) %>% pull(AvgSockeye))
  Coho_day <- coho + (data %>% slice(day) %>% pull(AvgCoho))
  Chum_day <- chum + (data %>% slice(day) %>% pull(AvgChum))
  Pink_day <- pink + (data %>% slice(day) %>% pull(AvgPink))
  Steelhead_day <- pink + (data %>% slice(day) %>% pull(AvgSteelhead))
}

