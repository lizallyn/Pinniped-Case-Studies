# function that returns the arriving salmon of each species for a given day of the year
# !!!!If you add steelhead data make sure to fix the data stream reference below!!!!

salmonSpeciesUpdate <- function(day, sockeye = 0, chinook = 0, coho = 0, chum = 0, pink = 0, steelhead = 0, data) {
  Chinook_day <- chinook + (data %>% slice(day) %>% pull(AvgChinook)) - (data %>% slice(day-1) %>% pull(AvgChinook))
  Sockeye_day <- sockeye + (data %>% slice(day) %>% pull(AvgSockeye)) - (data %>% slice(day-1) %>% pull(AvgSockeye))
  Coho_day <- coho + (data %>% slice(day) %>% pull(AvgCoho)) - (data %>% slice(day-1) %>% pull(AvgCoho))
  Chum_day <- chum + (data %>% slice(day) %>% pull(AvgChum)) - (data %>% slice(day-1) %>% pull(AvgChum))
  Pink_day <- pink + (data %>% slice(day) %>% pull(AvgPink)) - (data %>% slice(day-1) %>% pull(AvgPink))
  Steelhead_day <- steelhead + (data %>% slice(day) %>% pull(AvgSteelhead)) - (data %>% slice(day-1) %>% pull(AvgSteelhead))
  
 return(data.frame(Chinook = Chinook_day,
             Sockeye = Sockeye_day,
             Coho = Coho_day,
             Chum = Chum_day,
             Pink = Pink_day,
             Steelhead = Steelhead_day, row.names = c("count"))
 )
}
salmonSpeciesUpdate(12, sockeye = 1, data = Arrive_fish)
