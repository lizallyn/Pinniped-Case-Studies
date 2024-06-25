# function that returns the arriving salmon of each species for a given day of the year
# !!!!If you add steelhead data make sure to fix the data stream reference below!!!!

salmonSpeciesUpdate <- function(day, sockeye = 0, chinook = 0, coho = 0, chum = 0, pink = 0, steelhead = 0, data) {
  Chinook_day <- chinook + (data %>% slice(day) %>% pull(Chinook))
  Sockeye_day <- sockeye + (data %>% slice(day) %>% pull(Sockeye))
  Coho_day <- coho + (data %>% slice(day) %>% pull(Coho)) 
  # Chum_day <- chum + (data %>% slice(day) %>% pull(Chum))
  # Pink_day <- pink + (data %>% slice(day) %>% pull(Pink)) 
  # Steelhead_day <- steelhead + (data %>% slice(day) %>% pull(Steelhead))
  #
 return(data.frame(Chinook = Chinook_day,
             Sockeye = Sockeye_day,
             Coho = Coho_day,
             # Chum = Chum_day,
             # Pink = Pink_day,
             # Steelhead = Steelhead_day, 
             row.names = c("count"))
 )
}
# salmonSpeciesUpdate(12, sockeye = 1, data = Arrive_fish)
