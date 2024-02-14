# function that returns the arriving salmon of each species for a given day of the year

salmonSpeciesUpdate <- function(day, sockeye = 0, chinook = 0, coho = 0, chum = 0, pink = 0, steelhead = 0, data) {
  Chinook_day <- chinook + (data %>% slice(day) %>% pull(AvgChinook))
  Sockeye_day <- sockeye + (data %>% slice(day) %>% pull(AvgSockeye))
  Coho_day <- coho + (data %>% slice(day) %>% pull(AvgCoho))
  Chum_day <- chum + (data %>% slice(day) %>% pull(AvgChum))
  Pink_day <- pink + (data %>% slice(day) %>% pull(AvgPink))
  Steelhead_day <- pink + (data %>% slice(day) %>% pull(AvgSteelhead))
  return(data.frame(Chinook = Chinook_day,
           Sockeye = Sockeye_day,
           Coho = Coho_day,
           Chum = Chum_day,
           Pink = Pink_day,
           Steelhead = Steelhead_day))
}


hello