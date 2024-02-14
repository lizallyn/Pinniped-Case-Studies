# function that returns the arriving salmon of each species for a given day of the year

salmonSpeciesUpdate <- function(day, sockeye = 0, chinook = 0, coho = 0, chum = 0, pink = 0, steelhead = 0, data) {
  Chinook_day <- chinook + (data %>% slice(day) %>% pull(AvgChinook))
  Sockeye_day <- sockeye + (data %>% slice(day) %>% pull(AvgSockeye))
  Coho_day <- coho + (data %>% slice(day) %>% pull(AvgCoho))
  Chum_day <- chum + (data %>% slice(day) %>% pull(AvgChum))
  Pink_day <- pink + (data %>% slice(day) %>% pull(AvgPink))
  Steelhead_day <- pink + (data %>% slice(day) %>% pull(AvgSteelhead))
  total <- sum(data %>% slice(day))
  
  Sockeye_prop <- Sockeye_day/total
  Chinook_prop <- Chinook_day/total
  Coho_prop <- Coho_day/total
  Chum_prop <- Chum_day/total
  Pink_prop <- Pink_day/total
  Steelhead_prop <- Steelhead_day/total
  
 return(data.frame(Chinook = c(Chinook_day, Chinook_prop),
             Sockeye = c(Sockeye_day, Sockeye_prop),
             Coho = c(Coho_day, Coho_prop),
             Chum = c(Chum_day, Chum_prop),
             Pink = c(Pink_day, Pink_prop),
             Steelhead = c(Steelhead_day, Steelhead_prop), row.names = c("count", "prop"))
 )
  
}

