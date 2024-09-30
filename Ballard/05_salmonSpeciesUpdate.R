# function that returns the arriving salmon of each species for a given day of the year
# !!!!If you add steelhead data make sure to fix the data stream reference below!!!!

salmonSpeciesUpdate <- function(day, sockeye = 0, chinook = 0, coho = 0, data) {
  Chinook_day <- chinook + (data %>% slice(day) %>% pull(Chinook))
  Sockeye_day <- sockeye + (data %>% slice(day) %>% pull(Sockeye))
  Coho_day <- coho + (data %>% slice(day) %>% pull(Coho))

 return(data.frame(Chinook = Chinook_day,
             Sockeye = Sockeye_day,
             Coho = Coho_day, 
             row.names = c("count"))
 )
}

