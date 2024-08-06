# function that returns the arriving salmon of each species for a given day of the year

salmonSpeciesUpdate <- function(day, chinook_gr = 0, chinook_ln = 0, data) {
  Chinook_GR <- chinook_gr + (data %>% slice(day) %>% pull(GR_Chinook))
  Chinook_LN <- chinook_ln + (data %>% slice(day) %>% pull(LocNis_Chinook))
  #
 return(data.frame(Chinook_GR = Chinook_GR,
                   Chinook_LN = Chinook_LN,
                   row.names = c("count"))
 )
}
# salmonSpeciesUpdate(12, sockeye = 1, data = Arrive_fish)
