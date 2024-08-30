# function that returns the arriving salmon of each species for a given day of the year

salmonSpeciesUpdate <- function(day, gr_chinook = 0, ln_chinook = 0, data) {
  Chinook_GR <- gr_chinook + (data %>% slice(day) %>% pull(GR_Chinook))
  Chinook_LN <- ln_chinook + (data %>% slice(day) %>% pull(LN_Chinook))
  #
 return(data.frame(Chinook_GR = Chinook_GR,
                   Chinook_LN = Chinook_LN,
                   row.names = c("count"))
 )
}
# salmonSpeciesUpdate(12, sockeye = 1, data = Arrive_fish)
