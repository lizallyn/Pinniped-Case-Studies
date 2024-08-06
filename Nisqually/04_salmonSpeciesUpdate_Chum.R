# function that returns the arriving salmon of each species for a given day of the year

salmonSpeciesUpdate <- function(day, chum = 0, data) {
  Chum <- chum + (data %>% slice(day) %>% pull(Chum))
  #
 return(data.frame(Chinook_GR = Chinook_GR,
                   row.names = c("count"))
 )
}
# salmonSpeciesUpdate(12, sockeye = 1, data = Arrive_fish)
