# Fish arrival function
# make it more generic to handle multiple species and run timings

fish <- read.csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/Ballard%20Locks%20Fish%20Counts/Ballard%20Locks%20Summarize%20all%20species%20all%20years.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

fish$Year <- as.factor(fish$Year)
fish$Residence[fish$Species == "Sockeye"] <- 3
fish$Residence[fish$Species == "Chinook"] <- 30
fish$Residence[fish$Species == "Coho"] <- 10
fish$Escape.Rate <- 1/fish$Residence

Daily_fish <- fish %>%
  group_by(Species,DayofYear) %>%
  summarize(avg = mean(Count, na.rm = T),
            avg.escape.rate = mean(Escape.Rate))
Daily_fish$avg[is.nan(Daily_fish$avg)] <- 0

salmon_arrive <- function(day) {
  return(Daily_fish[which(Daily_fish$DayofYear == day),])
}
                       
salmon_escape_rate <- function(day) {
  avg.escape <- sum(salmon_arrive(day)$avg.escape.rate * 
                      (salmon_arrive(day)$avg/sum(salmon_arrive(day)$avg)))
  if(is.nan(avg.escape)){
    avg.escape <- 0
  }
  return(avg.escape)
}
