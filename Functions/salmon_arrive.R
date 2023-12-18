# Fish arrival function
# make it more generic to handle multiple species and run timings

fish <- read.csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/Ballard%20Locks%20Fish%20Counts/Ballard%20Locks%20Summarize%20all%20species%20all%20years.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

fish$Year <- as.factor(fish$Year)
Daily_avg <- fish %>%
  group_by(DayofYear) %>%
  summarize(avg = mean(Count, na.rm = T))
Daily_avg$avg[is.nan(Daily_avg$avg)] <- 0

salmon_arrive <- function(day) {
  return(Daily_avg$avg[day])
}

# salmon_arrive(300)

# plot.daily.avg <- ggplot(data = Daily_avg) + 
#   geom_line(aes(x = DayofYear, y = avg))
# plot.daily.avg
# 
# plot.yearly.cts <- ggplot(data = fish) + 
#   geom_line(aes(x = DayofYear, y = Count, color = Species, lty = Year))
# plot.yearly.cts
