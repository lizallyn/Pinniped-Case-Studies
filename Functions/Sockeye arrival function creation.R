### Salmon arrival function creation
### March 29,2023
### Updated: June 22, 2023

#### Version 2 with 2012-2022 data

# install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(PNWColors)

## read in data
sockeye <- read.csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/Ballard%20Daily%20Counts%202012-2022.csv")
# sockeye <- read.csv("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped Case Studies/Data/Ballard Daily Counts 2012-2022.csv")
# sockeye <- read_csv(fetchGHdata("Pinniped-Case-Studies", "Data/BallardDailyCounts2012-2022.csv"))

DayofStudy <- rep(163:275, 11)
sockeye <- data.frame(cbind(sockeye, DayofStudy))
sockeye$Daily.Count <- as.numeric(sockeye$Daily.Count)
sockeye$Y_DoS <- paste(sockeye$Year, sockeye$DayofStudy)

Avg.Daily <- sockeye %>%
  group_by(DayofStudy) %>%
  summarise(avg.daily = round(digits = 0, x = mean(Daily.Count)))

# plot the daily Sockeye counts for each year, avg in black
plot1 <- 
  ggplot(sockeye) +
  geom_point(aes(x = DayofStudy, y = Daily.Count), 
             col = pnw_palette(name = "Sailboat", n = nrow(sockeye), type = "continuous")) +
  geom_point(data = Avg.Daily, aes(x = DayofStudy, y = avg.daily), col = "black")
plot1 

# create a function to fit a curve to the data
fit.to.fish <- function(params, data) {
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- rep(NA, length(data))
  days <- 1:length(data)
  for(i in days) {
    y.hat[i] <- dnorm(x = i, mean = mean, sd = sd) * expand
  }
  nll <- -sum(dpois(x=data, lambda=y.hat, log=TRUE))
  return(nll)
}
params <- c(73500, 23.6, 14)
fit.to.fish(params = params, data = Avg.Daily$avg.daily)

fish.fit.optim <- optim(par = params,
                        fn = fit.to.fish,
                        data = Avg.Daily$avg.daily,
                        method = "BFGS")

# create function to predict the number of fish given the day of the year
# start.day is the date offset in the data (data starts on June 12 = 163)
predict.fish <- function(params, day, start.day) {
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean+start.day, sd = sd) * expand
  return(y.hat)
}

# predict.fish(params, 1, 163)

y.hat <- rep(NA, 365)
days <- 1:365
for(i in days) {
  y.hat[i] <- predict.fish(params = fish.fit.optim$par, day = i, start.day = 163)
}
test <- data.frame(cbind(1:365, y.hat))

plot2 <- 
  ggplot() +
  geom_point(aes(x = 163:275, y = Avg.Daily$avg.daily), col = "orchid3") +
  geom_line(aes(x = test$V1, y = test$y.hat), col = "dodgerblue")
plot2 

# needs a higher peak to really fit right?? JK optim was just being stubborn, looks good now
