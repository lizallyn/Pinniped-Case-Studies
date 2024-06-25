### Salmon arrival function creation
### March 29,2023
### Updated: June 22, 2023

#### Version 2 with 2012-2022 data

# install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(PNWColors)

## read in data
sockeye <- read.csv("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Data/BallardDailyCounts2012-2022.csv")
# sockeye <- read.csv("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped Case Studies/Data/Ballard Daily Counts 2012-2022.csv")
# sockeye <- read_csv(fetchGHdata("Pinniped-Case-Studies", "Data/BallardDailyCounts2012-2022.csv"))

DayofStudy <- rep(163:275, 11)
sockeye <- data.frame(cbind(sockeye, DayofStudy))
sockeye$DailyCount <- as.numeric(sockeye$DailyCount)
sockeye$Y_DoS <- paste(sockeye$Year, sockeye$DayofStudy)

Avg.Daily <- sockeye %>%
  group_by(DayofStudy) %>%
  summarise(avgdaily = round(digits = 0, x = mean(DailyCount)))

# plot the daily Sockeye counts for each year, avg in black
# plot1 <- 
#   ggplot(sockeye) +
#   geom_point(aes(x = DayofStudy, y = DailyCount), 
#              col = pnw_palette(name = "Sailboat", n = nrow(sockeye), type = "continuous")) +
#   geom_point(data = Avg.Daily, aes(x = DayofStudy, y = avgdaily), col = "black")
# plot1 

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
params <- c(73500, 23.62, 13.985)
# fit.to.fish(params = params, data = Avg.Daily$avgdaily)

fish.fit.optim <- optim(par = params,
                        fn = fit.to.fish,
                        data = Avg.Daily$avgdaily,
                        method = "BFGS")
# fish.fit.optim$par

# create function to predict the number of fish given the day of the year
# start.day is the date offset in the data (data starts on June 12 = 163)
predict_fish <- function(params, day, start.day) {
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean+start.day, sd = sd) * expand
  return(y.hat)
}

predict_fish(params, 155, 163)

# y.hat <- rep(NA, 365)
# days <- 1:365
# for(i in days) {
#   y.hat[i] <- predict.fish(params = fish.fit.optim$par, day = i, start.day = 163)
# }
# test <- data.frame(cbind(1:365, y.hat))
# normal <- (1/fish.fit.optim$par[3]*sqrt(2*pi)) * exp(-0.5*((days-(fish.fit.optim$par[2] + 163))/fish.fit.optim$par[3])^2)

# plot2 <- 
#   ggplot() +
#   geom_point(aes(x = 163:275, y = Avg.Daily$avgdaily), col = "orchid3") +
#   geom_line(aes(x = test$V1, y = test$y.hat), col = "dodgerblue", lwd = 1) + 
#   geom_line(aes(x = days, y = normal), col = "turquoise3")
# plot2 

# Optim is super stubborn with this, make sure it's right before moving on
