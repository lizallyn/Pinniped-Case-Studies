### Salmon arrival function creation
### March 29,2023

#### Version 2 with 2012-2022 data

# install.packages("PNWColors")
library(dplyr)
library(ggplot2)
library(PNWColors)

## read in data
sockeye <- read.csv("/Users/lizallyn/Documents/GitHub/Thesis/Pinniped Case Studies/Data/Ballard Daily Counts 2012-2022.csv")

DayofStudy <- rep(1:113, 11)
sockeye <- data.frame(cbind(sockeye, DayofStudy))
sockeye$DayofStudy <- as.factor(sockeye$DayofStudy)
sockeye$Daily.Count <- as.numeric(sockeye$Daily.Count)
sockeye$Y_DoS <- paste(sockeye$Year, sockeye$DayofStudy)

Avg.Daily <- sockeye %>%
  group_by(DayofStudy) %>%
  summarise(avg.daily = round(digits = 0, x = mean(Daily.Count)))

plot1 <- 
  ggplot(sockeye) +
  geom_point(aes(x = DayofStudy, y = Daily.Count, fill = Year), col = pnw_palette(name = "Sailboat", n = nrow(sockeye), type = "continuous")) +
  geom_point(data = Avg.Daily, aes(x = DayofStudy, y = avg.daily), col = "black")
plot1 

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
# fit.to.fish(params = params, data = Avg.Daily$avg.daily)

fish.fit.optim <- optim(par = params,
                        fn = fit.to.fish,
                        data = Avg.Daily$avg.daily,
                        method = "BFGS")

predict.fish <- function(params, day) {
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean, sd = sd) * expand
  return(y.hat)
}

y.hat <- rep(NA, nrow(Avg.Daily))
days <- 1:nrow(Avg.Daily)
for(i in days) {
  y.hat[i] <- predict.fish(params = fish.fit.optim$par, day = i)
}
test <- data.frame(cbind(1:nrow(Avg.Daily), y.hat))

plot2 <- 
  ggplot() +
  geom_point(aes(x = Avg.Daily$DayofStudy, y = Avg.Daily$avg.daily), col = "orchid3") +
  geom_point(aes(x = test$V1, y = test$y.hat), col = "dodgerblue")
plot2 

# needs a higher peak to really fit right?? JK optim was just being stubborn, looks good now
