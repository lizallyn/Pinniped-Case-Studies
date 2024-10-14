# Prep sea lion counts
# From scott's data

library(tidyr)
library(dplyr)
library(anytime)
library(lubridate)

counts <- read.csv("Data/BallardLocks/Shilshole_sealion_data.csv")

counts$Date <- anydate(counts$Date)
counts$yday <- yday(counts$Date)

plot(counts$yday, counts$Max_Daily)

plot(1:365, dnbinom(x = 1:365, size = 5, mu = 50)*100)

plot(1:365, dnorm(1:365, 50, 50))

# stopped working on this because it's kind of irrelevant - 
# most of the bigger counts all happen oputside the model run period
# Just using counts to estimate that I should have ~5 Zc in source pool and ~1 in gauntlet daily
