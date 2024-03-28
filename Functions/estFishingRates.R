### Estimate fishing rates

source("~/Github/PinnipedCaseStudies/Functions/Prep_data_for_Salmon_functions.R")
catch.data <- read.csv("~/Github/PinnipedCaseStudies/Data/BallardLocks/FisheryCatchNumbers2023ShipCanal.csv")

merged <- merge(Daily_fish, catch.data)

# Coho
merged$avg.coho.catch.rate <- merged$CohoCatch/merged$AvgCoho
merged$avg.coho.catch.rate[which(is.infinite(merged$avg.coho.catch.rate))] <- 0
merged$avg.coho.catch.rate[which(is.nan(merged$avg.coho.catch.rate))] <- 0
mean(merged$avg.coho.catch.rate, na.rm = T)
# 0.239
plot(merged$avg.coho.catch.rate)

# Chinook
merged$avg.chinook.catch.rate <- merged$ChinookCatch/merged$AvgChinook
merged$avg.chinook.catch.rate[which(is.infinite(merged$avg.chinook.catch.rate))] <- 0
merged$avg.chinook.catch.rate[which(is.nan(merged$avg.chinook.catch.rate))] <- 0
mean(merged$avg.chinook.catch.rate, na.rm = T)
