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

# Doing again Oct 2024 bc I don't remember doing this?

source("Ballard/02_Prep_Ballard_salmon_data.R")

gauntlet_fish <- data.frame(Sockeye = gauntlet_sockeye, Chinook = gauntlet_chinook, 
                            Coho = gauntlet_coho)
combo <- cbind(Chosen_catch, gauntlet_fish)

combo$sockeye_rate <- combo$SockeyeCatch/combo$Sockeye
combo$chinook_rate <- combo$ChinookCatch/combo$Chinook
combo$coho_rate <- combo$CohoCatch/combo$Coho

mean(combo$coho_rate, na.rm = T)
mean(combo$chinook_rate, na.rm = T)
