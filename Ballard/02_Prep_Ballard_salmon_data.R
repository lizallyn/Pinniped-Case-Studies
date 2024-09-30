# Script to prep salmon data for SalmonSpeciesArrive function calls
library(tidyr)
library(dplyr)

fish.long <- read.csv("Data/BallardLocks/Ballard_Locks_Summarize_all_species_all_years.csv")

fish <- spread(fish.long, Species, Count)

colnames(fish) <- c("DayofYear", "Date", "Year", "Comment", "Chinook", "Coho", "Sockeye")

Daily_fish <- fish %>% 
  group_by(DayofYear) %>%
  summarize(AvgChinook = mean(Chinook, na.rm = T),
            AvgSockeye = mean(Sockeye, na.rm = T),
            AvgCoho = mean(Coho, na.rm = T))
Daily_fish$AvgChinook[is.nan(Daily_fish$AvgChinook)] <- 0
Daily_fish$AvgSockeye[is.nan(Daily_fish$AvgSockeye)] <- 0
Daily_fish$AvgCoho[is.nan(Daily_fish$AvgCoho)] <- 0
Daily_fish$AvgChum <- 0
Daily_fish$AvgPink <- 0
Daily_fish$AvgSteelhead <- 0
Daily_fish$total <- rowSums(Daily_fish[,-1])

dates_buffer <- 20
data_start <- which(Daily_fish$total > 0)[1] - dates_buffer
data_end <- which(Daily_fish$total > 0)[length(which(Daily_fish$total > 0))] + dates_buffer
Chosen_fish <- Daily_fish[data_start:data_end,]
Chosen_fish_int <- round(Chosen_fish)

# plot(Chosen_fish_int$AvgCoho)

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

# params <- c(39000, 46, 12)
# fit.to.fish(params = params, data = Chosen_fish_int$AvgSockeye)
params <- c(69000, 43.13, 12.48)
fish.fit.optim.sockeye <- optim(par = params,
                        fn = fit.to.fish,
                        data = Chosen_fish_int$AvgSockeye,
                        method = "BFGS")
sockeye_params <- fish.fit.optim.sockeye$par

params <- c(9365.43, 86, 13.8)
fish.fit.optim.chinook <- optim(par = params,
                        fn = fit.to.fish,
                        data = Chosen_fish_int$AvgChinook,
                        method = "BFGS")
chinook_params <- fish.fit.optim.chinook$par

params <- c(17225, 115.26, 7.8)
fish.fit.optim.coho <- optim(par = params,
                                fn = fit.to.fish,
                                data = Chosen_fish_int$AvgCoho,
                                method = "BFGS")
coho_params <- fish.fit.optim.coho$par

predict_fish <- function(params, day, start.day = data_start) {
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean+start.day, sd = sd) * expand
  return(y.hat)
}

predict_new_fish <- function(params, day, start.day = data_start){
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean+start.day, sd = sd) * expand
  y.hat_t.1 <- dnorm(x = day-1, mean = mean+start.day, sd = sd) * expand
  new.fish <- y.hat - y.hat_t.1
  new.fish[ new.fish < 0] <- 0
  return(new.fish)
}


# plot(predict_new_fish(sockeye_params, 130:290, data_start))
# lines(predict_new_fish(coho_params, 130:290, data_start))
# lines(predict_new_fish(chinook_params, 130:290, data_start))



### Recycle Bin

# Daily_fish_offset <- rbind(rep(0, 8), Daily_fish[1:(nrow(Daily_fish)-1),])
# Daily_fish_offset[,1] <- 1:366
# 
# Arrive_fish <- Daily_fish - Daily_fish_offset
# Arrive_fish[ Arrive_fish < 0 ] <- 0

### PIT data - Chinook from Bear and Cedar
# start_date <- yday("2023-05-12")
# end_date <- yday("2023-06-28")
# beta_2 <- 100
# mean_2 <- yday(x = "2023-06-22")
# alpha_2 <- (-beta_2*mean_2)/(mean_2-1)
# dpois(x = 173, lambda = mean_2)
# curve(dbeta(x,4,2),xlim=c(0, 1))
# x_list <- seq(0, 1, 1/(length(start_date:end_date)-1))
# dbeta(x = x_list, 4, 2)
# max_smolt <- 464/7
# scale <- max_smolt/max(dbeta(x = x_list, 4, 2))
# 
# smolt_passage <- data.frame(dayofyear = start_date:end_date, smolt = dbeta(x = x_list, 4, 2) * scale)
# plot(smolt_passage)
