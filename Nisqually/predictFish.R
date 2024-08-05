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

predictFish <- function(params, day, start.day = data_start) {
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean+start.day, sd = sd) * expand
  return(y.hat)
}

predictNewFish <- function(params, day, start.day = data_start){
  expand <- params[1]
  mean <- params[2]
  sd <- params[3]
  y.hat <- dnorm(x = day, mean = mean+start.day, sd = sd) * expand
  y.hat_t.1 <- dnorm(x = day-1, mean = mean+start.day, sd = sd) * expand
  new.fish <- y.hat - y.hat_t.1
  new.fish[ new.fish < 0] <- 0
  return(new.fish)
}
