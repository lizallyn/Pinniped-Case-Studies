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

fit.to.fish.beta <- function(params, data) {
  mean <- params[1]
  beta <- params[2]
  expand <- params[3]
  alpha_f <- (-beta*mean)/(mean-1)
  y.hat <- rep(NA, length(data))
  days <- 1:length(data)
  for(i in days) {
    y.hat[i] <- dbeta(x = i, shape1 = alpha_f + 1, shape2 = beta + 1, ncp = 0) * expand
  }
  nll <- -sum(dpois(x=data, lambda=y.hat, log=TRUE))
  return(nll)
}

fish.fit.optim <- function(params, fn = fit.to.fish, data, method = "BFGS") {
  old.val <- 1
  delta_val <- 1
  while(delta_val > 0){
    output <- optim(par = params,
                    fn = fn,
                    data = data,
                    method = method)
    delta_val <- output$value - old.val
    old.val <- output$value
  }
  return(output)
}

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
