# Social Seals version 9
# For separating out the salmon species
# February 2024

#### Set Up ####

## Load Packages and Data
library(ggplot2)

## Load Data Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Prep_data_for_Salmon_functions.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Prep_data_for_Harvest_functions.R")

## Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/makeArray.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/createHarvestPlan.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/salmonSpeciesUpdate.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/decideForagingDestination.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/collusion.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/learnX.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/learnY.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/get_dXdt.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/rungeKutta.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/getHarvested.R")

## Set Parameters

# loop parameters
# years <- 1
days <- 365

# seal parameters
num_seals <- 10
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0

# seal consumption parameters
alpha <- 1
Cmax <- 5
gamma <- 0
Y <- 0

# seal learning parameters
w <- 1
ymin <- -10
ymax <- 0
xmin <- -1
xmax <- 9
steepness <- 1
threshold <- -5
slope_x <- 0.1
intercept_x <- 0.1
step <- 0.25
decay <- 0.1
buffer_Pymin <- 0.1

# seal social learning parameters
num_seals_2_copy <- 4
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

# salmon parameters
sockeye_escape_rate <- 0.3
chinook_escape_rate <- 0.03
coho_escape_rate <- 0.1
natural_mort <- 0.0005

# fishing parameters
coho_catch_rate <- 0.3

# hunting parameters
zone_efficiency <- 0.8
processing_time <- 0.05
gamma_H <- 0
Y_H <- 0
min_fishers <- 13
max_fishers <- 25
salmon_days <- which(Daily_fish$total > 0)
harvest_max_perboat = 2

## Set Up Variables
oneDzeroes <- makeArray(days, start.val = 0, names = "Day")
twoDzeroes <- makeArray(c(num_seals, days), start.val = 0, names = c("Seal", "Day"))
# threeDzeroes <- makeArray(c(num_seals, days, years), start.val = 0, 
#                           names = c("Seal", "Day", "Year"))

# salmon rates and accounting
escape_chinook <- oneDzeroes
escape_sockeye <- oneDzeroes
escape_coho <- oneDzeroes
coho_catch_rate <- oneDzeroes
coho_catch_rate[boat_days] <- 0.3
chinook_catch_rate <- oneDzeroes
sockeye_catch_rate <- oneDzeroes
gauntlet_salmon <- oneDzeroes
gauntlet_chinook <- oneDzeroes
gauntlet_sockeye <- oneDzeroes
gauntlet_coho <- oneDzeroes

# seals
H <- oneDzeroes
salmon_consumed <- oneDzeroes
seal_prob_gauntlet <- twoDzeroes
seal_forage_loc <- twoDzeroes

# harvest matrix
harvest_plan <- createHarvestPlan(scenario = "Boat", days = days, years = years, boat_days = boat_days, salmon_days = salmon_days)

# Variables for x y learning bit
x <- twoDzeroes
y <- twoDzeroes
C <- twoDzeroes
B <- twoDzeroes
P_x <- twoDzeroes
P_y <- twoDzeroes

# for social learning
P_social <- twoDzeroes

#### Run time loop ####

  
for(t in 1:(days-1)) {
  
  # salmon arrive at the gauntlet
  daily_update <- salmonSpeciesUpdate(day = t, data = Daily_fish)
  gauntlet_chinook[t] <- daily_update %>% slice(1) %>% pull(Chinook)
  gauntlet_sockeye[t] <- daily_update %>% slice(1) %>% pull(Sockeye)
  gauntlet_coho[t] <- daily_update %>% slice(1) %>%  pull(Coho)
  gauntlet_salmon[t] <- sum(c(gauntlet_chinook[t], gauntlet_sockeye[t], gauntlet_coho[t]))
  
  # decide where each seal goes that day
  for(seal in 1:num_seals) {
    seal_forage_loc[seal,t] <- decideForagingDestination(seal_prob_gauntlet[seal,t])
  }
  
  # round of copying
  for(seal in 1:num_seals) {
    P_social[seal, t] <- collusion(probs_list = seal_prob_gauntlet[,t], 
                                      prob_gauntlet_of_seal = seal_prob_gauntlet[seal, t], 
                                      seals_2_copy = num_seals_2_copy, 
                                      mean = mean, beta = beta)
  }
  
  # calculate salmon mortality 
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  sockeye_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_sockeye[t], 
                               gamma = gamma, Y = Y, E = sockeye_escape_rate, 
                               F_catch = coho_catch_rate[t], M = natural_mort, deltat = 1)
  chinook_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_chinook[t], 
                               gamma = gamma, Y = Y, E = chinook_escape_rate, 
                               F_catch = coho_catch_rate[t], M = natural_mort, deltat = 1)
  coho_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_coho[t], 
                               gamma = gamma, Y = Y, E = coho_escape_rate, 
                               F_catch = coho_catch_rate[t], M = natural_mort, deltat = 1)
  if(any(c(sockeye_result, chinook_result, coho_result)) < 0) {
    consumed_chinook <- eat_some_fish(gauntlet_chinook[t], length(seals_at_gauntlet), 1, 5, 0, 0)
    consumed_sockeye <- eat_some_fish(gauntlet_chinook[t], length(seals_at_gauntlet), 1, 5, 0, 0)
    consumed_coho <- eat_some_fish(gauntlet_chinook[t], length(seals_at_gauntlet), 1, 5, 0, 0)
    
    
    
    escape_chinook[t] <- escapeRate(gauntlet_chinook[t], chinook_escape_rate)
    escape_sockeye[t] <- escapeRate(gauntlet_sockeye[t], sockeye_escape_rate)
    escape_coho[t] <- escapeRate(gauntlet_coho[t], coho_escape_rate)
  }
  
  # propagate to abundance in next time step for each species
  gauntlet_chinook[t+1] <- chinook_result["Ns"]
  gauntlet_sockeye[t+1] <- sockeye_result["Ns"]
  gauntlet_coho[t+1] <- coho_result["Ns"]
  # assign consumed salmon to seals at gauntlet
  salmon_consumed[seals_at_gauntlet, t] <- sum(c(sockeye_result["C"], chinook_result["C"], coho_result["C"]))/length(seals_at_gauntlet)
  # escape salmon
  escape_chinook[t+1] <- escape_chinook[t] + chinook_result["E"]
  escape_sockeye[t+1] <- escape_sockeye[t] + sockeye_result["E"]
  escape_coho[t+1] <- escape_coho[t] + coho_result["E"]
  
  # seal harvest
  H[t] <- getHarvested(day_plan = harvest_plan[t], num_gauntlet_seals = length(seals_at_gauntlet), 
                        zone_efficiency = zone_efficiency, Hmax = harvest_max_perboat, 
                        processing = processing_time, min_fishers = min_fishers, max_fishers = max_fishers, 
                        gamma = gamma_H, Y = Y_H)
  if(H[t] > 0){
    seal_prob_gauntlet[sample(seals_at_gauntlet, H[t]), t+1] <- NA
  }
  
  # calculate x, y and prob_gauntlet for next time step
  ## This could all become some functions
  for(seal in 1:num_seals){
    # calculate C
    C[seal, t] <- salmon_consumed[seal, t] - w
    
    # calculate d_x and d_y
    d_x <- learnX(food = C[seal, t], x_t = x[seal, t], 
                   forage_loc = seal_forage_loc[seal, t],  step = step, 
                   xmin = xmin, xmax = xmax, decay = decay)
    d_y <- learnY(hunting = H[t], y_t = y[seal, t], 
                   seal_forage_loc[seal, t], step = step, 
                   ymin = ymin, ymax = ymax, decay = decay)
    
    # update x and y and P_x and P_y
    x[seal, t+1] <- x[seal, t] + d_x
    P_x[seal, t+1] <- x[seal, t+1] * slope_x + intercept_x
    
    y[seal, t+1] <- y[seal, t] + d_y
    P_y[seal, t+1] <- 1-(1/((1+buffer_Pymin) + exp(-steepness * (threshold - y[seal, t+1]))))
    
    # calculate Prob gauntlet
    seal_prob_gauntlet[seal, t+1] <- P_x[seal, t+1] * P_y[seal, t+1]
  }
  
} # days loop


# Testing Space




# These only show the last year
par(mfrow = c(2,2))
plot(1:days, colSums(seal_forage_loc[,,j]), main = "Number of seals at the gauntlet")
plot(1:days, colMeans(seal_prob_gauntlet[,,j]), main = "avg. prob gauntlet")
plot(1:days, gauntlet_salmon[,j], main = "salmon at the gauntlet")
plot(1:days, colSums(salmon_consumed[,,j]), main = "salmon consumed")
plot(1:days, colMeans(C))

# with ggplot to show each seal individually

library(reshape2)

prob_gauntlet_plot <- melt(data = seal_prob_gauntlet[,,1], "Seal")
colnames(prob_gauntlet_plot) <- c("Seal", "Day", "Prob_G")
plot_probs <- ggplot(data = prob_gauntlet_plot, aes(x = Day, y = Prob_G, color = Seal)) + 
  geom_point()
plot_probs

C_plot <- melt(data = C[,,1], "Seal")
colnames(C_plot) <- c("Seal", "Day", "C")
plot_C <- ggplot(data = C_plot, aes(x = Day, y = C, color = Seal)) + 
  geom_point()
plot_C

x_plot <- melt(data = x[,,1], "Seal")
colnames(x_plot) <- c("Seal", "Day", "x")
plot_x <- ggplot(data = x_plot, aes(x = Day, y = x, color = Seal)) + 
  geom_point()
plot_x

Px_plot <- melt(data = P_x[,,1], "Seal")
colnames(Px_plot) <- c("Seal", "Day", "P_x")
plot_Px <- ggplot(data = Px_plot, aes(x = Day, y = P_x, color = Seal)) + 
  geom_point()
plot_Px

y_plot <- melt(data = y[,,1], "Seal")
colnames(y_plot) <- c("Seal", "Day", "y")
plot_y <- ggplot(data = y_plot, aes(x = Day, y = y, color = Seal)) + 
  geom_point()
plot_y

Py_plot <- melt(data = P_y[,,1], "Seal")
colnames(Py_plot) <- c("Seal", "Day", "P_y")
plot_Py <- ggplot(data = Py_plot, aes(x = Day, y = P_y, color = Seal)) + 
  geom_point()
plot_Py

# each salmon species escaping
escape.data <- data.frame(cbind(melt(escape_chinook, "Day"), melt(escape_sockeye, "Day")$value, melt(escape_coho, "Day")$value))
colnames(escape.data) <- c("Day", "Year", "Chinook", "Sockeye", "Coho")
escape_plot <- ggplot(data = escape.data, aes(x = Day)) +
  geom_point(data = escape.data, aes(y = Chinook), color = "dodgerblue") + 
  geom_point(data = escape.data, aes(y = Sockeye), color = "salmon") + 
  geom_point(aes(y = Coho), color = "green3")
escape_plot


