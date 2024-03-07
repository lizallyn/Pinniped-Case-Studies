# Social Seals version 9
# For separating out the salmon species
# February 2024

# Set Up ----

## Load Packages and Data
library(ggplot2)
library(patchwork)

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

## Set Up Parameters ----

# loop parameters
days <- 365

# seal parameters
num_seals <- 10

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
buffer_Pymin <- 0.1 # Need to rememmber why this felt necessary?

# seal social learning parameters
num_seals_2_copy <- 0
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

# salmon parameters
sockeye_escape_rate <- 0.3
chinook_escape_rate <- 0.03
coho_escape_rate <- 0.1
natural_mort <- 0.0005

# hunting parameters
zone_efficiency <- 0.8
processing_time <- 0.05
gamma_H <- 0
Y_H <- 0
min_fishers <- 13
max_fishers <- 25
salmon_days <- which(Daily_fish$total > 0)
harvest_max_perboat <- 2

## Set Up Variables ----

oneDzeroes <- makeArray(days, start.val = 0, names = "Day")
twoDzeroes <- makeArray(c(num_seals, days), start.val = 0, names = c("Seal", "Day"))

### Individual Values ----
salmon_consumed <- twoDzeroes
seal_prob_gauntlet <- twoDzeroes
seal_forage_loc <- twoDzeroes

x <- twoDzeroes
y <- twoDzeroes
C <- twoDzeroes
P_x <- twoDzeroes
P_y <- twoDzeroes

P_social <- twoDzeroes

### Actual States that I Need ----
escape_chinook <- oneDzeroes
escape_sockeye <- oneDzeroes
escape_coho <- oneDzeroes

gauntlet_chinook <- oneDzeroes
gauntlet_sockeye <- oneDzeroes
gauntlet_coho <- oneDzeroes

H <- oneDzeroes

### Variable Rates ----
coho_catch_rate <- oneDzeroes
coho_catch_rate[boat_days] <- 0.3
chinook_catch_rate <- oneDzeroes
sockeye_catch_rate <- oneDzeroes

harvest_plan <- createHarvestPlan(scenario = "Boat", days = days, years = years, boat_days = boat_days, salmon_days = salmon_days)

### Troubleshooting & BS ----
# for that one plot
gauntlet_salmon <- oneDzeroes
# troubleshooting ghost salmons
screwy <- c(species = NA, day = NA, gauntlet_t = NA, Ns = NA, C = NA, Catch = NA, E = NA)


# Run Loop ----
  
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
    if(num_seals_2_copy > 0){
      P_social[seal, t] <- collusion(probs_list = seal_prob_gauntlet[,t], 
                                     prob_gauntlet_of_seal = seal_prob_gauntlet[seal, t], 
                                     seals_2_copy = num_seals_2_copy, 
                                     mean = mean, beta = beta)
      seal_forage_loc[seal,t] <- decideForagingDestination(P_social[seal,t])
    }
  }
  
  # calculate salmon mortality 
  seals_at_gauntlet <- which(seal_forage_loc[,t] == 1)
  sockeye_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_sockeye[t], 
                               gamma = gamma, Y = Y, E = sockeye_escape_rate, 
                               F_catch = sockeye_catch_rate[t], M = natural_mort, deltat = 1)
  chinook_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_chinook[t], 
                               gamma = gamma, Y = Y, E = chinook_escape_rate, 
                               F_catch = chinook_catch_rate[t], M = natural_mort, deltat = 1)
  coho_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                               alpha = alpha, Ns = gauntlet_coho[t], 
                               gamma = gamma, Y = Y, E = coho_escape_rate, 
                               F_catch = coho_catch_rate[t], M = natural_mort, deltat = 1)
  if(any(c(sockeye_result, sum(sockeye_result[2:4])) > gauntlet_sockeye[t])) {
    screwy <- rbind(screwy, c(species = "Sockeye", day = t, gauntlet_t = gauntlet_sockeye[t],sockeye_result))
    print(paste("day", t, "check screwy!!!"))
    escape_sockeye[t+1] <- escape_sockeye[t] + gauntlet_sockeye[t] * sockeye_escape_rate
    gauntlet_sockeye[t+1] <- gauntlet_sockeye[t] - gauntlet_sockeye[t] * sockeye_escape_rate
  } else {
    gauntlet_sockeye[t+1] <- sockeye_result["Ns"]
    escape_sockeye[t+1] <- escape_sockeye[t] + sockeye_result["E"]
    }
  if(any(c(chinook_result, sum(chinook_result[2:4])) > gauntlet_chinook[t])) {
    screwy <- rbind(screwy, c(species = "Chinook", day = t, gauntlet_t = gauntlet_chinook[t],chinook_result))
    print(paste("day", t, "check screwy!!!"))
    escape_chinook[t+1] <- escape_chinook[t] + gauntlet_chinook[t] * chinook_escape_rate
    gauntlet_chinook[t+1] <- gauntlet_chinook[t] - gauntlet_chinook[t] * chinook_escape_rate
    
  } else {
    gauntlet_chinook[t+1] <- chinook_result["Ns"]
    escape_chinook[t+1] <- escape_chinook[t] + chinook_result["E"]
    }
  if(any(c(coho_result, sum(coho_result[2:4])) > gauntlet_coho[t])) {
    screwy <- rbind(screwy, c(species = "Coho", day = t, gauntlet_t = gauntlet_coho[t], coho_result))
    print(paste("day", t, "check screwy!!!"))
    escape_coho[t+1] <- escape_coho[t] + gauntlet_coho[t] * coho_escape_rate
    gauntlet_coho[t+1] <- gauntlet_coho[t] - gauntlet_coho[t] * coho_escape_rate
    
  } else {
    gauntlet_coho[t+1] <- coho_result["Ns"]
    escape_coho[t+1] <- escape_coho[t] + coho_result["E"]
    }
  
  # assign consumed salmon to seals at gauntlet
  consumed_sum <- c(sockeye_result["C"], chinook_result["C"], coho_result["C"])
  consumed_sum[which(consumed_sum < 0)] <- 0
  salmon_consumed[seals_at_gauntlet, t] <- sum(consumed_sum)/length(seals_at_gauntlet)
  # if(any(salmon_consumed[,t] > 100)){
  #   print(c(consumed_sum, t))
  #   print(salmon_consumed[,t])
  #   print(gauntlet_chinook[t])
  #   print(chinook_result)
  #   print(gauntlet_sockeye[t])
  #   print(sockeye_result)
  #   print(seals_at_gauntlet)
  # } # for troubleshooting

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


# Testing Space ----




# Summary Plots ----

## Quick and Dirty ----
par(mfrow = c(2, 2))
plot(1:days, colSums(seal_forage_loc), main = "Number of seals at the gauntlet")
plot(1:days, colMeans(seal_prob_gauntlet), main = "avg. prob gauntlet")
plot(1:days, gauntlet_salmon, main = "salmon at the gauntlet")
plot(1:days, colSums(salmon_consumed), main = "salmon consumed")


plot(1:days, colMeans(C))
plot(1:days, colMeans(P_x))
H.plot <- data.frame(cbind(1:days, H))
plot(H.plot)
plot(1:days, colMeans(P_y))

## Slightly Nicer ----

# with ggplot to show each seal individually

library(reshape2)

seal.colors <- RColorBrewer::brewer.pal(10, "Set3")
seal.dfs <- c("seal_prob_gauntlet", "C")

prepForPlots <- function(df, key.col = "Seal", 
                         other.cols = "Day", value.col){
  melted <- melt(data = df, key.col)
  colnames(melted) <- c(key.col, other.cols, value.col)
  melted[,key.col] <- as.factor(melted[,key.col])
  return(melted)
}

# dummy for colors
prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")

# Make Seal Palette
colors <- RColorBrewer::brewer.pal(num_seals, "Set3")
color.names <- levels(prob_gauntlet_plot[,"Seal"])
names(colors) <- color.names

# Make plots
prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")
plot_probs <- ggplot(data = prob_gauntlet_plot, aes(x = Day, y = Prob_G, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_probs

C_plot <- prepForPlots(C, value.col = "C")
plot_C <- ggplot(data = C_plot, aes(x = Day, y = C, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_C

x_plot <- prepForPlots(x, value.col = "x")
plot_x <- ggplot(data = x_plot, aes(x = Day, y = x, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_x

Px_plot <- prepForPlots(P_x, value.col = "P_x")
plot_Px <- ggplot(data = Px_plot, aes(x = Day, y = P_x, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_Px

H_plot <- data.frame(cbind(1:days, H))
colnames(H_plot) <- c("Day", "H")
plot_H <- ggplot(data = H_plot, aes(x = Day, y = H)) +
  geom_point(color = "turquoise")
plot_H

y_plot <- prepForPlots(y, value.col = "y")
plot_y <- ggplot(data = y_plot, aes(x = Day, y = y, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_y

Py_plot <- prepForPlots(P_y, value.col = "P_y")
plot_Py <- ggplot(data = Py_plot, aes(x = Day, y = P_y, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_Py

# each salmon species escaping

escape.data <- data.frame(cbind(1:days, escape_chinook, escape_sockeye, escape_coho))
colnames(escape.data) <- c("Day", "Chinook", "Sockeye", "Coho")
escape.data <- melt(escape.data, "Day", variable.name = "Species", value.name = "Count")
salmon.colors <- c("dodgerblue", "salmon", "green3")
salmon.names <- levels(escape.data$Species)
names(salmon.colors) <- salmon.names
escape_plot <- ggplot(data = escape.data, aes(x = Day)) +
  geom_point(data = escape.data, aes(y = Count, color = Species)) + 
  scale_color_manual(values = salmon.colors) +
  labs(y = "Cumulative Salmon Escaped")
escape_plot

## Composites ----

# these use Patchwork!

# prob gauntlet with individual learning bits
plot_probs + (plot_C/plot_x/plot_H/plot_y) + plot_layout(guides = "collect")
# P_x and P_y
plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")
# salmon consumed and salmon escaped
