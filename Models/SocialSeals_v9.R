# Social Seals version 9
# For separating out the salmon species
# Februrary 2024

#### Set Up ####

## Load Packages and Data
library(ggplot2)

## Load Data Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/Prep_data_for_Salmon_functions.R")

## Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/makeArray.R")
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
years <- 1
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
escape_rate <- 0.3
natural_mort <- 0.0005

# fishing parameters
catch_rate <- 0.3

# hunting parameters
zone_efficiency <- 0.8
processing_time <- 0.05
gamma_H <- 0
Y_H <- 0

## Set Up Variables
salmon_escape <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
gauntlet_salmon <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
gauntlet_chinook <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
gauntlet_sockeye <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
gauntlet_coho <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
H <- makeArray(days, years, start.val = NA, namex = "Day", namey = "Year")

salmon_consumed <- makeArray(num_seals, days, years, start.val = 0, 
                              namex = "Seal", namey = "Day", namez = "Year")
seal_prob_gauntlet <- makeArray(num_seals, days, years, start.val = seal_initial_prob_gauntlet, 
                                 namex = "Seal", namey = "Day", namez = "Year")
seal_forage_loc <- makeArray(num_seals, days, years, start.val = seal_start_loc, 
                              namex = "Seal", namey = "Day", namez = "Year")

# harvest matrix
salmon_days <- which(Daily_fish$total > 0)
harvest_plan <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")
fishers <- makeArray(days, years, start.val = 0, namex = "Day", namey = "Year")

# Variables for x y learning bit
x <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")
y <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")
C <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")
B <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")
P_x <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")
P_y <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")

# for social learning
P_social <- makeArray(num_seals, days, years, start.val = 0, namex = "Seal", namey = "Day", namez = "Year")


#### Run time loop ####
for(j in 1:years) {
  
  for(t in 1:(days-1)) {
    
    # salmon arrive at the gauntlet
    daily_update <- salmonSpeciesUpdate(day = t, data = Daily_fish)
    gauntlet_chinook[t, j] <- daily_update %>% slice(1) %>% pull(Chinook)
    gauntlet_sockeye[t, j] <- daily_update %>% slice(1) %>% pull(Sockeye)
    gauntlet_coho[t, j] <- daily_update %>% slice(1) %>%  pull(Coho)
    gauntlet_salmon[t, j] <- sum(c(gauntlet_chinook[t, j], gauntlet_sockeye[t, j], gauntlet_coho[t, j]))
    
    # decide where each seal goes that day
    for(seal in 1:num_seals) {
      seal_forage_loc[seal,t,j] <- decideForagingDestination(seal_prob_gauntlet[seal,t,j])
    }
    
    # round of copying
    for(seal in 1:num_seals) {
      P_social[seal, t, j] <- collusion(probs_list = seal_prob_gauntlet[,t,j], 
                                        prob_gauntlet_of_seal = seal_prob_gauntlet[seal, t, j], 
                                        seals_2_copy = num_seals_2_copy, 
                                        mean = mean, beta = beta)
    }
    
    # calculate salmon mortality 
    seals_at_gauntlet <- which(seal_forage_loc[,t,j] == 1) # think about escape rate here
    day_result <- rungeKutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                              alpha = alpha, Ns = gauntlet_salmon[t, j], 
                              gamma = gamma, Y = Y, E = escape_rate, 
                              F_catch = catch_rate, M = natural_mort, deltat = 1)
    # propogate to abundance in next time step for each species
    gauntlet_chinook[t+1, j] <- day_result[1] * daily_update %>% slice(2) %>%  pull(Chinook)
    gauntlet_sockeye[t+1, j] <- day_result[1] * daily_update %>% slice(2) %>%  pull(Sockeye)
    gauntlet_coho[t+1, j] <- day_result[1] * daily_update %>% slice(2) %>%  pull(coho)
    # assign consumed salmon to seals at gauntlet
    salmon_consumed[seals_at_gauntlet, t, j] <- day_result[2]/length(seals_at_gauntlet)
    # escape salmon
    salmon_escape[t, j] <- day_result[4] * gauntlet_salmon[t, j]
    
    # seal harvest
    H[t, j] <- getHarvested(day_plan = harvest_plan[t, j], num_gauntlet_seals = length(seals_at_gauntlet), 
                          zone_efficiency = zone_efficiency, Hmax = harvest_max_perboat, 
                          processing = processing_time, num_fishers = fishers[t, j], 
                          gamma = gamma_H, Y = Y_H)
    if(H[t, j] > 0){
      seal_prob_gauntlet[sample(seals_at_gauntlet, H[t, j]), t+1, j] <- NA
    }
    
    # calculate x, y and prob_gauntlet for next time step
    ## This could all become some functions
    for(seal in 1:num_seals){
      # calculate C
      C[seal, t, j] <- salmon_consumed[seal, t, j] - w
      
      # calculate d_x and d_y
      d_x <- learnX(food = C[seal, t, j], x_t = x[seal, t, j], 
                     forage_loc = seal_forage_loc[seal, t, j],  step = step, 
                     xmin = xmin, xmax = xmax, decay = decay)
      d_y <- learnY(hunting = H[t, j], y_t = y[seal, t, j], 
                     seal_forage_loc[seal, t, j], step = step, 
                     ymin = ymin, ymax = ymax, decay = decay)
      
      # update x and y and P_x and P_y
      x[seal, t+1, j] <- x[seal, t, j] + d_x
      P_x[seal, t+1, j] <- x[seal, t+1, j] * slope_x + intercept_x
      
      y[seal, t+1, j] <- y[seal, t, j] + d_y
      P_y[seal, t+1, j] <- 1-(1/((1+buffer_Pymin) + exp(-steepness * (threshold - y[seal, t+1, j]))))
      
      # calculate Prob gauntlet
      seal_prob_gauntlet[seal, t+1, j] <- P_x[seal, t+1, j] * P_y[seal, t+1, j]
    }
    
    
  } # days loop
} # years loop

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

