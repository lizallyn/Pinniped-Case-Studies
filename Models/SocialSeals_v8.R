# Social seals version 8 
# with instantaneous salmon from Tim
# Feb 2024

#### Set Up ####

## Load Packages and Data
library(ggplot2)

## Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/salmon_arrive.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/decide_foraging_destination.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/collusion.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/learn_x.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/learn_y.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/get_dXdt.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/runge_kutta.R")

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


## Set Up Variables
salmon_escape <- array(dim = c(days, years),
                       data = rep(0, days * years))
dimnames(salmon_escape) <- list(Day = 1:days, Year = 1:years)
gauntlet_salmon <- array(dim = c(days, years),
                         data = rep(0, days * years))
dimnames(gauntlet_salmon) <- list(Day = 1:days, Year = 1:years)
salmon_consumed <- array(dim = c(num_seals, days, years), 
                         data = rep(0, num_seals * days * years))
dimnames(salmon_consumed) <- list(Seal = 1:num_seals, Day = 1:days, 
                                  Year = 1:years)
seal_prob_gauntlet <- array(dim = c(num_seals, days, years), 
                            data = rep(seal_initial_prob_gauntlet, 
                                       num_seals * days * years))
dimnames(seal_prob_gauntlet) <- list(Seal = 1:num_seals, Day = 1:days, 
                                     Year = 1:years)
seal_forage_loc <- array(dim = c(num_seals, days, years), 
                         data = rep(seal_start_loc, 
                                    num_seals * days * years))
dimnames(seal_forage_loc) <- list(Seal = 1:num_seals, Day = 1:days, 
                                  Year = 1:years)
H <- array(dim = c(days, years),
           data = rep(NA, days * years))
dimnames(H) <- list(Day = 1:days, Year = 1:years)

# Variables for x y learning bit
x <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))
y <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))
C <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))
B <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))
P_x <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))
P_y <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))

# for social learning
P_social <- array(dim = c(num_seals, days, years), data = rep(0, num_seals * days * years))


#### Run time loop ####
for(j in 1:years) {
  
  for(t in 1:(days-1)) {
    
    # salmon arrive at the gauntlet
    salmon_arriving <- sum(salmon_arrive(day = (t+1))$avg)
    gauntlet_salmon[t, j] <- gauntlet_salmon[t, j] + salmon_arriving
    
    # decide where each seal goes that day
    for(seal in 1:num_seals) {
      seal_forage_loc[seal,t,j] <- decide_foraging_destination(seal_prob_gauntlet[seal,t,j])
    }
    
    # round of copying
    for(seal in 1:num_seals) {
      P_social[seal, t, j] <- collusion(probs_list = seal_prob_gauntlet[,t,j], 
                                        prob_gauntlet_of_seal = seal_prob_gauntlet[seal, t, j], 
                                        seals_2_copy = num_seals_2_copy, 
                                        mean = mean, beta = beta)
    }
    
    # calculate salmon consumption 
    seals_at_gauntlet <- which(seal_forage_loc[,t,j] == 1)
    day_result <- runge_kutta(Cmax = Cmax, Nseal = length(seals_at_gauntlet), 
                              alpha = alpha, Ns = gauntlet_salmon[t, j], 
                              gamma = gamma, Y = Y, E = escape_rate, 
                              F_catch = catch_rate, M = natural_mort, deltat = 1)
    salmon_consumed[seals_at_gauntlet, t, j] <- day_result[2]/length(seals_at_gauntlet)
    gauntlet_salmon[t+1, j] <- day_result[1]
    
    # seal harvest
    H[t, j] <- 0
    
    # calculate x, y and prob_gauntlet for next time step
    ## This could all become some functions
    for(seal in 1:num_seals){
      # calculate C
      C[seal, t, j] <- salmon_consumed[seal, t, j] - w
      
      # calculate d_x and d_y
      d_x <- learn_x(food = C[seal, t, j], x_t = x[seal, t, j], 
                     forage_loc = seal_forage_loc[seal, t, j],  step = step, 
                     xmin = xmin, xmax = xmax, decay = decay)
      d_y <- learn_y(hunting = H[t, j], y_t = y[seal, t, j], 
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
