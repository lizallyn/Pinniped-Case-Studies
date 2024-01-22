### V7
# late Jan 2024

#### Set Up ####

## Load Packages and Data
library(ggplot2)

## Load Function Files
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/salmon_arrive.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/eat_some_fish_3.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/decide_foraging_destination.R")
# source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/get_influenced.R")
source("https://raw.githubusercontent.com/lizallyn/Pinniped-Case-Studies/main/Functions/collusion.R")

## Set Parameters

# loop parameters
years <- 1
days <- 365

# seal parameters
num_seals <- 20
seal_initial_prob_gauntlet <- 0.1
seal_start_loc <- 0

# seal consumption parameters
seal_handling_time <- 0.05
seal_satiation <- 5
pd <- 0
Y <- 1

# salmon parameters
escape_rate <- 0.3

# seal learning parameters
w <- 0.1
ymin <- -10
ymax <- 0
xmin <- -1
xmax <- 9
steepness <- 5
threshold <- -3

# seal social learning parameters
num_seals_2_copy <- 2
mean <- 0.5 # of the beta dist
beta <- 15 # spread of the beta dist

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
    predation_rate <- 
      eat_some_fish(num_gauntlet_salmon = gauntlet_salmon[t,j], 
                    num_seals_at_gauntlet = length(seals_at_gauntlet), 
                    handling_time = seal_handling_time, 
                    satiation = seal_satiation, pd = pd, Y = Y)
    
    # calculate salmon inst mortality
    inst_predation <- predation_rate / (predation_rate + catch_rate + escape_rate) *
      (1 - exp(-predation_rate - catch_rate - escape_rate))
    inst_catch_rate <- catch_rate / (predation_rate + catch_rate + escape_rate) *
      (1 - exp(-predation_rate - catch_rate - escape_rate))
    inst_escape <- escape_rate / (predation_rate + catch_rate + escape_rate) *
      (1 - exp(-predation_rate - catch_rate - escape_rate))
    gauntlet_salmon[t, j] <- gauntlet_salmon[t, j] * exp(-inst_predation - inst_catch_rate - inst_escape)
    
    # assign consumed salmon to seals at the gauntlet
    if(predation_rate>0) {
      salmon_consumed[seals_at_gauntlet, t, j] <- inst_predation * 
        gauntlet_salmon[t, j]/length(seals_at_gauntlet)
    }
    
    # seal harvest
    H[t, j] <- 0
    
    # calculate x, y and prob_gauntlet for next time step
    ## This could all become some functions
    for(seal in 1:num_seals){
      C[seal, t, j] <- salmon_consumed[seal, t, j]/seal_satiation - w
      if(C[seal, t, j] > 0){
        d_x <- 0.25*(xmax - x[seal, t, j])
      } else if(C[seal, t, j] < 0){
        d_x <- 0.25*(xmin - x[seal, t, j])
      } else {d_x <- 0}
      x[seal, t+1, j] <- x[seal, t+1, j] + d_x
      P_x[seal, t+1, j] <- x[seal, t+1, j] * 0.1 + 0.1
      
      if(sum(H[t, j]) == 0){
        d_y <- 0.25*(ymax - y[seal, t, j])
      } else if(sum(H[t, j]) > 0){
        d_y <- 0.25*(ymin - y[seal, t, j])
      }
      y[seal, t+1, j] <- y[seal, t, j] + d_y
      P_y[seal, t+1, j] <- 1-(1/(1.1 + exp(-steepness * (threshold - y[seal, t+1, j]))))
      
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

# with ggplot to show each seal individually

library(reshape2)

prob_gauntlet_plot <- melt(data = seal_prob_gauntlet[,,1], "Seal")
colnames(prob_gauntlet_plot) <- c("Seal", "Day", "Prob")
plot_probs <- ggplot(data = prob_gauntlet_plot, aes(x = Day, y = Prob, color = Seal)) + 
  geom_point()
plot_probs
