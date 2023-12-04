# Rescorla-Wagner via Miller and Shettleworth 2007, 2013
# December 2023

# parameters
salience <- 0.15 # seems to be standard for this model?
learning_rate <- 1 # standard bc no data I guess?

# variables
V_G <- array(dim = days)
V_W <- array(dim = days)
V_F <- array(dim = days)
V_B <- array(dim = days)

