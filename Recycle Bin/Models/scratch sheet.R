
y.test <- seq(-10, 0, 0.1)
buffer_Pymin <- 0.4
threshold <- -10

plot(y.test, 1-(1/((1+buffer_Pymin) + exp(-steepness * (threshold - y.test)))))
