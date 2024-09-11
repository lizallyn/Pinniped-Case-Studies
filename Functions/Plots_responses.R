# functional response plots

# RECEPTIVITY
# prob_gauntlet_of_seal <- seq(0, 1, 0.01)
# alpha_c <- (-beta*mean)/(mean-1)
# receptivity <- dbeta(x = prob_gauntlet_of_seal, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0)
# max <- dbeta(x = mean, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0)
# min <- 0
# scaled_rec <- (receptivity - min)/(max - min)
# rec.data <- data.frame(cbind(prob_gauntlet_of_seal, scaled_rec))
# receptivity_plot <- ggplot(data = rec.data, aes(x = prob_gauntlet_of_seal, y = scaled_rec)) +
#   geom_line(lwd = 2, color = "black") +
#   labs(y = "Receptivity", x = "P_G") +
#   theme_classic()
# receptivity_plot
# ggsave(plot = receptivity_plot, filename = "receptivity_plot.png", device = "png",
#       path = "Plot Exports", height = 5, width = 8)

# Y and X to PROB plots
list_y <- seq(ymin, ymax, 0.1)
y_over_Py_plot <- ggplot() + 
  geom_line(aes(x = list_y, y = 1-(1/((1+buffer_Pymin_val) + 
                                        exp(-steepness * (threshold_val - list_y))))), color = "seagreen", lwd = 2) + 
  geom_line(aes(x = list_y, y = 1-(1/((1+buffer_Pymin_specialist) + 
                                        exp(-steepness * (threshold_specialist - list_y))))), color = "salmon", lwd = 2) + 
  labs(y = "gauntlet probability", x = "y")
y_over_Py_plot

list_x <- seq(xmin, xmax, 0.1)
x_over_Px_plot <- ggplot() + 
  geom_line(aes(x = list_x, y = list_x * slope_x_val + intercept_x_val), color = "seagreen", lwd = 2) + 
  geom_line(aes(x = list_x, y = 1-(1/((1+buffer_Pxmin_specialist) + 
                                        exp(-steepness * (threshold_x_specialist - list_x))))), color = "salmon", lwd = 2) + 
  labs(y = "gauntlet probability", x = "x")
x_over_Px_plot


