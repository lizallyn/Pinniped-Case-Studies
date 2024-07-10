## Plots for Visualizing model results

# Packages ----
library(reshape2)
library(ggplot2)
library(patchwork)


# Create Functions ----

prepForPlots <- function(df, key.col = "Seal", 
                         other.cols = "Day", value.col){
  melted <- melt(data = df, key.col)
  colnames(melted) <- c(key.col, other.cols, value.col)
  melted[,key.col] <- as.factor(melted[,key.col])
  return(melted)
}

# Make Palette
# dummy for colors
prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")
colors <- rep(RColorBrewer::brewer.pal(10, "Set3"), length.out = num_seals)
color.names <- levels(prob_gauntlet_plot[,"Seal"])
names(colors) <- color.names

# Plotting Function - individual seal dots
makePlot_1 <- function(data, value.col, colors){
  data_for_plot <- prepForPlots(data, value.col = value.col)
  plot <- ggplot(data = data_for_plot, aes(x = data_for_plot[,2], 
                                           y = data_for_plot[,3], color = data_for_plot[,1])) + 
    geom_point() +
    scale_color_manual(values = colors) +
    labs(y = names(data_for_plot)[3], x = names(data_for_plot)[2], color = names(data_for_plot)[1])
  return(plot)
}

# Plotting Function - one dimension
makePlot_2 <- function(x, x.name, y, y.name, color){
  data_for_plot <- data.frame(col1 = x, col2 = y)
  colnames(data_for_plot) <- c(x.name, y.name)
  plot <- ggplot(data = data_for_plot, aes(x = data_for_plot[,1], y = data_for_plot[,2])) + 
    geom_point(color = color) +
    labs(y = names(data_for_plot)[2], x = names(data_for_plot)[1])
  return(plot)
}

# Plotting Function - salmon species data 3D
salmon.colors <- c("seagreen", "salmon", "goldenrod")
salmon.names <- c("Chinook", "Sockeye", "Coho")
names(salmon.colors) <- salmon.names

makePlot_3 <- function(x, data, col.names, variable.name, value.name, colors){
  data_for_melt <- data.frame(cbind(x, data))
  colnames(data_for_melt) <- col.names
  data_for_plot <- melt(data = data_for_melt, id.vars = col.names[1], variable.name = variable.name, value.name = value.name)
  plot <- 
    ggplot(data = data_for_plot) + 
    geom_point(aes(x = data_for_plot[,1], y = data_for_plot[,3], color = data_for_plot[,2])) +
    scale_color_manual(values = colors) +
    labs(y = value.name, x = col.names[1], color = variable.name)
  return(plot)
}



# Plots of Individual Seals ----

plot_probs <- makePlot_1(seal_prob_gauntlet, "Gauntlet Probabilities", colors)
plot_eaten <- makePlot_1(salmon_consumed_pv, "Salmon Eaten per Seal", colors)
plot_C <- makePlot_1(C, "C (adjusted consumption)", colors)
plot_x <- makePlot_1(x, "x (foraging opinion)", colors)
plot_Px <- makePlot_1(P_x, "P_x", colors)
plot_y <- makePlot_1(y, "y (harvest risk opinion", colors)
plot_Py <- makePlot_1(P_y, "P_y", colors)
plot_Psoc <- makePlot_1(P_social, "P_social", colors)

# Plots of Aggregated 1D data ----
plot_seals <- makePlot_2(x = 1:days, x.name = "Day", y = colSums(seal_forage_loc), 
                         y.name = "Num Seals at Gauntlet", color = "turquoise3")
plot_consumed <- makePlot_2(x = 1:days, x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")
plot_H <- makePlot_2(x = 1:days, x.name = "Day", y = H, y.name = "Seals Harvested", color = "orchid")

# Plots of Salmon Species data ----

escape_plot <- makePlot_3(x = 1:days, data = cbind(escape_chinook, escape_sockeye, escape_coho),
           col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
           value.name = "Cumulative Escaped Salmon", colors = salmon.colors)
eaten_sp_plot <- makePlot_3(x = 1:days, data = cbind(eaten_chinook, eaten_sockeye, eaten_coho),
                            col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
                            value.name = "Daily Salmon Eaten", colors = salmon.colors)
gauntlet_plot <- makePlot_3(x = 1:days, data = cbind(gauntlet_chinook, gauntlet_sockeye, gauntlet_coho),
                            col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
                            value.name = "Daily Salmon at Gauntlet", colors = salmon.colors)
fished_plot <- makePlot_3(x = 1:days, data = cbind(fished_chinook, fished_sockeye, fished_coho),
                          col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
                          value.name = "Daily Fished Salmon", colors = salmon.colors)


# OTHER ----

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
  geom_line(aes(x = list_x, y = 1-(1/((1+0) + 
                                        exp(-2 * (2 - list_x))))), color = "salmon", lwd = 2) + 
  labs(y = "gauntlet probability", x = "x")
x_over_Px_plot


salmon_escapement <- data.frame(Sockeye = escape_sockeye[days], Chinook = escape_chinook[days],
                                Coho = escape_coho[days])

prob_gauntlet_of_seal <- seq(0, 1, 0.01)
alpha_c <- (-beta*mean)/(mean-1)
receptivity <- dbeta(x = prob_gauntlet_of_seal, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0)
max <- dbeta(x = mean, shape1 = alpha_c + 1, shape2 = beta + 1, ncp = 0)
min <- 0
scaled_rec <- (receptivity - min)/(max - min)
rec.data <- data.frame(cbind(prob_gauntlet_of_seal, scaled_rec))
receptivity_plot <- ggplot(data = rec.data, aes(x = prob_gauntlet_of_seal, y = scaled_rec)) + 
  geom_line(lwd = 2, color = "black") +
  labs(y = "Receptivity", x = "P_G") +
  theme_classic()
receptivity_plot
# ggsave(plot = receptivity_plot, filename = "receptivity_plot.png", device = "png",
#       path = "Plot Exports", height = 5, width = 8)
