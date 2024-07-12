## Functions for creating plots with similar structures


library(reshape2)
library(ggplot2)
library(patchwork)

# plot_x_range <- start_loop:end_loop

prepForPlots <- function(df, key.col = "Seal", 
                         other.cols = "Day", value.col){
  melted <- melt(data = df, key.col)
  colnames(melted) <- c(key.col, other.cols, value.col)
  melted[,key.col] <- as.factor(melted[,key.col])
  return(melted)
}

# dummy for colors
prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")

# Make Palette
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
