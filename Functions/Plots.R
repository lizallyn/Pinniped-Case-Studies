## Plots for Visualizing model results

# with ggplot to show each seal individually

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

# Make Seal Palette
colors <- rep(RColorBrewer::brewer.pal(10, "Set3"), length.out = num_seals)
color.names <- levels(prob_gauntlet_plot[,"Seal"])
names(colors) <- color.names

# Make plots
prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")
plot_probs <- ggplot(data = prob_gauntlet_plot, aes(x = Day, y = Prob_G, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors) +
  labs(y = "Prob_Gauntlet")
plot_probs

eaten_plot <- prepForPlots(salmon_consumed, value.col = "eaten")
plot_eaten <- ggplot(data = eaten_plot, aes(x = Day, y = eaten, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors) +
  labs(y = "Salmon Eaten per Seal")
plot_eaten

C_plot <- prepForPlots(C, value.col = "C")
plot_C <- ggplot(data = C_plot, aes(x = Day, y = C, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors) + 
  labs(y = "C")
plot_C

x_plot <- prepForPlots(x, value.col = "x")
plot_x <- ggplot(data = x_plot, aes(x = Day, y = x, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors) + 
  labs(y = "x")
plot_x

Px_plot <- prepForPlots(P_x, value.col = "P_x")
plot_Px <- ggplot(data = Px_plot, aes(x = Day, y = P_x, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_Px

seal.data <- data.frame(cbind(1:days, colSums(seal_forage_loc, na.rm = T)))
colnames(seal.data) <- c("Day", "Count")
plot_seals <- ggplot(data = seal.data, aes(x = Day, y = Count)) +
  geom_point(color = "dodgerblue") +
  labs(y = "Num Seals at the Gauntlet")
plot_seals

consumed_plot <- data.frame(cbind(1:days, consumed_total))
colnames(consumed_plot) <- c("Day", "consumed")
plot_consumed <- ggplot(data = consumed_plot, aes(x = Day, y = consumed)) + 
  geom_point() +
  labs(y = "Total Salmon Consumed")
plot_consumed

H_plot <- data.frame(cbind(1:days, H))
colnames(H_plot) <- c("Day", "H")
plot_H <- ggplot(data = H_plot, aes(x = Day, y = H)) +
  geom_point(color = "black")
plot_H

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

Psoc_plot <- prepForPlots(P_social, value.col = "P_social")
plot_Psoc <- ggplot(data = Psoc_plot, aes(x = Day, y = P_social, color = Seal)) +
  geom_point() +
  scale_color_manual(values = colors)
plot_Psoc

# each salmon species

salmon_escapement <- data.frame(Sockeye = escape_sockeye[days], Chinook = escape_chinook[days],
                                Coho = escape_coho[days])

escape.data <- data.frame(cbind(1:days, escape_chinook, escape_sockeye, escape_coho))
colnames(escape.data) <- c("Day", "Chinook", "Sockeye", "Coho")
escape.data <- melt(escape.data, "Day", variable.name = "Species", value.name = "Count")
salmon.colors <- c("seagreen", "salmon", "goldenrod")
salmon.names <- levels(escape.data$Species)
names(salmon.colors) <- salmon.names
escape_plot <- ggplot(data = escape.data, aes(x = Day)) +
  geom_point(data = escape.data, aes(y = Count, color = Species)) + 
  scale_color_manual(values = salmon.colors) +
  labs(y = "Cumulative Salmon Escaped")
escape_plot

eaten.sp.data <- data.frame(cbind(1:days, eaten_chinook, eaten_sockeye, eaten_coho))
colnames(eaten.sp.data) <- c("Day", "Chinook", "Sockeye", "Coho")
eaten.sp.data <- melt(eaten.sp.data, "Day", variable.name = "Species", value.name = "Count")
eaten_sp_plot <- ggplot(data = eaten.sp.data, aes(x = Day, y = Count)) + 
  geom_point(aes(color = Species)) +
  scale_color_manual(values = salmon.colors) +
  labs(y = "Daily Salmon Eaten")
eaten_sp_plot

gauntlet.data <- data.frame(cbind(1:days, gauntlet_chinook, gauntlet_sockeye, gauntlet_coho))
colnames(gauntlet.data) <- c("Day", "Chinook", "Sockeye", "Coho")
gauntlet.data <- melt(gauntlet.data, "Day", variable.name = "Species", value.name = "Count")
gauntlet_plot <- ggplot(data = gauntlet.data, aes(x = Day, y = Count)) + 
  geom_point(aes(color = Species)) +
  scale_color_manual(values = salmon.colors) +
  labs(y = "Daily Salmon at Gauntlet")
gauntlet_plot

fished.data <- data.frame(cbind(1:days, fished_chinook, fished_sockeye, fished_coho))
colnames(fished.data) <- c("Day", "Chinook", "Sockeye", "Coho")
fished.data <- melt(fished.data, "Day", variable.name = "Species", value.name = "Count")
fished_plot <- ggplot(data = fished.data, aes(x = Day, y = Count)) +
  geom_point(aes(color = Species)) + 
  scale_color_manual(values = salmon.colors) +
  labs(y = "Daily Fished Salmon")

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


## Composites ----

# these use Patchwork!

# prob gauntlet with individual learning bits
plot_probs + (plot_C/plot_x/plot_H/plot_y) + plot_layout(guides = "collect")
# P_x and P_y
plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")
# salmon consumed and salmon escaped
