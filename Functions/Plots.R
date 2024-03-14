## Plots for Visualizing model results

# with ggplot to show each seal individually

library(reshape2)
library(ggplot2)
library(patchwork)

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
colors <- rep(RColorBrewer::brewer.pal(10, "Set3"), times = (num_seals/10))
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

seal.data <- data.frame(cbind(1:days, colSums(seal_forage_loc)))
colnames(seal.data) <- c("Day", "Count")
plot_seals <- ggplot(data = seal.data, aes(x = Day, y = Count)) +
  geom_point(color = "dodgerblue") +
  labs(y = "Num Seals at the Gauntlet")
plot_seals

H_plot <- data.frame(cbind(1:days, H))
colnames(H_plot) <- c("Day", "H")
plot_H <- ggplot(data = H_plot, aes(x = Day, y = H)) +
  geom_point(color = "turquoise")
plot_H

prob_gauntlet_of_seal <- seq(0, 1, 0.01)
alpha <- (-beta*mean)/(mean-1)
receptivity <- dbeta(x = prob_gauntlet_of_seal, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
max <- dbeta(x = mean, shape1 = alpha + 1, shape2 = beta + 1, ncp = 0)
min <- 0
scaled_rec <- (receptivity - min)/(max - min)
rec.data <- data.frame(cbind(prob_gauntlet_of_seal, scaled_rec))
receptivity_plot <- ggplot(data = rec.data, aes(x = prob_gauntlet_of_seal, y = scaled_rec)) + 
  geom_line(lwd = 2, color = "turquoise3") +
  labs(y = "Receptivity", x = "P_G")
receptivity_plot

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

# each salmon species

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

## Composites ----

# these use Patchwork!

# prob gauntlet with individual learning bits
plot_probs + (plot_C/plot_x/plot_H/plot_y) + plot_layout(guides = "collect")
# P_x and P_y
plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")
# salmon consumed and salmon escaped
