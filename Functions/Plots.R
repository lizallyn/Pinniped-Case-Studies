# with ggplot to show each seal individually

library(reshape2)
library(ggplot2)
library(patchwork)

seal.colors <- RColorBrewer::brewer.pal(10, "Set3")
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
colors <- RColorBrewer::brewer.pal(num_seals, "Set3")
color.names <- levels(prob_gauntlet_plot[,"Seal"])
names(colors) <- color.names

# Make plots
prob_gauntlet_plot <- prepForPlots(seal_prob_gauntlet, value.col = "Prob_G")
plot_probs <- ggplot(data = prob_gauntlet_plot, aes(x = Day, y = Prob_G, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_probs

# went_plot <- prepForPlots(salmon_consumed, value.col = "eaten")
# plot_eaten <- ggplot(data = eaten_plot, aes(x = Day, y = eaten, color = Seal)) + 
#   geom_point() +
#   scale_color_manual(values = colors)
# plot_eaten

eaten_plot <- prepForPlots(salmon_consumed, value.col = "eaten")
plot_eaten <- ggplot(data = eaten_plot, aes(x = Day, y = eaten, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_eaten

C_plot <- prepForPlots(C, value.col = "C")
plot_C <- ggplot(data = C_plot, aes(x = Day, y = C, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_C

x_plot <- prepForPlots(x, value.col = "x")
plot_x <- ggplot(data = x_plot, aes(x = Day, y = x, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_x

Px_plot <- prepForPlots(P_x, value.col = "P_x")
plot_Px <- ggplot(data = Px_plot, aes(x = Day, y = P_x, color = Seal)) + 
  geom_point() +
  scale_color_manual(values = colors)
plot_Px

H_plot <- data.frame(cbind(1:days, H))
colnames(H_plot) <- c("Day", "H")
plot_H <- ggplot(data = H_plot, aes(x = Day, y = H)) +
  geom_point(color = "turquoise")
plot_H

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

# each salmon species escaping

escape.data <- data.frame(cbind(1:days, escape_chinook, escape_sockeye, escape_coho))
colnames(escape.data) <- c("Day", "Chinook", "Sockeye", "Coho")
escape.data <- melt(escape.data, "Day", variable.name = "Species", value.name = "Count")
salmon.colors <- c("dodgerblue", "salmon", "green3")
salmon.names <- levels(escape.data$Species)
names(salmon.colors) <- salmon.names
escape_plot <- ggplot(data = escape.data, aes(x = Day)) +
  geom_point(data = escape.data, aes(y = Count, color = Species)) + 
  scale_color_manual(values = salmon.colors) +
  labs(y = "Cumulative Salmon Escaped")
escape_plot

## Composites ----

# these use Patchwork!

# prob gauntlet with individual learning bits
plot_probs + (plot_C/plot_x/plot_H/plot_y) + plot_layout(guides = "collect")
# P_x and P_y
plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")
# salmon consumed and salmon escaped