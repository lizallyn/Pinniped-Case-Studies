

library(ggmap)
library(ggrepel)
library(ggplot2)
library(cowplot)

# bounds for big map
maxlong <- -125.5
minlat <- 46.5
minlong <- -121.5
maxlat <- 48.5

# bounds for inset map
insleft <- -140
insright <- -110
instop <- 60
insbott <- 30

insetbox.lat <- c(minlat, minlat, maxlat, maxlat, minlat)
insetbox.long <- c(minlong, maxlong, maxlong, minlong, minlong)
insetbox.shape <- data.frame(cbind(insetbox.lat, insetbox.long))

outline.lat <- c(insbott, insbott, instop, instop, insbott)
outline.long <- c(insleft, insright, insright, insleft, insleft)
outline <- data.frame(cbind(outline.lat, outline.long))

inset <- get_stadiamap(bbox=c(insleft, insbott, insright, instop), 
                       zoom=4, maptype="stamen_terrain_background")
base_ter <- get_stadiamap(bbox = c(maxlong, minlat, minlong, maxlat), 
                          zoom=11, maptype="stamen_terrain_background")
insetmap <- ggmap(inset) +
  geom_path(data = insetbox.shape, aes(x = insetbox.long, y = insetbox.lat), lwd = 0.5) +
  theme_void() +
  geom_path(data = outline, aes(x = outline.long, y = outline.lat), lwd = 1.5)
map1 <- ggmap(base_ter) +
  labs(x = "Longitude", y = "Latitude")

map_with_inset <- ggdraw() + 
  draw_plot(map1) + 
  draw_plot(insetmap, x = 0.68, y = 0.1, 
            width = 0.3, height=0.3)
map_with_inset
