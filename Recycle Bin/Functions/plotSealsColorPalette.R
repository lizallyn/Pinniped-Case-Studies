
plotSealsColorPalette <- function(num_levels, palette = "Set3"){
  colors <- rep(RColorBrewer::brewer.pal(10, palette), times = (num_levels/10))
  color.names <- 1:num_levels
  names(colors) <- color.names
  return(colors)
}

