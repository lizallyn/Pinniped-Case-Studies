# salmon plots

salmon_escapement <- data.frame(Chinook_GR = escape_gr[days],
                                Chinook_LN = escape_ln[days])

plot_consumed <- makePlot_2(x = 1:days, x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

# Plots of Salmon Species data

salmon.colors <- c("seagreen2", "seagreen4")

escape_plot <- makePlot_3(x = 1:days, data = cbind(escape_gr, escape_ln),
                          col.names = c("Day", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                          value.name = "Cumulative Escaped Salmon", colors = salmon.colors)
eaten_sp_plot <- makePlot_3(x = 1:days, data = cbind(eaten_gr, eaten_ln),
                            col.names = c("Day", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                            value.name = "Daily Salmon Eaten", colors = salmon.colors)
gauntlet_plot <- makePlot_3(x = 1:days, data = cbind(gauntlet_gr, gauntlet_ln),
                            col.names = c("Day", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                            value.name = "Daily Salmon at Gauntlet", colors = salmon.colors)
fished_plot <- makePlot_3(x = 1:days, data = cbind(fished_gr, fished_ln), 
                          col.names = c("Day", "Chinook_GR", "Chinook_LN"), variable.name = "Species",
                          value.name = "Daily Salmon Fished", colors = salmon.colors)