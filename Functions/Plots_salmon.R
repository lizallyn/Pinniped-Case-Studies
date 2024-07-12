# salmon plots

salmon_escapement <- data.frame(Sockeye = escape_sockeye[days], Chinook = escape_chinook[days],
                                Coho = escape_coho[days])

plot_consumed <- makePlot_2(x = 1:days, x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

# Plots of Salmon Species data

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
                          value.name = "Daily Salmon Fished", colors = salmon.colors)