# salmon plots

salmon_escapement <- data.frame(Sockeye = escape_sockeye[days], Chinook = escape_chinook[days],
                                Coho = escape_coho[days])
salmon_catch <- data.frame(Sockeye = sum(fished_sockeye), Chinook = sum(fished_chinook),
                           Coho = sum(fished_coho))
salmon_eaten <- data.frame(Sockeye = sum(eaten_sockeye), Chinook = sum(eaten_chinook),
                           Coho = sum(eaten_coho))

plot_consumed <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

# Plots of Salmon Species data

escape_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(escape_chinook, escape_sockeye, escape_coho),
                          col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
                          value.name = "Cumulative Escaped Salmon", colors = salmon.colors)
eaten_sp_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(eaten_chinook, eaten_sockeye, eaten_coho),
                            col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
                            value.name = "Daily Salmon Eaten", colors = salmon.colors)
gauntlet_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(gauntlet_chinook, gauntlet_sockeye, gauntlet_coho),
                            col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species", 
                            value.name = "Daily Salmon at Gauntlet", colors = salmon.colors)
fished_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(fished_chinook, fished_sockeye, fished_coho), 
                          col.names = c("Day", "Chinook", "Sockeye", "Coho"), variable.name = "Species",
                          value.name = "Daily Salmon Fished", colors = salmon.colors)