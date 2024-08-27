# salmon plots

salmon_escapement <- data.frame(Chum = escape_chum[days],
                                Chinook_GR = escape_gr[days],
                                Chinook_LN = escape_ln[days])

salmon_consumed <- data.frame(Chum = sum(eaten_chum), Chinook_GR = sum(eaten_gr),
                              Chinook_LN = sum(eaten_ln))

plot_consumed <- makePlot_2(x = 1:days, x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

# Plots of Salmon Species data

salmon.colors <- c("dodgerblue", "seagreen2", "seagreen4")
salmon.names <- c("Chum", "Chinook_GR", "Chinook_LN")
names(salmon.colors) <- salmon.names

arrival_plot <- makePlot_3(x = 1:days, data = Daily_Fish[,c("Chum", "GR_Chinook", "LN_Chinook")], 
                           col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                           value.name = "Daily Arriving Salmon", colors = salmon.colors)
escape_plot <- makePlot_3(x = 1:days, data = cbind(escape_chum, escape_gr, escape_ln),
                          col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                          value.name = "Cumulative Escaped Salmon", colors = salmon.colors)
eaten_sp_plot <- makePlot_3(x = 1:days, data = cbind(eaten_chum, eaten_gr, eaten_ln),
                            col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                            value.name = "Daily Salmon Eaten", colors = salmon.colors)
gauntlet_plot <- makePlot_3(x = 1:days, data = cbind(gauntlet_chum, gauntlet_gr, gauntlet_ln),
                            col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                            value.name = "Daily Salmon at Gauntlet", colors = salmon.colors)
fished_plot <- makePlot_3(x = 1:days, data = cbind(fished_chum, fished_gr, fished_ln), 
                          col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species",
                          value.name = "Daily Salmon Fished", colors = salmon.colors)