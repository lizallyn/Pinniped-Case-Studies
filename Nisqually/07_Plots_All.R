# salmon plots

salmon_escapement <- data.frame(Chum = escape_chum[days],
                                Chinook_GR = escape_gr[days],
                                Chinook_LN = escape_ln[days])
salmon_catch <- data.frame(Chum = sum(fished_chum),
                           Chinook_GR = sum(fished_gr),
                           Chinook_LN = sum(fished_ln))

salmon_consumed <- data.frame(Chum = sum(eaten_chum), Chinook_GR = sum(eaten_gr),
                              Chinook_LN = sum(eaten_ln))

plot_consumed <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

harvested <- data.frame(Seals = sum(H), Stellers = sum(H_ej), Californias = sum(H_zc))


# Plots of Salmon Species data

salmon.colors <- c(Chum = "salmon4", GR = "darkseagreen4", LN = "lightblue3")
salmon.names <- c("Chum", "Chinook_GR", "Chinook_LN")
names(salmon.colors) <- salmon.names


arrival_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = Daily_Fish[,c("Chum", "GR_Chinook", "LN_Chinook")], 
                           col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                           value.name = "Arriving Salmon (Fitted)", colors = salmon.colors, legend.inc = T)
raw.arrival <- makePlot_3(x = 1:days + (start_loop - 1), data = fish.wide[1:days, c("Chum", "GR", "LocNis")],
                          col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species",
                          value.name = "Arriving Salmon (Data)", colors = salmon.colors, legend.inc = T)
raw_arrival_plot <- arrival_plot + 
  geom_point(aes(x = raw.arrival$data[,1], y = raw.arrival$data[,3], color = raw.arrival$data[,2]), shape = 1)
escape_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(escape_chum, escape_gr, escape_ln),
                          col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                          value.name = "Cumulative Escaped Salmon", colors = salmon.colors, legend.inc = T)
eaten_sp_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(eaten_chum, eaten_gr, eaten_ln),
                            col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                            value.name = "Salmon Eaten", colors = salmon.colors, legend.inc = T)
gauntlet_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(gauntlet_chum, gauntlet_gr, gauntlet_ln),
                            col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species", 
                            value.name = "Salmon at Gauntlet", colors = salmon.colors, legend.inc = T)
fished_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(fished_chum, fished_gr, fished_ln), 
                          col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species",
                          value.name = "Salmon Fished", colors = salmon.colors, legend.inc = T)
catch_rate_plot <- makePlot_3(x = 1:days + (start_loop - 1), data = cbind(chum_catch_rate, gr_catch_rate, ln_catch_rate), 
                          col.names = c("Day", "Chum", "Chinook_GR", "Chinook_LN"), variable.name = "Species",
                          value.name = "Salmon Catch Rate", colors = salmon.colors, legend.inc = T)
