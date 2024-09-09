# salmon plots

salmon_escapement <- data.frame(Chum = escape_chum[days])

plot_consumed <- makePlot_2(x = 1:days, x.name = "Day", y = consumed_total, y.name = "Daily Salmon Consumed", 
                            color = "dodgerblue")

# Plots of Salmon Species data

escape_plot <- makePlot_2(y = escape_chum, y.name = "Daily Chum Escaping the Gauntlet", x = 1:days, 
                          x.name = "Day", color = "orchid3")
gauntlet_plot <- makePlot_2(y = gauntlet_chum, y.name = "Daily Chum at the Gauntlet", x = 1:days, 
                            x.name = "Day", color = "turquoise3")
fished_plot <- makePlot_2(y = fished_chum, y.name = "Daily Chum Fished at the Gauntlet", x = 1:days, 
                          x.name = "Day", color = "aquamarine3")
