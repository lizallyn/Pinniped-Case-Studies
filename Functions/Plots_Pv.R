# Seal Plots

# Plots of Individual Seals
plot_probs <- makePlot_1(seal_prob_gauntlet, "Gauntlet Probabilities", colors)
plot_eaten <- makePlot_1(salmon_consumed_pv, "Salmon Eaten per Seal", colors)
plot_C <- makePlot_1(C, "C (adjusted consumption)", colors)
plot_x <- makePlot_1(x, "x (foraging opinion)", colors)
plot_Px <- makePlot_1(P_x, "P_x", colors)
plot_y <- makePlot_1(y, "y (harvest risk opinion", colors)
plot_Py <- makePlot_1(P_y, "P_y", colors)
plot_Psoc <- makePlot_1(P_social, "P_social", colors)

# Plots of Aggregated 1D data
plot_seals <- makePlot_2(x = 1:days, x.name = "Day", y = colSums(seal_forage_loc, na.rm = T), 
                         y.name = "Num Seals at Gauntlet", color = "turquoise3")
plot_H <- makePlot_2(x = 1:days, x.name = "Day", y = H, y.name = "Seals Harvested", color = "orchid")

