# California PLOTS

# Plots of Individuals
plot_probs_zc <- makePlot_1(zc_prob_gauntlet, "Gauntlet Probabilities CSL", colors)
plot_eaten_zc <- makePlot_1(salmon_consumed_zc, "Salmon Eaten per CSL", colors)
plot_C_zc <- makePlot_1(C_zc, "C (adjusted consumption) CSL", colors)
plot_x_zc <- makePlot_1(x_zc, "x (foraging opinion) CSL", colors)
plot_Px_zc <- makePlot_1(P_x_zc, "P_x CSL", colors)
plot_y_zc <- makePlot_1(y_zc, "y (harvest risk opinion) CSL", colors)
plot_Py_zc <- makePlot_1(P_y_zc, "P_y CSL", colors)
plot_Psoc_zc <- makePlot_1(P_social_zc, "P_social CSL", colors)

# Plots of Aggregated 1D data
plot_zc <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = colSums(zc_forage_loc, na.rm = T), 
                      y.name = "Californias at Gauntlet", color = "turquoise3")
plot_H_zc <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = H_zc, y.name = "Californias Harvested", color = "orchid")

