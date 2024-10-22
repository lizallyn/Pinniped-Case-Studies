# Steller plots

# Plots of Individuals
plot_probs_ej <- makePlot_1(ej_prob_gauntlet, "Gauntlet Probabilities SSL", colors)
plot_eaten_ej <- makePlot_1(salmon_consumed_ej, "Salmon Eaten per SSL", colors)
plot_C_ej <- makePlot_1(C_ej, "C (adjusted consumption) SSL", colors)
plot_x_ej <- makePlot_1(x_ej, "x (foraging opinion) SSL", colors)
plot_Px_ej <- makePlot_1(P_x_ej, "P_x SSL", colors)
plot_y_ej <- makePlot_1(y_ej, "y (harvest risk opinion) SSL", colors)
plot_Py_ej <- makePlot_1(P_y_ej, "P_y SSL", colors)
plot_Psoc_ej <- makePlot_1(P_social_ej, "P_social SSL", colors)

# Plots of Aggregated 1D data
plot_ej <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = colSums(ej_forage_loc, na.rm = T), 
                         y.name = "Stellers at Gauntlet", color = "turquoise3")
plot_H_ej <- makePlot_2(x = 1:days + (start_loop - 1), x.name = "Day", y = H_ej, y.name = "Stellers Harvested", color = "orchid")

