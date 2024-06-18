source("Functions/assembleTheLegos.R")
assembleTheLegos(path_to_pars = "Functions/BaseRun_set_pars.R", 
                 path_to_vars = "Functions/BaseRun_initialize_variables.R")

plot_Px + plot_probs + plot_seals + plot_layout(guides = "collect")

eaten_sp_plot/plot_eaten/plot_C/plot_x/plot_Px + plot_layout(guides = "collect")

gauntlet_plot / eaten_sp_plot / fished_plot / escape_plot + plot_layout(guides = "collect")


x_over_Px_plot / y_over_Py_plot


plot_seals/plot_H/plot_y/plot_Py + plot_layout(guides = "collect")



gauntlet_plot / fished_plot

(gauntlet_plot + theme_classic()) / (plot_seals + theme_classic()) + plot_layout(guides = "collect")

# ggsave(plot = gauntlet_plot, filename = "gauntlet_plot.png", device = "png",
#       path = "Plot Exports", height = 3, width = 8)
