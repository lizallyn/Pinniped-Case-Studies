source("Functions/assembleTheLegos.R")
assembleTheLegos("Base")
assembleTheLegos("Experiment", path_to_pars = "Functions/ExcZone_set_pars.R", 
                 path_to_vars = "Functions/ExcZone_initialize_variables.R")

gauntlet_plot / eaten_sp_plot / fished_plot / escape_plot + plot_layout(guides = "collect")

eaten_sp_plot/plot_eaten/plot_C/plot_x/plot_Px + plot_layout(guides = "collect")

plot_seals/plot_H/plot_y/plot_Py + plot_layout(guides = "collect")

plot_Px + plot_Py + plot_probs + plot_layout(guides = "collect")

gauntlet_plot / fished_plot
