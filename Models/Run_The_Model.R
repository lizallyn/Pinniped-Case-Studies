source("Functions/assembleTheLegos.R")
assembleTheLegos(path_to_pars = "Functions/BaseRun_set_pars.R", 
                 path_to_vars = "Functions/BaseRun_initialize_variables.R")

# assembleFirstHalf(path_to_pars = "Functions/BaseRun_set_pars.R")
# num_seals <- 2
# assembleSecondHalf(path_to_vars = "Functions/BaseRun_initialize_variables.R")
# salmon_escapement
# 
# num_seals <- 50
# assembleSecondHalf(path_to_vars = "Functions/BaseRun_initialize_variables.R")
# salmon_escapement
# 
# 
plot_Px + plot_probs + plot_seals + plot_layout(guides = "collect")

plot_seals/plot_H/plot_y/plot_Py + plot_layout(guides = "collect")

eaten_sp_plot/plot_eaten/plot_C/plot_x/plot_Px + plot_layout(guides = "collect")

gauntlet_plot / eaten_sp_plot / fished_plot / escape_plot + plot_layout(guides = "collect")


# 
# 
# x_over_Px_plot / y_over_Py_plot
# 
# 
# 
# 
# 
# 
# 
# 
# gauntlet_plot / fished_plot
# 
# (gauntlet_plot + theme_classic()) / (plot_seals + theme_classic()) + plot_layout(guides = "collect")

# ggsave(plot = gauntlet_plot, filename = "gauntlet_plot.png", device = "png",
#       path = "Plot Exports", height = 3, width = 8)
