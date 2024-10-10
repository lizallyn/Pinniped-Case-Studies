# to shorten manipulation code

showManipulations <- function(output, base_val) {
  output <- data.frame(output)
  
  plot_escape_manipulations <- ggplot(data = output, aes(x = output[,1])) +
    geom_line(aes(y = Chum), color = "seagreen", lwd = 1.2) + 
    geom_line(aes(y = Chinook_GR), color = "goldenrod", lwd = 1.2) + 
    geom_line(aes(y = Chinook_LN), color = "salmon", lwd = 1.2) +
    geom_vline(xintercept = base_val, lwd = 2, color = "gray", linetype = 3) +
    labs(y = "Salmon Escaped", x = "Parameter Value") +
    theme_classic()
  
  escape_4_tab <- rbind(output[1,], output[nrow(output),])
  escape_4_tab <- escape_4_tab %>% rename_at(colnames(escape_4_tab)[1], ~ "Run")
  escapement_table <- rbind(base_salmon_escapement, escape_4_tab)
  
  returnables <- list()
  returnables[[1]] <- plot_escape_manipulations
  returnables[[2]] <- kable(escapement_table, caption = "Escaped Salmon") %>% 
    kable_styling(full_width = F, font_size = font_size)
  return(returnables)
}

