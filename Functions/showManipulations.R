# to shorten manipulation code

# For Nisqually
showManipulations <- function(loc, output, base_val, colors = salmon_colors) {
  output <- data.frame(output)
  
  if(loc == "Nisqually"){
    plot_escape_manipulations <- ggplot(data = output, aes(x = output[,1])) +
      geom_line(aes(y = Escape[,"Chum"]), color = colors["Chum"], lwd = 1.2) + 
      geom_line(aes(y = Escape[,"Chinook_GR"]), color = colors["GR"], lwd = 1.2) + 
      geom_line(aes(y = Escape[, "Chinook_LN"]), color = colors["LN"], lwd = 1.2) +
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
    
  } else if(loc == "Ballard"){
    
    plot_escape_manipulations <- ggplot(data = output, aes(x = output[,1])) +
      geom_line(aes(y = Escape[,"Sockeye"]), color = colors["Sockeye"], lwd = 1.2) + 
      geom_line(aes(y = Escape[,"Chinook"]), color = colors["Chinook"], lwd = 1.2) + 
      geom_line(aes(y = Escape[, "Coho"]), color = colors["Coho"], lwd = 1.2) +
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
    
  } else {
    
    return("Not a valid loc. Try Nisqually or Ballard")
    
  }
}

# showManipulations(loc = "Nisqually", output = Escape, base_val = 0.1, colors = salmon_colors)[[1]]
# 
# showManipulations <- function(output, base_val, colors) {
#   output <- data.frame(output)
#   
#   plot_escape_manipulations <- ggplot(data = output, aes(x = output[,1])) +
#     geom_line(aes(y = Escape[,"Chum"]), color = colors["Chum"], lwd = 1.2) + 
#     geom_line(aes(y = Escape[,"Chinook_GR"]), color = colors["GR"], lwd = 1.2) + 
#     geom_line(aes(y = Escape[, "Chinook_LN"]), color = colors["LN"], lwd = 1.2) +
#     geom_vline(xintercept = base_val, lwd = 2, color = "gray", linetype = 3) +
#     labs(y = "Salmon Escaped", x = "Parameter Value") +
#     theme_classic()
#   
#   escape_4_tab <- rbind(output[1,], output[nrow(output),])
#   escape_4_tab <- escape_4_tab %>% rename_at(colnames(escape_4_tab)[1], ~ "Run")
#   escapement_table <- rbind(base_salmon_escapement, escape_4_tab)
#   
#   returnables <- list()
#   returnables[[1]] <- plot_escape_manipulations
#   returnables[[2]] <- kable(escapement_table, caption = "Escaped Salmon") %>% 
#     kable_styling(full_width = F, font_size = font_size)
#   return(returnables)
# }