

updateNormal <- function(salmon_consumed, w, hunting, x_t, y_t, forage_loc, 
                         bundle_x_pars, bundle_y_pars, dead, baseline_x, baseline_y,
                         specialist){
  C <- salmon_consumed - w
  
  d_x <- learnX(food = C, x_t = x_t, forage_loc = forage_loc, bundle_x_pars, 
                dead = dead, baseline = baseline_x)
  d_y <- learnY(hunting = hunting, y_t = y_t, forage_loc = forage_loc, 
                bundle_y_pars, dead = dead, baseline = baseline_y)
  
  x_t1 <- x_t + d_x
  y_t1 <- y_t + d_y
  
  if(specialist == T){
    P_x[seal, t+1] <- 1-(1/((1+buffer_Pxmin_specialist) + exp(-steepness * (threshold_x_specialist - x[seal, t+1]))))
  }
  
  return(d_y)
}

updateNormal(salmon_consumed = salmon_consumed_pv[seal, t], w = w, hunting = H[t], 
             x_t = x[seal, t], y_t = y[seal, t],
             forage_loc = seal_forage_loc[seal, t], bundle_x_pars = bundle_x_pars,
             bundle_y_pars = bundle_y_pars, dead = seal %in% kill_list, 
             baseline_x = baseline_x[seal], baseline_y = baseline_y[seal], 
             specialist = seal %in% specialist_seals)
