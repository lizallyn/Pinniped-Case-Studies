# create decide_foraging_destination function

decideForagingDestination <- function(prob_gauntlet) {
  if(is.na(prob_gauntlet)) {
    seal_forage_dest <- NA
  } else if(runif(1, 0, 1) < prob_gauntlet) {
    seal_forage_dest <- 1
  } else {seal_forage_dest <- 0}
  
  return(seal_forage_dest)
}

