# create decide_foraging_destination function

decide_foraging_destination <- function(prob_gauntlet) {
  if(runif(1, 0, 1) < prob_gauntlet) {
    seal_forage_dest <- 1
  } else seal_forage_dest <- 0
  return(seal_forage_dest)
}


