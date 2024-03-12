# salmon species composition proportions for a given day

salmonSpeciesComp <- function(chinook = 0, sockeye = 0, coho = 0, chum = 0, pink = 0, steelhead = 0) {
  sockeye_prop <- sockeye/total
  chinook_prop <- chinook/total
  coho_prop <- coho/total
  chum_prop <- chum/total
  pink_prop <- pink/total
  steelhead_prop <- steelhead/total
  
  return(data.frame(Chinook = chinook_prop,
                    Sockeye = sockeye_prop,
                    Coho = coho_prop,
                    Chum = chum_prop,
                    Pink = pink_prop,
                    Steelhead = steelhead_prop))
}