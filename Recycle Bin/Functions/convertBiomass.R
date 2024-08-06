# for biomass <--> numbers conversions
species.options <- c("Seal", "Steller", "California", "Sockeye", "Chinook", "Coho", "Chum", "Pink", "Steelhead")
unit.options <- c("kg per individual")
LookupTable <- data.frame(matrix(nrow = 9, ncol = 1, data = c(44.7, NA, NA, NA, NA, NA, NA, NA, NA), 
                                 dimnames = list(species.options, unit.options)))

convertBiomass <- function(reference = LookupTable, species, start.value, conversion = c("to kg", "to numbers")){
  if(conversion == "to kg"){
    result <- reference[species,] * start.value
  } else if(conversion == "to numbers"){
    result <- start.value / reference[species,]
  } else {
    print("not a valid conversion option. Try: to kg, to numbers")
  }
  return(result)
}

convertBiomass(reference = LookupTable, species = "Seal", start.value = 5, conversion = "to kg")

