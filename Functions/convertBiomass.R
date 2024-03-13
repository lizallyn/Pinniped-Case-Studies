# for biomass <--> numbers conversions
species.options <- c("Seal", "Steller", "California", "Sockeye", "Chinook", "Coho", "Chum", "Pink", "Steelhead")
unit.options <- c("tons per individual")
LookupTable <- data.frame(matrix(nrow = 9, ncol = 1, data = 1:9, dimnames = list(species.options, unit.options)))

convertBiomass <- function(reference = LookupTable, species, start.value, conversion = c("to tons", "to numbers")){
  if(conversion == "to tons"){
    result <- reference[species,] * start.value
  } else if(conversion == "to numbers"){
    result <- start.value / reference[species,]
  } else {
    print("not a valid conversion option. Try: to tons, to numbers")
  }
  return(result)
}

convertBiomass(reference = LookupTable, species = "Steller", start.value = 5, conversion = "to numbers")

