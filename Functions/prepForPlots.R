

prepForPlots <- function(df, key.col = "Seal", 
                         other.cols = "Day", value.col){
  melted <- melt(data = df, key.col)
  colnames(melted) <- c(key.col, other.cols, value.col)
  melted[,key.col] <- as.factor(melted[,key.col])
  return(melted)
}
