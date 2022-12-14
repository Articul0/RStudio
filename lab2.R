getMean <- function(folder, type, id = 1:332)  {
  files <- list.files(folder, full.names = TRUE)
  polutions <- data.frame()
  
  for (i in id) {
    polutions <- rbind(polutions, read.csv(files[i]))
  }
  
  return(mean(polutions[, type], na.rm=TRUE))
}

getCompleteObservation <- function(folder, id = 1:332) {
  files <- list.files(folder, full.names = TRUE)
  polutions <- data.frame()
  
  counts <- data.frame(id = integer(), count = integer())
  
  for (i in id) {
    polutions <- na.omit(read.csv(files[i]))

    datum <- data.frame(i, nrow(polutions))
    names(datum) <- c("id", "count")
    
    counts <- rbind(counts, datum)
  }
  
  return(counts)
}

getCorrelation <- function(folder, limen = 0) {
  files <- list.files(folder, full.names = TRUE)
  polutions <- data.frame()
  correlation <- vector()
  
  for (i in seq_along(files)) {
    polutions <- na.omit(read.csv(files[i]))
    
    if (nrow(polutions) > limen) {
      value <- cor(polutions[, "sulfate"], polutions[, "nitrate"])
      correlation <- append(correlation, value)
    }
  }
  
  return(correlation)
}