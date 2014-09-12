pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  setwd(directory)
  getfilename = function(num) paste(paste(rep("0", 3-nchar(num)),collapse=""), num, ".csv", sep="")
  files <- lapply(id, function(num) read.csv(getfilename(num)))  
  pms = sapply(files, "[[", pollutant)
  pms = lapply(pms, function(l) l[!is.na(l)])
  globalsum = sum(sapply(pms, sum))
  globalcount = sum(sapply(pms, length))
  return(globalsum/globalcount)
  
}