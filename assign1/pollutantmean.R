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
  formerdir <- getwd()
  setwd(directory)

  getfn = function(num) paste(c(rep("0", 3-nchar(num)), num, ".csv"), collapse="", sep="")
  files <- lapply(id, function(num) read.csv(getfn(num)))  
  setwd(formerdir)
  
  pms = sapply(files, "[[", pollutant)
  pms = lapply(pms, function(l) l[!is.na(l)])
  globalsum = sum(sapply(pms, sum))
  globalcount = sum(sapply(pms, length))
  
  return(globalsum/globalcount)
}