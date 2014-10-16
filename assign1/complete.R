complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  formerdir <- getwd()
  setwd(directory)
  getfn = function(num) paste(c(rep("0", 3-nchar(num)), num, ".csv"), collapse="", sep="")
  files <- lapply(id, function(num) read.csv(getfn(num)))  
  setwd(formerdir)
  
  obs = lapply(files, function(df) df[which(!is.na(df$sulfate)&!is.na(df$nitrate)),])
  nobs = (sapply(obs, nrow))
  
  return(data.frame(id, nobs))
}