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
  
  setwd(directory)
  getfilename = function(num) paste(paste(rep("0", 3-nchar(num)),collapse=""), num, ".csv", sep="")
  files <- lapply(id, function(num) read.csv(getfilename(num)))  
  obs = files[which(!is.na(files$sulfate)&!is.na(files$nitrate)),]
  obs_num = sum(sapply(obs, length))