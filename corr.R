corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  lastwd = getwd()
  setwd(directory)
  getfilename = function(num) paste(paste(rep("0", 3-nchar(num)),collapse=""), num, ".csv", sep="")
  files <- lapply(id, function(num) read.csv(getfilename(num)))  
  setwd(lastwd)
  
  source("complete.R")
  nobs = complete(directory)
  comp = files[which(nobs$nobs>threshold)]
  data = lapply(comp, function(df) cbind(df$sulfate, df$nitrate))
  cors = sapply(data, function(df) cor(df, use="pairwise.complete.obs")[[1,2]])  
}