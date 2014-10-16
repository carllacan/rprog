corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  formerdir = getwd()
  setwd(directory)
  getfn = function(num) paste(c(rep("0", 3-nchar(num)), num, ".csv"), collapse="", sep="")
  files <- lapply(1:332, function(num) read.csv(getfn(num)))  
  setwd(formerdir)
  
  source("complete.R")
  nobs = complete(directory)
  comp = files[which(nobs$nobs>threshold)]
  if (!length(comp)) {
    setwd(formerdir)
    return(c())
  }
  data = lapply(comp, function(df) cbind(df$sulfate, df$nitrate))
  cors = sapply(data, function(df) cor(df, use="pairwise.complete.obs")[[1,2]])
  
  return(cors)
}