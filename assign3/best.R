best <- function (state, outcome) {
  
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  if (!(state %in% data$State)){
      stop("invalid state")
  }
  if  (!(outcome %in% c("heart failure", "heart attack", "pneumonia"))) {
    stop("invalid outcome")
  }
      
  
  colnames(data) <- tolower(colnames(data))
  outcome <- sub(" ", ".", outcome)
  
  field <- paste("hospital.30.day.death..mortality..rates.from.", outcome, sep="")
  data <- data[which(data$state==state & data[,field]!="Not Available"),]
  
  bests <- data[which.min(as.numeric(data[,field])),]$hospital.name

  return( bests[order(bests)])
  
          
}
