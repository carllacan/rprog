rankhospital <- function(state, outcome, num) {
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
  data[,field] <- as.numeric(data[,field])
  data$ranking <- NA
  ranks <- unique(data[,field])[order(unique(data[,field]))]
  for (r in 1:length(ranks)){
    data[which(data[,field]==ranks[r]),]$ranking <- r
  }
  
  data <- data[order(data[,field], data$hospital.name),]
  
  if (num == "best") {
    return(data[1,]$hospital.name)
  }
  if (num=="worst"){
    return(data[nrow(data),]$hospital.name)
  }
  if (num > nrow(data)){
    return(NA)
  }
  return(data[num,]$hospital.name)
  
}