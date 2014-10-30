rankall <- function(outcome, num="best") {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if  (!(outcome %in% c("heart failure", "heart attack", "pneumonia"))) {
    stop("invalid outcome")
  }
  
  colnames(data) <- tolower(colnames(data))
  outcome <- sub(" ", ".", outcome)
  field <- paste("hospital.30.day.death..mortality..rates.from.", outcome, sep="")
  data[,field] <- as.numeric(data[,field])
  
  
  result <- data.frame()
  r <- function(s){
    hospitals <- data[which(data$state==s & !is.na(data[,field])),]
    hospitals <- hospitals[order(hospitals[,field], hospitals$hospital.name),]
    if (num == "best") {
      return(hospitals[1,]$hospital.name)
    }
    if (num=="worst"){
      return(hospitals[nrow(hospitals),]$hospital.name)
    }
    if (num > nrow(hospitals)){
      return(NA)
    }
    return(hospitals[num,]$hospital.name)
  }
  state <- unique(data$state)
  state <- state[order(state)]
  hospital <- sapply(state, r)
  
  return(data.frame(hospital, state))
  
}