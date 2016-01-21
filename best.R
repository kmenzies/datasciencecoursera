

best <- function(state, outcome) {
  validOptions <- list(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack="heart attack", Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure="heart failure", Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia="pneumonia") 
  ## Read outcome data
  cvsData <- read.csv("~/Documents/Coursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(!(state %in% cvsData$State)){ 
    stop("invalid state")  
  }
  if(!(outcome %in% validOptions)) {
    stop("invalid outcome")  
  }
  
  ## Return hospital name in that state with lowest 30-day death
  stateRows<-cvsData$State==state
  data2 <- cvsData[stateRows, ]
  
  findMin <- as.numeric(data2[,names(which(validOptions==outcome))])
  index2 <- which(findMin == min(findMin, na.rm = TRUE))
  
  ## rate
  names<-data2$Hospital.Name[index2]
  names[which(names==max(names))]
}