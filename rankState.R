validOptions <- list(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack="heart attack", Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure="heart failure", Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia="pneumonia")
toOpen <- "~/Documents/Coursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  cvsData <- readFile()
  ## Check that state and outcome are valid
  checkInputs(cvsData, state, outcome)
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  toOrder<-selectNumSt(cvsData, state, outcome)
  ordered <- order(toOrder[,2], toOrder[,1], decreasing = FALSE)

  if(num == "best"){
    index <- ordered[1]
  }else if (num == "worst") {
    index <- ordered[length(ordered)]
  }else if (!is.numeric(num) | num > length(ordered)) {
    stop("invalid rank")
  } else {
    index <- ordered[num]
  }
  toOrder[index, "Hospital.Name"]
}

selectNumSt <- function(cvsData, state, outcome){
  #which rows are the rows of the state we're interested in?
  stateRows<-cvsData$State==state
  #use the true, false vector generated above to select appropriate rows from the whole table
  stData <- cvsData[stateRows, c("Hospital.Name", names(which(validOptions==outcome)))]
  #cast the outcome we're interested in as a numeric
  stData[,2] <- as.numeric(stData[,2])
  #remove NAs
  stData <- stData[(!is.na(stData[,2])),]
  stData
  
}

readFile <- function() {
  read.csv(file = toOpen, colClasses = "character")
}

checkInputs <- function(cvsData, state, outcome) {
  if(!(state %in% cvsData$State)){ 
    stop("invalid state")  
  }
  if(!(outcome %in% validOptions)) {
    stop("invalid outcome")  
  }
  invisible(TRUE)
}