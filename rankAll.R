validOptions <- list(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack="heart attack", Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure="heart failure", Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia="pneumonia")
toOpen <- "~/Documents/Coursera/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv"

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  cvsData <- readFile()
  ## Check that outcome is valid
  if(!(outcome %in% validOptions)) {
    stop("invalid outcome")  
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  toOrder<-selectOutcome(cvsData, outcome)
  byState<-split(toOrder, toOrder$State)
  t(sapply(byState, function(foo) orderAndRank(foo, rank=num)))
  
}

orderAndRank <- function (data, rank){
  
  ordered <- order(data[,2], data[,1], decreasing = FALSE)
  
  if(rank == "best"){
    index <- ordered[1]
  }else if (rank == "worst") {
    index <- ordered[length(ordered)]
  } else {
    index <- ordered[rank]
  }
  data[index,c(1,3)]
}

selectOutcome <- function(cvsData, outcome){
  #select appropriate rows from the whole table
  outcomeData <- cvsData[, c("Hospital.Name", names(which(validOptions==outcome)), "State")]
  #cast the outcome we're interested in as a numeric
  outcomeData[,2] <- as.numeric(outcomeData[,2])
  outcomeData <- outcomeData[(!is.na(outcomeData[,2])),]
  outcomeData
  
}

readFile <- function() {
  read.csv(file = toOpen, colClasses = "character")
}

