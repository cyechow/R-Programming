rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  ## Check that outcome is valid:
  if (tolower(outcome) != "heart attack" && tolower(outcome) != "heart failure" && tolower(outcome) != "pneumonia") {
    stop("invalid outcome")
  }
  
  ## Initialize data frame:
  rankedHospitals <- data.frame(hospital = as.character(), state = as.character(), stringsAsFactors = FALSE)
  
  ## Get the death rates:
  if (tolower(outcome) == "heart attack") {
    deathRates <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  }
  else if (tolower(outcome) == "heart failure") {
    deathRates <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  }
  else {
    deathRates <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  }
  
  ## For each state, find the hospital of the given rank
  counter <- 0 ## Setting up row counter
  stateCol <- data$State
  states <- sort(as.character(unique(stateCol)))
  for (state in states) {
    stateDeathRates <- deathRates[stateCol == state]
    hospitals <- data$Hospital.Name[stateCol == state]
    
    ## Get order permutation for lowest to highest, remove all NA:
    perm <- order(stateDeathRates, hospitals, na.last = NA, decreasing = FALSE)
    
    if (num == "best") {
      N <- 1
    }
    else if (num == "worst") {
      N <- length(perm)
    }
    else if (!is.numeric(num)) {
      N <- 0
    }
    else {
      N <- num
    }
    
    if (length(stateDeathRates) < N || length(stateDeathRates) == 0 || N <= 0) {
      newRow <- c("<NA>", state)
    }
    else {
      newRow <- c(hospitals[perm[N]], state)
    }
    
    rankedHospitals[seq(counter + 1, nrow(rankedHospitals) + 1), ] <- newRow
    row.names(rankedHospitals)[counter + 1] <- state
    counter <- counter + 1
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  rankedHospitals
}