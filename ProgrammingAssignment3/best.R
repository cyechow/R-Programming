best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states = data$State
  if (!is.element(state, states)) {
    stop("invalid state")
  }
  
  if (tolower(outcome) != "heart attack" && tolower(outcome) != "heart failure" && tolower(outcome) != "pneumonia") {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  if (tolower(outcome) == "heart attack") {
    deathRates <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  }
  else if (tolower(outcome) == "heart failure") {
    deathRates <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  }
  else {
    deathRates <- suppressWarnings(as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  }
  
  deathRates <- deathRates[states == state]
  hospitals <- data$Hospital.Name[states == state]
  
  if (length(deathRates) == 0) {
    NA
  }
  else {
    hospitals[which.min(deathRates)]
  }
  
}