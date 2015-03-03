rankhospital <- function(state, outcome, num) {
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
  
  ## Return hospital name in that state with given rank 30-day death rate
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
  
  ## Get order permutation for lowest to highest, all NA sent to the end:
  perm <- order(deathRates, hospitals, na.last = TRUE, decreasing = FALSE)
  
  if (num == "best") {
    num <- 1
  }
  else if (num == "worst") {
    num <- length(perm)
  }
  else if (!is.numeric(num)) {
    num <- 0
  }
  
  if (length(deathRates) < num || length(deathRates) == 0 || num <= 0) {
    NA
  }
  else {
    hospitals[perm[num]]
  }
}