complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  # Initialize
  N <- length(id)
  completeData <- data.frame(id = integer(0), nobs = integer(0))
  
  # Get list of files
  files <- list.files(directory, full.name = TRUE)
  files <- files[id]
  
  for (i in 1:N)
  {
    data <- read.csv(files[i])
    
    # Add new row:
    completeData[nrow(completeData) + 1, ] = c(id[i], sum(complete.cases(data), na.rm = TRUE))
  }
  
  completeData
}