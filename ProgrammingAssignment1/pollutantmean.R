pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  # Initialize
  N <- length(id)
  meanValues <- numeric(0)
  
  # Get list of files
  files <- list.files(directory, full.names = TRUE)
  files <- files[id]
  
  # Loop through files and read each:
  for (i in 1:N)
  {
    data <- read.csv(files[i])
    # Get all values of the pollutant specified:
    meanValues <- c(meanValues, data[,pollutant])
  }

  # Calculate the mean:
  mean(meanValues, na.rm = TRUE)
}