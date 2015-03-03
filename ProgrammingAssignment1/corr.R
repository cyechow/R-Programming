corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  # Assumes complete.R has been sourced.
  
  # Get list of files
  files <- list.files(directory, full.name = TRUE)
  
  # Retrieve file "complete" information:
  completeData = complete(directory)
  
  # Get files above threshold:
  idx = completeData$nobs > threshold
  
  # Get file subset:
  files = files[idx]
  
  # Set length and initialize array:
  N = sum(idx, na.rm = TRUE)
  cr = rep(NA,N)
  
  if (N == 0)
  {
    cr <- numeric(0)
  }
  else
  {
    # Loop through files and  calculate correlation:
    for (i in 1:N)
    {
      data = read.csv(files[i])
      cr[i] = cor(data$sulfate[complete.cases(data)], data$nitrate[complete.cases(data)])
    }
  }

  cr
}