#1 Calculates and return the mean of a pollutant
pollutantmean <- function(directory, pollutant, id = 1:332){
  #list all the files in the specdata directory
  files_full <- list.files(directory, full.names=TRUE) 
  
  #initialize data frame
  dat <- data.frame() 
  
  #bind/merge each file
  for (i in id) {
    dat <- rbind(dat, read.csv(files_full[i]))  
  }
  
  #if pollutant is sulfate, it will suppress observation with a missing(NA) value, then will get the mean of the sulfate with the remaining observations
  if (pollutant == "sulfate") {      
    return (mean(na.omit(dat)$sulfate))  
    
  }
  #if pollutant is nitrate, it will suppress all row with a missing(NA)value, then will get the mean of the nitrate with the remaining observations
  else if (pollutant == "nitrate") { 
    return (mean(na.omit(dat)$nitrate))
  }
}
#pollutantmean function test run
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

#2 Return a data frame  where the first column is the name of the file, and the second column is the number of complete cases
complete <- function(directory, id = 1:332){
  #list all the files in the specdata directory
  files_full <- list.files(directory, full.names=TRUE)
  
  #initialize final data frame
  final_data <- data.frame()
  
  #read each file, create a temporary data frame that will hold the id, and the nobs(total number of success) after suppressing the observation with missing(NA) values
  #and bind it to the final data
  for (i in id) {
    dat <- read.csv(files_full[i]) 
    temp <- data.frame(id = i, nobs = nrow(na.omit(dat)))
    final_data <- rbind(final_data, temp)
  }
  return (final_data)
}
#complete function test run
complete("specdata", 1)
complete("specdata", c(2,4,8,10,12))
complete("specdata", 30:25)
complete("specdata", 3)

#3 Returns a  vector of correlations for the monitors that meet the threshold requirement.
corr <- function(directory, threshold = 0 ){
  #list all the files in the specdata directory
  files_full <- list.files(directory, full.names=TRUE)
  #gets the data frame(id and nods) of the files using the complete function created
  nobs <- complete(directory)
  #get the IDs of the observation wherein the nobs or number of complete cases is greater than the threshold
  id <- (which(nobs["nobs"] > threshold))
  
  result <- c()
  
  #loop for each file. compress missing values and correlate each nitrate and sulfate 
  for (i in id){
    dat <- read.csv(files_full[i]) 
    result <- c(result, cor(na.omit(dat)$nitrate, na.omit(dat)$sulfate))
    
  }
  
  return(result)
}
#corr function test run
cr <- corr("specdata", 150)
head(cr);summary(cr)

cr <- corr("specdata", 400)
head(cr);summary(cr)

cr <- corr("specdata", 5000)
head(cr);summary(cr); length(cr)

cr <- corr("specdata")
head(cr);summary(cr); length(cr)

#4 Shows histogram of the hospital 30-day death (mortality) rate from hear attack
outcome <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], main="Hospital 30-Day Death (Mortality) Rates from Heart Attack")

