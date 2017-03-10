library(readr)
library(stats)

#' @param directory is a character vector of length 1 indicating the location of csv file
#' @param pollutant is a character vector of length 1 indicating the name of
#'                  pollutant for which we will calculate the mean:
#'                  "sulfate" or "nitrate"
#' @param id is an integer vector indicating the monitor ID numbers to be used                  
#' 
#' @example 
#'   source("pollutantmean.R")
#'   pollutantmean("specdata", "sulfate", 1:10)
#' @export
complete <- function(directory="specdata", id = 1:332) {
  data <- data.frame( "id" = integer(0), "nobs" = integer(0))

    for (nFile in id) {
       t <- file.path(directory, sprintf("%03d.csv", nFile))
    
       if (file.exists(t)) {
           tmp <- read_csv(t, progress=F)          # Load file
           tmp <- na.omit(tmp)        # remove rows with NA
           data[nrow(data) + 1, ] <- c( nFile, nrow(tmp))
       }
    }
    return (data)
}