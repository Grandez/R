
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
pollutantmean <- function(directory="specdata", pollutant="sulfate", id = 1:332) {
  count <- 0
  
  #browser()
  for (nFile in id) {
    t <- file.path(directory, sprintf("%03d.csv", nFile))

    tmp <- read_csv(t, progress=F)          # Load file
    tmp <- na.omit(tmp)        # remove rows with NA
  
    if (count == 0) {
      data <- tmp
    }
    else {
      data <- rbind(data, tmp)
    }
    count <- count + 1
  }
  mean(data[[pollutant]])
}