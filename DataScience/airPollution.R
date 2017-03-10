library(readr)
library(stats)


loadFile <- function(directory="specdata", id = 0, remove=FALSE) {
  
  columns <- cols(col_date(format = ""), col_double(), col_double(), col_integer())
                  
  t <- file.path(directory, sprintf("%03d.csv", id))
  
  if (file.exists(t)) {    
    print(t)
    tmp <- read_csv(t, col_types = columns, progress=F)    # Load file
    if (remove) {
      tmp <- na.omit(tmp)
    }
    return(tmp)
  }  
  return(NULL)
}

#' @param directory is a character vector of length 1 indicating the location of csv file
#' @param id is an integer vector indicating the monitor ID numbers to be used                  
#' 
loadFiles <- function(directory="specdata", id = 1:332) {
  count <- 0
  
  for (nFile in id) {
    tmp <- loadFile(directory, nFile)
    if (!is.null(data)) {
        if (count == 0) {
          data <- tmp
        }
        else {
          data <- rbind(data, tmp)
       }
       count <- count + 1
    }
  }
  return(data)
}


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
    data <- loadFiles(directory, id)
    mean(data[[pollutant]], na.rm=T)
}

#' Return a data frame of th form:
#' id    nobs
#' nnn   nnnn
#' where 'id' is th monitor id number and 'nobs' is the number of complete cases
#' 
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
    tmp <- loadFile(directory, nFile)
    if (!is.null(tmp)) {
      tmp <- na.omit(tmp)        # remove rows with NA
      data[nrow(data) + 1, ] <- c( nFile, nrow(tmp))
    }
  }
  return (data)
}

#' Write a function that takes a directory of data files and a threshold for complete cases
#' and calculates the correlation between sulfate and nitrate for monitor locations 
#' where the number of completely observed cases (on all variables) 
#' is greater than the threshold. The function should return a vector of correlations 
#' for the monitors that meet the threshold requirement. 
#' If no monitors meet the threshold requirement, 
#' then the function should return a numeric vector of length 0
#' 
#' @param directory is a character vector of length 1 indicating the location of csv file
#' @param threshold is a character vector of length 1 indicating the number of completely
#'                  observed observations (on all variables) required to compute 
#'                  the correla tion
#'                  pollutant for which we will calculate the mean:
#'                  "sulfate" or "nitrate"
#' @param id is an integer vector indicating the monitor ID numbers to be used                  
#' 
#' @example 
#'   source("pollutantmean.R")
#'   pollutantmean("specdata", "sulfate", 1:10)
#' @export
corr <- function(directory="specdata", threshold = 0) {

  res <- vector('numeric')
  all <- complete(directory)              # Identificar el numero de observaciones completas
  th  <- subset(all, nobs > threshold)    # Seleccionar los que cumplen el threshold 
  
  
  for (id in th[["id"]]) {
    data <- loadFile(directory, id, TRUE)
    if (!is.null(data)) {
      c <- cor(data[["sulfate"]], data[["nitrate"]])
      res <- c(res, c)
    }
  }
  return (res)
}
