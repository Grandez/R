
best <- function(state, outcome) {
  return (rankhospital(state, outcome, "best"))
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- loadFile()

  ## Check that state and outcome are valid
  ## 1.- Check if character
  if(!is.character(state))   stop("invalid state")
  if(!is.character(outcome)) stop("invalid outcome")

  
  #browser()
  if(is.character(num)) {
    orden = switch(num, "best"  = 1, "worst" = 0 )
  }  
  else {
    if (!is.numeric(num)) {
      orden <- NULL
    } else {
      if (num < 1) {
        orden <- NULL   
      }
      orden <- num
    }    
  }
  
  if (is.null(orden))   stop("invalid num")
  
  
  ## when outcome is invalid, index is set to null  
  index = switch(outcome, 
                 "heart attack"  = 11, 
                 "heart failure" = 17, 
                 "pneumonia"     = 23
                 )
  if (is.null(index)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Only select the Hospital name and the column associate to data
  columns <- c("Hospital.Name", as.character(colnames(data)[index]))
  
  ## Filter by state
  ## if no data then it is an invalid state
  df <- subset(data, State == state, select = columns)
  if (nrow(df) == 0) stop("invalid state")
  
  # set friendly column names
  colnames(df) <- c("NAME", "VALUE")
  
  # remove NA
  df <- df[!is.na(df$VALUE),]
  
  
  dfs <- df[order(df$VALUE,df$NAME),]

  if (orden == 0) 
    orden <- nrow(dfs)
  
  res <- dfs[orden,1]
  return (res)
}


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- loadFile()
  
  ## Check that state and outcome are valid
  ## 1.- Check if character
  if(!is.character(outcome)) stop("invalid outcome")
  
  if(is.character(num)) {
    orden = switch(num, "best"  = 1, "worst" = 0 )
  }  
  else {
    if (!is.numeric(num)) orden <- NULL
    if (num < 1) orden <- NULL
    orden = num
  }
  
  if (is.null(orden))   stop("invalid num")
  
  ## when outcome is invalid, index is set to null  
  index = switch(outcome, 
                 "heart attack"  = 11, 
                 "heart failure" = 17, 
                 "pneumonia"     = 23
  )
  if (is.null(index)) stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Only select the Hospital name and the column associate to data
  columns <- c("State", "Hospital.Name", as.character(colnames(data)[index]))

  #browser()
  ## Cogemos solo las columnas que nos interesa
  df <- subset(data, select = columns)
  colnames(df) <- c("STATE", "hospital", "VALUE")
  
  # Quitar NA
  df <- df[!is.na(df$VALUE),]
  

  # Partimos por estado
  dfss <- split(df, f = df$STATE)

  
  count = 0
  for (st in dfss) {
    aux <- as.data.frame(st)
    
    #Ordenar
    aux <- aux[order(aux$VALUE),]
    
    pos <- if (orden == 0) nrow(aux) else orden

    if (count == 0) {
      res <- aux[pos,]
    }
    else {
      res <- rbind(res, aux[pos,])
    }  
    count <- count + 1
  }
  
  return (res)
}

loadFile <- function(file.name="outcome-of-care-measures.csv") {
  oldw <- getOption("warn")
  options(warn = -1)
  #browser()
#  if (!exists("my.df") | is.null(my.df)) {
      my.df <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      my.df[, 11] <- as.numeric(my.df[, 11])
      my.df[, 17] <- as.numeric(my.df[, 17])
      my.df[, 23] <- as.numeric(my.df[, 23])
#  }
  options(warn = oldw)
  return(my.df)
  
}
