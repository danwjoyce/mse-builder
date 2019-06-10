# Helper functions for MSE app

DataFrameRowEmpty <- function(row.df) {
  # Takes a SINGLE row of a data.frame and returns TRUE is all columns are empty
  return( 
    all( apply(row.df, 2, function(x) all(x=="")) )
  )
}


HumaniseDate <- function( this.date ) {
  # Takes a Date object e.g. Sys.Date and humanises the string
  # e.g. "2019-3-13" is "13th March 2019"
  # day of month
  this.day <- as.numeric( format(as.Date(this.date,format="%Y-%m-%d"), "%d") )
  # base string
  date.str <- format(this.date, format="%B %Y")
  # convert day to "st", "rd", "th" etc.
  day.str <- scales::ordinal(this.day, rules = ordinal_english())
  return( paste( day.str, date.str ) )
}

HumaniseTime <- function( this.date ) {
  format( this.date, "%H:%M")
}

SortIndexTable <- function( index.df ) {
  # Sort index.df on date then time
  return( index.df[ order(index.df$MSE.date, index.df$MSE.time, decreasing = TRUE), ] )
}

HumaniseIndexTable <- function( index.df ) {
  # Pretty print for display
  if( nrow(index.df) == 0 ) {
    temp <- index.df[ , 1:(ncol(index.df)-1) ]  # ditch hashed filename
  } else {
    temp <- data.frame(
      Unique.ID = index.df$UniqueID,
      Forename = index.df$Forename,
      Surname = index.df$Surname,
      MSE.Date = HumaniseDate( index.df$MSE.date ),
      Time = HumaniseTime( index.df$MSE.time )
    )
  }
  colnames(temp) <- c("Unique ID", "Forename", "Surname", "MSE Date", "Time")
  return( temp )
}

######################################  
# Helpers for indexing MSEs and patients

CreateIndex <- function() {
  data.frame(
    UniqueID = character(),
    Forename = character(),
    Surname = character(),
    MSE.date = as.Date(character()),
    MSE.time = as.POSIXct(character()),
    Hash.fname = character(),
    stringsAsFactors = FALSE)
}

AddToIndex <- function(input, index.df, fname.hash) {
  new.pt <- data.frame( UniqueID = as.character( input$Demog.uID ),
                        Forename = as.character( input$Demog.Forename ),
                        Surname  = as.character( input$Demog.Surname ),
                        MSE.date = as.Date( input$Demog.MSEDate ),
                        MSE.time = as.POSIXct( paste0( as.character(input$Demog.MSEDate ), " ", input$Demog.MSETime ), tz = "GMT",
                                               format = "%Y-%m-%d %H:%M" ),
                        Hash.fname = as.character( fname.hash ),
                        stringsAsFactors = FALSE)
  
  return( SortIndexTable( rbind( index.df, new.pt ) ) )
}

UpdateIndexEntry <- function(index.df, current.MSE, current.hash ) {
  # update the date and time of this index row according to current MSE data
  index.row <- which( index.df$Hash.fname == current.hash )
  index.df$MSE.date[ index.row ] <- current.MSE$Demog.MSEDate
  index.df$MSE.time[ index.row ] <- as.POSIXct( paste0( as.character(current.MSE$Demog.MSEDate ), " ", current.MSE$Demog.MSETime ), tz = "GMT",
              format = "%Y-%m-%d %H:%M" )
  
  # return the sorted index table 
  return( SortIndexTable( index.df ) )
  
}
  

RefreshPatientTable <- function(index.df) {
  DT::renderDataTable( DT::datatable( HumaniseIndexTable( index.df ), selection = 'single', options = list( pageLength = 50)  ) )
}

GenerateFilename <- function( this.mse ) {
  return( sprintf(
      "%s_%s.rds", 
      as.integer(Sys.time()), 
      digest::digest( this.mse )
    )
  )
}

ScrapeData <- function(input, fields) {
  # put variables in a data frame
  data <- data.frame(matrix(nrow=1,ncol=0))
  print(fields)
  for (x in fields) {
    var <- input[[x]]
    if (length(var) > 1 ) {
      # handles lists from checkboxGroup and multiple Select
      data[[x]] <- list(var)
    } else {
      # all other data types
      data[[x]] <- var
    }
  }
  return( data )
  
}


