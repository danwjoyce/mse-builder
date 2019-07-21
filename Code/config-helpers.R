# functions to load and save config

Config.ReadFile <- function() {
  config.file <- read.delim("./config.txt", header = FALSE, 
                             sep = " ", 
                             stringsAsFactors = FALSE)
  names(config.file) <- c("Key","Value")
  return( config.file)
}

Config.WriteFile <- function() {
  filecon <- file( "./config.txt", "w")
  on.exit( close( filecon ) )
  for( i in 1:nrow(config.file) ) {
    writeLines( paste( config.file[i,1], config.file[i,2] ), con = filecon, sep = "\n" )
  }
  close(filecon)
}

Config.GetValue <- function( k ) {
  return( 
    type.convert(
      trimws( with( config.file, Value[ Key == k ] ) ),
      as.is = TRUE
    )
  )
}

Config.SetValue <- function( k, v ) {
 config.file$Value[ config.file$Key == k ] <<- v
}
