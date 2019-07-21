# Controls Tab Code

####################################################################
# UI Fields : to scrape from form
fields.control <- c("MSEnewpt", "MSEclear", "MSErestore", "MSEupdate", "MSEsave")  


#############################################################
# Data Entry Codes for Fields
# Not relevant 

####################################################################
# Control Tab UI Definition
MSE.tab.Controls <- function() {
  box(
    title = "Controls", status = "warning", width = NULL, collapsible = FALSE, solidHeader = TRUE,
    column(2, align="center",
           actionButton("MSEnewpt", "New Patient")
    ),
    
    column(2, align="center",
           actionButton("MSEclear", "Normal MSE")
    ),
    column(2, align="center",
           actionButton("MSErestore", "Undo")
    ),
    column(2, align="center",
           actionButton("MSEupdate", "Update MSE")
    ),
    column(2, align="center",
           actionButton("MSEsave", "Save As New MSE")
    )
  )
}

####################################################################
# Validation functions

####################################################################
# Functionality for actionButtons
LoadMSE <- function(input, this.fname) {
  # load a saved patient MSE
  if( file.exists(this.fname) ) {
    this.mse   <- readRDS( this.fname )
  } else {
    print(" Fatal Error : LoadMSE : indexed file does not exist in pts directory")
    stop()
  }
  return( this.mse )
}

SaveMSE <- function( this.MSE, this.fname ){
  saveRDS( this.MSE, this.fname )
}

####################################################################
# Server Code
Controls.tab.Server <- function( input, output, session, fields ) {
  observeEvent(input$MSEnewpt, {
    # This resets the MSE and clears all demographics for a "brand new" patient + MSE
    # ensure no fields are disabled
    shinyjs::enable("Demog.Forename")
    shinyjs::enable("Demog.Surname")
    shinyjs::enable("Demog.uID")
    
    RestoreForm(session, NULL, clear.demog = TRUE)    
    current.scope.MSE <<- ScrapeData(input, fields)
    current.scope.fname <<- NULL
    current.scope.fname.hash <<- NULL
  })
  
  # With a loaded MSE, update according to current form edits
  observeEvent(input$MSEupdate, {
    # Update this patient's MSE from form data - for when you load an MSE, make changes and update the MSE
    # rather than create a new MSE for the same patient
    
    # 1 - check that we have an MSE loaded
    if( is.null( current.scope.fname ) ) {
      showModal(modalDialog(
        title = "Error",
        "No MSE loaded, so can't update"
      ))
    } else {
      
      # 2 - scrape and save (overwriting)
      current.scope.MSE <<- ScrapeData(input, fields)
      SaveMSE( current.scope.MSE, current.scope.fname )
      # 3 - update the index
      index.df <<- UpdateIndexEntry(index.df, current.scope.MSE, current.scope.fname.hash )
      saveRDS( index.df, index.path )
      # 4 - force refresh of patients tab
      output$pts.table <- RefreshPatientTable(index.df)
    }
    
  })
  
  # Normal MSE Button
  observeEvent(input$MSEclear, {
    # Reset form / "normal" MSE -- this can be invoked on e.g. the currently loaded patient MSE
    RestoreForm(session, NULL, clear.demog = FALSE)
  })
  
  # "Undo"
  observeEvent(input$MSErestore, {
    # restore the fields from current.scope.MSE -- essentially, "undo" any changes on the current form
    RestoreForm( session, current.scope.MSE, clear.demog = FALSE )
  })
  
  # "Save As New MSE"
  observeEvent(input$MSEsave, {
    #  validate form, save, update index.df and store this MSE as the current global MSE
    if( MSE.tab.ValidateDemographics(input, output) ) {  
      
      # First, check that the date is different from that of the current.scope.MSE
      # i.e. if the user intended to save a new MSE for this patient with the same date and time
      temp.mse.date <- input$Demog.MSEDate
      temp.mse.time <- input$Demog.MSETime
      
      # 1)  then scrape data 
      current.scope.MSE <<- ScrapeData(input, fields)
      # DEBUGGING ONLY - remove later
      assign( "current.scope.MSE", current.scope.MSE, envir = .GlobalEnv)
      ###############################
      
      # 2) generate a unique filename
      fname.hash <- GenerateFilename( current.scope.MSE )
      fname <- paste0( data.path, fname.hash )
      SaveMSE( current.scope.MSE, fname )
      
      # update the index table, and save
      index.df <<- AddToIndex(input, index.df, fname.hash)
      saveRDS( index.df, index.path )
      # refresh the patient's table from index.df
      output$pts.table <- RefreshPatientTable(index.df)
      
      # set the global current.scope.fname.hash
      current.scope.fname.hash <<- fname.hash
    }
  })
}
