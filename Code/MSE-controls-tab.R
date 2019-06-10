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


# ####################################################################
# # Narrative building from structured data 
