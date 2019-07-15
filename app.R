rm( list = ls() )
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(scales)
source("./Code/MSE-demographics-tab.R")
source("./Code/MSE-appearance-behaviour-tab.R")
source("./Code/MSE-speech-tab.R")
source("./Code/MSE-controls-tab.R")
source("./Code/helper-functions.R")

##########################

  index.path <- "./pts/index.rds"
  data.path  <- "./pts/"

  
# aggregate list of fields to scrape
  fields <- c( fields.demog, fields.appbeh, fields.speech )




  RestoreForm <- function(session, current.MSE, clear.demog = FALSE) {
    # When an MSE is loaded, call this to set the MSE.tab up
    # If called with current.MSE == NULL, resets the form 
    Restore_MSE.tab.Speech(session, current.MSE )
    Restore_MSE.tab.AppBeh(session, current.MSE )
    Restore_MSE.tab.Psychomotor(session, current.MSE)
    
    if( clear.demog == TRUE ) {
      Restore_MSE.tab.Demographics(session, current.MSE )
    }
    
  }


########################## UI components for MSE Tab

##########################
# Dashboard Tabs

MSE.tab <- function() {
  fluidRow(
    MSE.tab.Demographics(),
    MSE.tab.AppBeh(),
    MSE.tab.Psychomotor(),
    MSE.tab.Speech(),
    MSE.tab.Controls()
  )
}

Patients.tab <- function( ) {
  
  fluidRow(
    box(title = "Select Patient MSE", status = "primary", width = 12,
        column(12,
               DT::dataTableOutput("pts.table"),
               actionButton("MSEload", "Load MSE"),
               actionButton("MSEdelete", "Delete Selected")
               )
        )
    
  )
}

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "MSE Builder"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("MSE", tabName = "mse", icon = icon("th")),
      menuItem("Patients", tabName = "patients", icon = icon("address-book"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      # First tab content
      tabItem(tabName = "mse", MSE.tab()
      ),
      # Second tab content
      tabItem(tabName = "patients", Patients.tab()
      )
    )
  )
)

server <- function(input, output, session) {

  # some globals for session state  
  current.scope.MSE <<- NULL
  current.scope.fname <<- NULL
  current.scope.fname.hash <<- NULL
  index.df <<- CreateIndex()

  
  # check if there's a patient indexing file
  if( !file.exists( index.path ) ) {
    # ... create one if not
    saveRDS( index.df, index.path )
    
  } else {
    # ... load the index
    index.df <<- SortIndexTable( readRDS( index.path ) )
  }

  ###################################################################
  # Event Handlers
  
  ###################################################################
  # Demographics controls
  observeEvent(input$Demog.TodayDate, {
    updateDateInput(session, inputId = "Demog.MSEDate", value = Sys.Date() )
  })
  
  observeEvent(input$Demog.CurrentTime, {
    updateTextInput(session, inputId = "Demog.MSETime", value = format( Sys.time(), format = "%H:%M") )
  })
  

  ###################################################################
  # For the Controls (MSEupdate, MSEclear, MSErestore (undo) and MSEsave (Save as New))
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
      saveRDS( current.scope.MSE, current.scope.fname )
      # 3 - update the index
      index.df <<- UpdateIndexEntry(index.df, current.scope.MSE, current.scope.fname.hash )
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
      # DEBUGGING ONLY
      assign( "current.scope.MSE", current.scope.MSE, envir = .GlobalEnv)
      
      # 2) generate a unique filename
      fname.hash <- GenerateFilename( current.scope.MSE )
      fname <- paste0( data.path, fname.hash )
      saveRDS( current.scope.MSE, fname )
      
      # update the index table, and save
      index.df <<- AddToIndex(input, index.df, fname.hash)
      saveRDS( index.df, index.path )
      # refresh the patient's table from index.df
      output$pts.table <- RefreshPatientTable(index.df)
      # DEBUG
      # assign( "test.index", index.df, envir = .GlobalEnv)
      # print( index.df )
    }
  })
  
  ############################## "Speech" Form events
  observeEvent(input$Gen.Narrative.Speech, {
    current.scope.MSE <<- ScrapeData(input, fields)
    output$Narrative.Speech <- renderText( NarrativeSpeech( current.scope.MSE ) )
  })
  
  ############################## "Appearance and Behaviour" Form events
  observeEvent(input$Gen.Narrative.AppBeh, {
    current.scope.MSE <<- ScrapeData(input, fields)
    output$Narrative.AppBeh <- renderText( NarrativeAppBeh( current.scope.MSE ) )
  })

  
  
  ############################## "Patients" Tab events 
  
  # the datatable for displaying the index (all patients + MSEs)
  output$pts.table <- RefreshPatientTable(index.df)
  
  observeEvent(input$MSEload, {
    # establish row selected, then build filename to retrieve
    row.idx  <- input$pts.table_rows_selected
    if( !is.null( row.idx ) ) {
      this.row <- index.df[ row.idx, ]
      this.fname <- paste0( data.path, this.row$Hash.fname )
      current.scope.MSE <<- LoadMSE(input, this.fname)
      # print(current.scope.MSE)
      current.scope.fname <<- this.fname
      current.scope.fname.hash <<- this.row$Hash.fname
      # print( current.scope.fname.hash )
      # Set MSE form up and then switch to MSE Tab
      RestoreForm( session, current.scope.MSE,  clear.demog = TRUE )
      updateTabItems(session, "tabs", selected = "mse")
      
      # set up MSE tab to disable some buttons
      shinyjs::disable("Demog.Forename")
      shinyjs::disable("Demog.Surname")
      shinyjs::disable("Demog.uID")
    }
  })

  observeEvent(input$MSEdelete, {
    # 1 - check there is a selected MSE
    row.idx  <- input$pts.table_rows_selected
    if( is.null( row.idx ) ) {
      showModal(modalDialog(
        title = "Error",
        "No MSE selected for deleting"
      ))
    } else {
      del.fname <- paste0( data.path, index.df$Hash.fname[row.idx] )
      # print( del.fname )
      if( file.remove(del.fname) == FALSE ) {
        showModal(modalDialog(
          title = "Error Removing File",
          paste0("The selected MSE file does not exist: ", del.fname )
        ))
      } else {
        # - if successful, update the index
        index.df <<- SortIndexTable( index.df[ -row.idx, ] )
        output$pts.table <- RefreshPatientTable(index.df)
      }
    }
  })
  
    
  # Terminate gracefully
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)