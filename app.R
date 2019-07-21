rm( list = ls() )
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(scales)
library(shinyMCE)
library(sodium)
source("./Code/MSE-demographics-tab.R")
source("./Code/MSE-appearance-behaviour-tab.R")
source("./Code/MSE-speech-tab.R")
source("./Code/MSE-controls-tab.R")
source("./Code/patients-tab.R")
source("./Code/editor-tab.R")
source("./Code/tools-tab.R")
source("./Code/helper-functions.R")
source("./Code/config-helpers.R")


# # aggregate list of fields to scrape
#   fields <- c( fields.demog, fields.appbeh, fields.speech )

########################## UI components for MSE Tab

##########################
# Dashboard Tabs - for tabs with composite layouts

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
    Patients.tab.UI()
}

Tools.tab <- function() {
    Tools.tab.UI()
}

Editor.tab <- function() {
    Editor.tab.UI()
}

ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "MSE Builder"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Patients", tabName = "patients", icon = icon("address-book")),
      menuItem("Build MSE", tabName = "mse", icon = icon("th")),
      menuItem("Editor", tabName = "editor", icon = icon("edit")),
      menuItem("Tools", tabName = "tools", icon = icon("toolbox"))
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      tabItem(tabName = "patients", Patients.tab() ),
      tabItem(tabName = "mse", MSE.tab() ),
      tabItem(tabName = "editor", Editor.tab() ),
      tabItem(tabName = "tools", Tools.tab() )
    )
  )
)

server <- function(input, output, session) {

  # aggregate list of fields to scrape
  fields <- c( fields.demog, fields.appbeh, fields.speech )
  
  # Session / connection globals
  config.file <<- Config.ReadFile()
  index.path <<- Config.GetValue("index.path")
  data.path  <<- Config.GetValue("pts.path")
  encryption.on <<- Config.GetValue("encryption")
  current.passphrase <<- Config.GetValue("passphrase")
  
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
  # Demographics 
  #   MSE-demographics-tab.R
  MSE.tab.Demographics.Server( input, output, session )

  ###################################################################
  # For the Controls (MSEupdate, MSEclear, MSErestore (undo) and MSEsave (Save as New))
  #   MSE-controls-tab.R
  Controls.tab.Server( input, output, session, fields )

  ############################## "Speech" Form events
  #   MSE-speech-tab.R
  MSE.tab.Speech.Server(input,output,session,fields)
  
  ############################## "Appearance and Behaviour" Form events
  #   MSE-appearance-behaviour-tab.R
  MSE.tab.AppBeh.Server(input,output,session,fields)

  ############################# "Patients" Tab Server Code 
  #   patients-tab.R
  Patients.tab.Server( input, output, session )

  ############################# "Editor" Tab Server Code
  #   editor-tab.R
  Editor.Server( input, output, session )
  
  # Terminate gracefully
  session$onSessionEnded(function() {
    # DEBUG : uncomment this code to MAKE SURE NO CONFIDENTIAL DATA IS LEFT IN R ENVIRONMENT !!!!!!
    # Need to check the global environment WITHOUT the following and modify the list = () appropriately to ensure
    # environment cleaned up !
    print("Clearing Global Environment")
    # rm( list = c( "config.file",
    #               "current.scope.MSE", "index.df",
    #               "current.passphrase",
    #               "index.path" ), envir = .GlobalEnv )
    print( ls() )
    print( environment() )
    rm( list = ls(), envir = environment() )
    stopApp()
  })
  
}

shinyApp(ui, server)