# Demographics Tab Code

####################################################################
# UI Fields : to scrape from form
fields.demog <- c("Demog.Forename", "Demog.Surname", "Demog.uID", "Demog.MSEDate", "Demog.MSETime")  


#############################################################
# Data Entry Codes for Fields
# Not relevant 

####################################################################
# Speech Tab UI Definition
  MSE.tab.Demographics <- function() {
    fluidRow(
      box(title = "Demographics", status = "warning", width = 12, collapsible = FALSE, solidHeader = TRUE,
        column(3,
          textInput(inputId = "Demog.Forename", label = "Forename", value = ""),
          span(textOutput("Missing.Demog.Forename"), style="color:red")
        ),
        column(3,
          textInput(inputId = "Demog.Surname", label = "Surname", value = ""),
          span(textOutput("Missing.Demog.Surname"), style="color:red")
        ),
        column(2,
          dateInput(inputId = "Demog.MSEDate", label = "MSE Date:", format = "dd-MM-yyyy", language = "en-GB"),
          actionButton(inputId = "Demog.TodayDate", label = "Today")
        ),
        column(1,
          textInput(inputId = "Demog.MSETime", label = "Time:", value = format( Sys.time(), "%H:%M"), 
                    placeholder = "Enter in 24hr format Hour:Minutes"),
          span(textOutput("Error.Demog.Time"), style="color:red"),
          actionButton(inputId = "Demog.CurrentTime", label = "Now")
        ),
        column(2,
          textInput(inputId = "Demog.uID", label = "Unique ID", value = ""),
          span(textOutput("Missing.Demog.uID"), style="color:red")
        )
      )
    )
  }
  
  
####################################################################
# Validation functions
MSE.tab.ValidateDemographics <- function(input, output) {
  valid.fields <- 0
  if ( input$Demog.Forename == "" ) {
    output$Missing.Demog.Forename <- renderText({"Required"})
  } else {
    output$Missing.Demog.Forename <- renderText({""})
    valid.fields <- valid.fields + 1
  }
  
  if ( input$Demog.Surname == "" ) {
    output$Missing.Demog.Surname <- renderText({"Required"})
  } else {
    output$Missing.Demog.Surname <- renderText({""})
    valid.fields <- valid.fields + 1
  }
  
  if ( input$Demog.uID == "" ) {
    output$Missing.Demog.uID <- renderText({"Required"})
  } else {
    output$Missing.Demog.uID <- renderText({""})
    valid.fields <- valid.fields + 1
  }
  
  # 
  # textInput(inputId = "Demog.MSETime", label = "Time:", value = format( Sys.time(), "%H:%M"), 
  #           placeholder = "Enter in 24hr format Hour:Minutes"),
  # span(textOutput("Error.Demog.Time"), style="color:red")
  valid.date.time <- as.POSIXct( paste0( as.character(input$Demog.MSEDate ), " ", input$Demog.MSETime ), format = "%Y-%m-%d %H:%M" )
  if( is.na( valid.date.time ) ) {
    output$Error.Demog.Time <- renderText({"Use 24hr format HH:MM"})
  } else {
    output$Error.Demog.Time <- renderText({""})
    valid.fields <- valid.fields + 1
  }
  
  return( ifelse( valid.fields == 4, TRUE, FALSE ) )
}

####################################################################
# Restore a speech tab UI from saved data
# pass current.MSE = NULL for a reset

Restore_MSE.tab.Demographics <- function(session, current.MSE){
  # if current.MSE == NULL, be sure to reset the free text areas
  if( is.null( current.MSE ) ) {
    updateTextInput(session, "Demog.Forename", value = "")
    updateTextInput(session, "Demog.Surname", value = "")
    updateTextInput(session, "Demog.uID", value = "")
    updateDateInput(session, "Demog.MSEDate", value = Sys.Date())
    updateTextInput(session, "Demog.MSETime", value = HumaniseTime( Sys.time()))
  } else {
    updateTextInput(session, "Demog.Forename", value = current.MSE[["Demog.Forename"]])
    updateTextInput(session, "Demog.Surname", value = current.MSE[["Demog.Surname"]])
    updateTextInput(session, "Demog.uID", value = current.MSE[["Demog.uID"]])
    updateDateInput(session, "Demog.MSEDate", value = current.MSE[["Demog.MSEDate"]])
    updateTextInput(session, "Demog.MSETime", value = current.MSE[["Demog.MSETime"]])
  }
}

#   
#   
# 
# ####################################################################
# # Narrative building from structured data 
#   
# NarrativeSpeechRate <- function( speech.str ) {
#   # rules for building the narrative description of speech process
#   speech.str <- unlist( speech.str )
#   
#   # Was normal ?
#   if( is.null( speech.str ) ) {
#     narr.speech <- "Speech was normal in rate, rhythm and tone"
#     return( narr.speech )
#   } else {
#     N <- length( speech.str )
#     # rate abnormalities
#     narr.speech <- paste0( paste0( speech.str[1:(N-1)], collapse = ", " ), " and ", speech.str[N] )
#   }
#   return( narr.speech )
# }