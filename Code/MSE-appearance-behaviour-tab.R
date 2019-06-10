# Appearance and Behaviour Tab Code

####################################################################
# Appearance and Behaviour  UI Fields : to scrape from form
fields.appbeh <- c("AppBeh.Dress", "AppBeh.Dress.Text", "AppBeh.Self.Neglect", "AppBeh.Self.Neglect.Text",
                   "AppBeh.Eye.Contact", "AppBeh.Eye.Contact.Text",
                   "AppBeh.Rapport.Establish", "AppBeh.Rapport.Maintain",
                   "AppBeh.Rapport.Guarding", "AppBeh.Rapport.Guarding.Text",
                   "AppBeh.Rapport.Minimising", "AppBeh.Rapport.Minimising.Text",
                   
                   "AppBeh.Psychomotor.Global", "AppBeh.Psychomotor.Gesture",
                   "AppBeh.Psychomotor.Reactive", "AppBeh.Psychomotor.Disorganisation",
                   "AppBeh.Psychomotor.Mannerism", "AppBeh.Psychomotor.Mannerism.Text",
                   "AppBeh.Psychomotor.Stereotypy", "AppBeh.Psychomotor.Stereotypy.Text",
                   
                   "AppBeh.Motor.Persev", "AppBeh.Motor.Persev.Text",
                   "AppBeh.Motor.Echopraxia", "AppBeh.Motor.Echopraxia.Text",
                   "AppBeh.Motor.Tremor", "AppBeh.Motor.Tremor.Text",
                   "AppBeh.Motor.Gait", "AppBeh.Motor.Gait.Text"
                   )  


################## Appearance and Behaviour Data Entry

appbeh.dress <- c(
  "Appropriate" = "",
  "Underdressed" = "underdressed",
  "Overdressed" = "overdressed"
)

appbeh.self.neglect <- c(
  "None" = "",
  "Mild" = "mild",
  "Moderate" = "moderate",
  "Severe" = "severe"
)

appbeh.eye.contact <- c(
  "Normal" = "",
  "Over Extended" = "over extended",
  "Avoidant" = "avoidant",
  "Fixated" = "fixated"
)

appbeh.rapport <- c(
  "Normal" = "",
  "Some difficulty" = "some difficulty",
  "Very difficult" = "very difficult"
)

appbeh.guarding.minimising <- c(
  "None" = "",
  "Occasional" = "occasional",
  "Frequent" = "frequent",
  "Completely" = "completely"
)

appbeh.psychomotor.global <- c(
  "Normal" = "",
  "Akinetic" = "akinetic",
  "Retarded" = "retarded",
  "Exaggerated" = "exaggerated"
)

appbeh.psychomotor.gesture <- c(
  "Normal" = "",
  "Minimal" = "minimal",
  "Excessive" = "excessive"
)

appbeh.psychomotor.reactive <- c(
  "Normal" = "",
  "Slow" = "slow",
  "Prompt/Excessive" = "excessive"
)

appbeh.psychomotor.disorganisation <- c(
  "Normal" = "",
  "Mild" = "mild",
  "Moderate" = "moderate",
  "Complete" = "complete"
)

appbeh.psychomotor.mannerisms <- c(
  "None" = "",
  "Mild" = "mild",
  "Moderate" = "moderate",
  "Severe" = "severe"
)

appbeh.psychomotor.stereotypy <- appbeh.psychomotor.mannerisms


####################################################################
# Appearance and Behaviour Tab UI Definition

MSE.tab.AppBeh <- function() {
  fluidRow(
      box(
        title = "Appearance", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons( inputId = "AppBeh.Dress", label = "Dress", inline = FALSE, 
                        choices = appbeh.dress ),
          textAreaInput( inputId = "AppBeh.Dress.Text", label = "Comments", value = "", height = "35px")
        ),
        tags$hr(style="border-color: grey;"),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons( inputId = "AppBeh.Self.Neglect", label = "Self-Neglect", inline = FALSE, 
                        choices = appbeh.self.neglect ),
          textAreaInput( inputId = "AppBeh.Self.Neglect.Text", label = "Comments", value = "", height = "35px")
        )
      ),
      box(
        title = "Social Communication", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        splitLayout(cellWidths = c("35%", "65%"),
                    radioButtons(inputId = "AppBeh.Eye.Contact", label = "Eye Contact", inline = FALSE, 
                                 choices = appbeh.eye.contact),
                    textAreaInput( inputId = "AppBeh.Eye.Contact.Text", label = "Comments", value = "", height = "60px")
                    
        ),
        tags$hr(style="border-color: grey;"),
        radioButtons(inputId = "AppBeh.Rapport.Establish", label = "Establishing Rapport", inline = TRUE, 
                     choices = appbeh.rapport),
        radioButtons(inputId = "AppBeh.Rapport.Maintain", label = "Maintaining Rapport", inline = TRUE, 
                     choices = appbeh.rapport)
        
      ),
      box(
        title = "Engagement", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Rapport.Guarding", label = "Guarding", inline = FALSE, 
                       choices = appbeh.guarding.minimising),
          textAreaInput( inputId = "AppBeh.Rapport.Guarding.Text", label = "For example", value = "", height = "60px")
        ),
        tags$hr(style="border-color: grey;"),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Rapport.Minimising", label = "Minimising", inline = FALSE, 
                       choices = appbeh.guarding.minimising),
          textAreaInput( inputId = "AppBeh.Rapport.Minimising.Text", label = "For example", value = "", height = "60px")
        )
      )
  )
}

MSE.tab.Psychomotor <- function() {
  fluidRow(
      box(
        title = "Global Psychomotor", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        radioButtons(inputId = "AppBeh.Psychomotor.Global", label = "Globally", inline = TRUE,
                     choices = appbeh.psychomotor.global),
        tags$hr(style="border-color: grey;"),
        radioButtons(inputId = "AppBeh.Psychomotor.Gesture", label = "Gesturing", inline = TRUE, 
                     choices = appbeh.psychomotor.gesture),
        radioButtons(inputId = "AppBeh.Psychomotor.Reactive", label = "Reactions", inline = TRUE, 
                     choices = appbeh.psychomotor.reactive)
      ),
      box(
        title = "Psychomotor Disturbance", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        radioButtons(inputId = "AppBeh.Psychomotor.Disorganisation", label = "Disturbance of Goal-Directed Behaviour", inline = TRUE, 
                     choices = appbeh.psychomotor.disorganisation),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Mannerism", label = HTML("Mannerisms/Habits <br/> (purposeful)"), inline = FALSE, 
                       choices = appbeh.psychomotor.mannerisms),
          textAreaInput( inputId = "AppBeh.Psychomotor.Mannerism.Text", label = "Examples", value = "", height = "60px")
        ),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Stereotypy", label = "Stereotypies (purposeless)", inline = FALSE, 
                       choices = appbeh.psychomotor.stereotypy),
          textAreaInput( inputId = "AppBeh.Psychomotor.Stereotypy.Text", label = "Examples", value = "", height = "60px")
        )
      ),
      box(
        title = "Other Psychomotor Abnormalities", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        splitLayout(cellWidths = c("35%", "65%"),
                    checkboxInput(inputId = "AppBeh.Motor.Persev", label = "Perseveration", value = FALSE),
                    textAreaInput(inputId = "AppBeh.Motor.Persev.Text", NULL, "", width = "200px", height = "35px")
        ),
        splitLayout(cellWidths = c("35%", "65%"),
                    checkboxInput(inputId = "AppBeh.Motor.Echopraxia", label = "Echopraxia", value = FALSE),
                    textAreaInput(inputId = "AppBeh.Motor.Echopraxia.Text", NULL, "", width = "200px", height = "35px")
        ),
        splitLayout(cellWidths = c("35%", "65%"),
                    checkboxInput(inputId = "AppBeh.Motor.Tremor", label = "Tremor", value = FALSE),
                    textAreaInput(inputId = "AppBeh.Motor.Tremor.Text", NULL, "", width = "200px", height = "35px")
        ),
        splitLayout(cellWidths = c("35%", "65%"),
                    checkboxInput(inputId = "AppBeh.Motor.Gait", label = "Abnormal Gait", value = FALSE),
                    textAreaInput(inputId = "AppBeh.Motor.Gait.Text", NULL, "", width = "200px", height = "35px")
        ),
        
        actionButton(inputId = "Gen.Narrative.AppBeh", "Generate"),
        textOutput("Narrative.AppBeh")
      )
    )
}
####################################################################
# Restore a speech tab UI from saved data
# pass current.MSE = NULL for a reset
Restore_MSE.tab.AppBeh <- function(session, current.MSE){

  updateRadioButtons( session, inputId = "AppBeh.Dress", label = "Dress", inline = FALSE,
                      selected = current.MSE[["AppBeh.Dress"]] )
  updateRadioButtons( session, inputId = "AppBeh.Self.Neglect", label = "Self-Neglect", inline = FALSE,
                      selected = current.MSE[["AppBeh.Self.Neglect"]] )
  updateRadioButtons(session, inputId = "AppBeh.Eye.Contact", label = "Eye Contact", inline = FALSE, 
                      selected = current.MSE[["AppBeh.Eye.Contact"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Establish", label = "Establishing Rapport", inline = TRUE, 
                     selected = current.MSE[["AppBeh.Rapport.Establish"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Maintain", label = "Maintaining Rapport", inline = TRUE, 
                     selected = current.MSE[["AppBeh.Rapport.Maintain"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Guarding", label = "Guarding", inline = FALSE, 
                     selected = current.MSE[["AppBeh.Rapport.Guarding"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Minimising", label = "Minimising", inline = FALSE,
                     selected = current.MSE[["AppBeh.Rapport.Minimising"]] )
  
    # if current.MSE == NULL, be sure to reset the free text areas
    if( is.null( current.MSE ) ) {
      updateTextAreaInput( session, inputId = "AppBeh.Dress.Text", label = "Comments", value = "")
      updateTextAreaInput( session, inputId = "AppBeh.Self.Neglect.Text", label = "Comments", value = "" )
      updateTextAreaInput( session, inputId = "AppBeh.Eye.Contact.Text", label = "Comments", value = "" )
      updateTextAreaInput( session, inputId = "AppBeh.Rapport.Guarding.Text", label = "Comments", value = "" )
      updateTextAreaInput( session, inputId = "AppBeh.Rapport.Minimising.Text", label = "Comments", value = "" )
    } else {
      updateTextAreaInput( session, inputId = "AppBeh.Dress.Text", label = "Comments", value = current.MSE[["AppBeh.Dress.Text"]])
      updateTextAreaInput( session, inputId = "AppBeh.Self.Neglect.Text", label = "Comments", 
                           value = current.MSE[["AppBeh.Self.Neglect.Text"]] )
      updateTextAreaInput( session, inputId = "AppBeh.Eye.Contact.Text", label = "Comments", 
                           value = current.MSE[["AppBeh.Eye.Contact.Text"]] )
      updateTextAreaInput( session, inputId = "AppBeh.Rapport.Guarding.Text", label = "Comments", 
                           value = current.MSE[["AppBeh.Rapport.Guarding.Text"]] )
      updateTextAreaInput( session, inputId = "AppBeh.Rapport.Minimising.Text", label = "Comments", 
                           value = current.MSE[["AppBeh.Rapport.Minimising.Text"]] )
    }
    
}


## TO DO 
# Restore_MSE.tab.Psychomotor <- function( session, current.MSE ){
#     updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Global", label = "Globally", inline = TRUE,
#                  selected = current.MSE[["AppBeh.Psychomotor.Global"]])
#     updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Gesture", label = "Gesturing", inline = TRUE, 
#                  selected = current.MSE[["AppBeh.Psychomotor.Gesture"]])
#     updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Reactive", label = "Reactions", inline = TRUE, 
#                  selected = current.MSE[["AppBeh.Psychomotor.Reactive"]])
#     updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Disorganisation", label = "Disturbance of Goal-Directed Behaviour", inline = TRUE, 
#                  selected = current.MSE[["AppBeh.Psychomotor.Disorganisation"]])
#     updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Mannerism", label = HTML("Mannerisms/Habits <br/> (purposeful)"), inline = FALSE, 
#                  selected = current.MSE[["AppBeh.Psychomotor.Mannerism"]])
#     updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Stereotypy", label = "Stereotypies (purposeless)", inline = FALSE, 
#                  selected = current.MSE[["AppBeh.Psychomotor.Stereotypy"]])
# 
#     updateCheckboxInput(session, inputId = "AppBeh.Motor.Persev", label = "Perseveration", 
#                         value = current.MSE[["AppBeh.Motor.Persev"]])
#     
#     updateCheckboxInput(session, inputId = "AppBeh.Motor.Echopraxia", label = "Echopraxia", 
#                         value = current.MSE[["AppBeh.Motor.Echopraxia"]]),
#     textAreaInput(inputId = "AppBeh.Motor.Echopraxia.Text", NULL, "", width = "200px", height = "35px")
#     ),
# 
#     checkboxInput(inputId = "AppBeh.Motor.Tremor", label = "Tremor", value = FALSE),
#     textAreaInput(inputId = "AppBeh.Motor.Tremor.Text", NULL, "", width = "200px", height = "35px")
#     
#     checkboxInput(inputId = "AppBeh.Motor.Gait", label = "Abnormal Gait", value = FALSE),
#     textAreaInput(inputId = "AppBeh.Motor.Gait.Text", NULL, "", width = "200px", height = "35px")
# 
#     
#     # text areas
#     textAreaInput( inputId = "AppBeh.Psychomotor.Mannerism.Text", label = "Examples", value = "", height = "60px")
#     textAreaInput( inputId = "AppBeh.Psychomotor.Stereotypy.Text", label = "Examples", value = "", height = "60px")
#     textAreaInput(inputId = "AppBeh.Motor.Persev.Text", NULL, "", width = "200px", height = "35px")
# }
              

####################################################################
# Narrative building from structured data 

NarrativeAppBeh <- function( current.mse ) {
  return(
    paste(
      NarrativeAppEyeRapport( current.mse)
    )
  )
}

NarrativeAppEyeRapport <- function( current.mse) {

  # debug only
    current.mse <- current.scope.MSE
  ####
  appear.str <- "In terms of appearance, they were "
  # completely normal Appearance ? 
  dress.df <- current.mse[ , grep("AppBeh.Dress", names(current.mse), value = TRUE) ]
  if( DataFrameRowEmpty( dress.df ) == TRUE ) {
    appear.str <- paste0( appear.str, "dressed appropriately for conditions" )
  } else {
    appear.str <- paste0( appear.str, switch( dress.df$AppBeh.Dress,
                                    "underdressed" = {"underdressed for conditions"},
                                    "overdressed" = {"overdressed for conditions"},
                                    {"dressed appropriately for condtions"}
                            ) )
  }
  
  if( dress.df$AppBeh.Dress.Text != "" ) {
    appear.str <- paste0( appear.str, " (", dress.df$AppBeh.Dress.Text, ") " )
  } 
  
  neglect.df <- current.mse[ , grep("AppBeh.Self.Neglect", names(current.mse), value = TRUE) ]
  if( DataFrameRowEmpty( neglect.df ) == TRUE ) {
    appear.str <- paste0( appear.str, " with no signs of self-neglect.")
  } else {
    appear.str <- paste0( appear.str, switch( neglect.df$AppBeh.Self.Neglect,
                                  "mild" = {" with signs of mild self-neglect "},
                                  "moderate" = {" with signs of moderate self-neglect "},
                                  "severely" = {" with signs of severe self-neglect "},
                                  {""} )
    )
    if( neglect.df$AppBeh.Self.Neglect.Text != "" ) {
      appear.str <- paste0( appear.str, "(", neglect.df$AppBeh.Self.Neglect.Text, ").")
    } else {
      appear.str <- paste0( appear.str, ".")
    }
  }
  
  eyecon.str <- "Eye contact was "
  eyecon.df <- current.mse[ , grep("AppBeh.Eye", names(current.mse), value = TRUE) ]
  if( DataFrameRowEmpty( eyecon.df ) == TRUE ) {
    eyecon.str <- paste0( eyecon.str, "within normal limits.")
  } else {
    eyecon.str <- paste0( eyecon.str, switch( eyecon.df$AppBeh.Eye.Contact,
                                  "over extended" = {"over-extended"},
                                  "avoidant" = {"avoidant"},
                                  "fixated" = {"fixated"}
      
    ))
    if( eyecon.df$AppBeh.Eye.Contact.Text != "" ) {
      eyecon.str <- paste0( eyecon.str, " (", eyecon.df$AppBeh.Eye.Contact.Text, ").")
    } else {
      eyecon.str <- paste0( eyecon.str, ".")
    }
  }
  
  rapport.str <- "Rapport was "
  rapport.df <- current.mse[ , grep("AppBeh.Rapport.Establish|Maintain", names(current.mse), value = TRUE) ]
  if( DataFrameRowEmpty( rapport.df ) == TRUE ) {
    rapport.str <- paste0( rapport.str, "easy to establish and maintain.")
  } else {
    rapport.str <- paste0( rapport.str, switch( rapport.df$AppBeh.Rapport.Establish,
                                   "some difficulty" = {"established with some difficulty "},
                                   "very difficult" = {"very difficult to establish  "}, 
                                   {"easy to establish "})
                  )
    

    rapport.str <- paste0( rapport.str, ifelse( rapport.df$AppBeh.Rapport.Maintain == "" |
                                                  rapport.df$AppBeh.Rapport.Establish == "", "but ", "and " ) )
    
    rapport.str <- paste0( rapport.str, switch( rapport.df$AppBeh.Rapport.Maintain,
                                   "some difficulty" = {"somewhat difficult to maintain."},
                                   "very difficult" = {"very difficult to maintain. "},
                                   {"easy to maintain. "})
    )
  }
  
  # guarding / minimising
  guard.df <- current.mse[ , grep("AppBeh.Rapport.Guarding|Minimising", names(current.mse), value = TRUE) ]
  guard.str <- ""
  if( DataFrameRowEmpty( guard.df ) == TRUE ) {
    guard.str <- paste0( "There was no evidence of guarding or minimising of symptoms/behaviours.")
  } else {
    guard.str <- paste0( guard.str, switch(guard.df$AppBeh.Rapport.Guarding,
                                           "occasional" = {"They were occasionally guarded"},
                                           "frequent" = {"They were frequently guarded"}, 
                                           "completely" = {"They were completely guarded on all topics"},
                                           {"There was no evidence of guarding"})
    )
    guard.str <- ifelse( guard.df$AppBeh.Rapport.Guarding.Text == "",
                         paste0( guard.str, " " ), 
                         paste0( guard.str, " (e.g. ", guard.df$AppBeh.Rapport.Guarding.Text, ")" ) )
    
    guard.str <- paste0( guard.str, ifelse( guard.df$AppBeh.Rapport.Guarding == "" &
                                            guard.df$AppBeh.Rapport.Minimising != "", " but ", " and ") )
    
    guard.str <- paste0( guard.str, switch(guard.df$AppBeh.Rapport.Minimising,
                                           "occasional" = {"there was occasional minimising of symptoms/behaviours"},
                                           "frequent" = {"there was frequent minimising of symptoms/behaviours"}, 
                                           "completely" = {"they minimised all symptoms/behaviours"},
                                           {" there was no evidence of minimising of symptoms/behaviours"}) 
    )
    guard.str <- ifelse( guard.df$AppBeh.Rapport.Minimising.Text == "",
                         paste0( guard.str, "."), 
                         paste0( guard.str, " (e.g. ", guard.df$AppBeh.Rapport.Minimising.Text, ")." ) )
    
    
    
  }
  
  return( paste( appear.str, eyecon.str, rapport.str, guard.str ) )
}



