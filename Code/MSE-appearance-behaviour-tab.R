# Appearance and Behaviour Tab Code

####################################################################
# Appearance and Behaviour  UI Fields : to scrape from form
fields.appbeh <- c("AppBeh.Dress", "AppBeh.Dress.Text", "AppBeh.Self.Neglect", "AppBeh.Self.Neglect.Text",
                   "AppBeh.Eye.Contact", "AppBeh.Eye.Contact.Text",
                   "AppBeh.Rapport.Establish", "AppBeh.Rapport.Maintain",
                   "AppBeh.Rapport.Guarding", "AppBeh.Rapport.Guarding.Text",
                   "AppBeh.Rapport.Minimising", "AppBeh.Rapport.Minimising.Text",
                   
                   "AppBeh.Psychomotor.Alert", "AppBeh.Psychomotor.Alert.Text", 
                   "AppBeh.Psychomotor.Gesture.Form",
                   "AppBeh.Psychomotor.Gesture.Rate",
                   "AppBeh.Psychomotor.Reactions.Form",
                   "AppBeh.Psychomotor.Reactions.Rate",
                   "AppBeh.Psychomotor.Disorganisation", "AppBeh.Psychomotor.Disorganisation.Text",
                   "AppBeh.Psychomotor.Mannerism", "AppBeh.Psychomotor.Mannerism.Text",
                   "AppBeh.Psychomotor.Stereotypy", "AppBeh.Psychomotor.Stereotypy.Text",
                   "AppBeh.Psychomotor.Tics", "AppBeh.Psychomotor.Tics.Text",
                   
                   "AppBeh.Motor.Persev", "AppBeh.Motor.Persev.Text",
                   "AppBeh.Motor.Echopraxia", "AppBeh.Motor.Echopraxia.Text",
                   "AppBeh.Motor.Tremor", "AppBeh.Motor.Tremor.Text",
                   "AppBeh.Motor.Gait", "AppBeh.Motor.Gait.Text"
                   )  


################## Appearance and Behaviour Data Entry

appbeh.dress <- c(
  "Appropriate" = "normal",
  "Underdressed" = "underdressed",
  "Overdressed" = "overdressed"
)

appbeh.self.neglect <- c(
  "None" = "normal",
  "Mild" = "mild",
  "Moderate" = "moderate",
  "Severe" = "severe"
)

appbeh.eye.contact <- c(
  "Normal" = "normal",
  "Over Extended" = "over extended",
  "Avoidant" = "avoidant",
  "Fixated" = "fixated"
)

appbeh.rapport <- c(
  "Normal" = "normal",
  "Some difficulty" = "some difficulty",
  "Very difficult" = "very difficult"
)

appbeh.guarding.minimising <- c(
  "None" = "normal",
  "Occasional" = "occasional",
  "Frequent" = "frequent",
  "Completely" = "completely"
)

appbeh.psychomotor.alert <- c(
  "Alert" = "normal",
  "Sedated" = "sedated",
  "Drowsy" = "drowsy",
  "Lethargic" = "lethargic",
  "Stuporous" = "stuporous"
)

appbeh.psychomotor.gesture.form <- c(
  "Normal" = "normal",
  "Absent" = "absent",
  "Minimal" = "minimal",
  "Excessive" = "excessive"
)

appbeh.psychomotor.gesture.rate <- c(
  "Normal" = "normal",
  "Slow" = "slow",
  "Fast" = "fast"
)

appbeh.psychomotor.reactions.form <- appbeh.psychomotor.gesture.form
appbeh.psychomotor.reactions.rate <- appbeh.psychomotor.gesture.rate
  
appbeh.psychomotor.disorganisation <- c(
  "Normal" = "normal",
  "Mild" = "mild",
  "Moderate" = "moderate",
  "Complete" = "complete"
)

appbeh.psychomotor.mannerisms <- c(
  "None" = "normal",
  "Occasional" = "occasional",
  "Frequent" = "frequent",
  "Regular" = "regular"
)

appbeh.psychomotor.stereotypy <- appbeh.psychomotor.mannerisms
appbeh.psychomotor.tics <- appbeh.psychomotor.stereotypy


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
        title = "Global Psychomotor Behaviour", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Alert", label = "Alertness", inline = FALSE,
                       choices = appbeh.psychomotor.alert),
          textAreaInput( inputId = "AppBeh.Psychomotor.Alert.Text", label = "Comments", value = "", height = "60px")
        ),
        tags$hr(style="border-color: grey;"),
        tags$p(tags$b("Spontaneous Gestures")),
        splitLayout(cellWidths = c("50%", "50%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Gesture.Form", label = "Content", inline = FALSE, 
                       choices = appbeh.psychomotor.gesture.form),
          radioButtons(inputId = "AppBeh.Psychomotor.Gesture.Rate", label = "Rate", inline = FALSE, 
                       choices = appbeh.psychomotor.gesture.rate)
        ),
        tags$hr(style="border-color: grey;"),
        tags$p(tags$b("Reactions to Stimuli")),
        splitLayout(cellWidths = c("50%", "50%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Reactions.Form", label = "Content", inline = FALSE, 
                       choices = appbeh.psychomotor.reactions.form),
          radioButtons(inputId = "AppBeh.Psychomotor.Reactions.Rate", label = "Rate", inline = FALSE, 
                       choices = appbeh.psychomotor.reactions.rate)
        )
      ),
      box(
        title = "Psychomotor Anomalies", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        tags$p(tags$b("Disorganisation of Goal-Directed Behaviour")),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Disorganisation", label = "Degree", inline = FALSE, 
                       choices = appbeh.psychomotor.disorganisation),
          textAreaInput(inputId = "AppBeh.Psychomotor.Disorganisation.Text", label = "Examples", value = "", height = "60px")
        ),
        tags$hr(style="border-color: grey;"),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Mannerism", label = HTML("Mannerisms/Habits <br> (purposeful)"), inline = FALSE, 
                       choices = appbeh.psychomotor.mannerisms),
          textAreaInput( inputId = "AppBeh.Psychomotor.Mannerism.Text", label = "Examples", value = "", height = "60px")
        ),
        tags$hr(style="border-color: grey;"),
        splitLayout(cellWidths = c("35%", "65%"),
          radioButtons(inputId = "AppBeh.Psychomotor.Stereotypy", label = HTML("Stereotypies <br> (purposeless)"), inline = FALSE, 
                       choices = appbeh.psychomotor.stereotypy),
          textAreaInput( inputId = "AppBeh.Psychomotor.Stereotypy.Text", label = "Examples", value = "", height = "60px")
        ),
        tags$hr(style="border-color: grey;"),
        splitLayout(cellWidths = c("35%", "65%"),
                    radioButtons(inputId = "AppBeh.Psychomotor.Tics", label = "Tics", inline = FALSE, 
                                 choices = appbeh.psychomotor.tics),
                    textAreaInput( inputId = "AppBeh.Psychomotor.Tics.Text", label = "Examples", value = "", height = "60px")
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
# Restore App and Behaviour tab UI from saved data
# pass current.MSE = NULL for a reset
Restore_MSE.tab.AppBeh <- function(session, current.MSE){

  updateRadioButtons( session, inputId = "AppBeh.Dress", label = "Dress", inline = FALSE,
                      choices = appbeh.dress, selected = current.MSE[["AppBeh.Dress"]] )
  updateRadioButtons( session, inputId = "AppBeh.Self.Neglect", label = "Self-Neglect", inline = FALSE,
                      choices = appbeh.self.neglect, selected = current.MSE[["AppBeh.Self.Neglect"]] )
  updateRadioButtons(session, inputId = "AppBeh.Eye.Contact", label = "Eye Contact", inline = FALSE, 
                      choices = appbeh.eye.contact, selected = current.MSE[["AppBeh.Eye.Contact"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Establish", label = "Establishing Rapport", inline = TRUE, 
                     choices = appbeh.rapport, selected = current.MSE[["AppBeh.Rapport.Establish"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Maintain", label = "Maintaining Rapport", inline = TRUE, 
                     choices = appbeh.rapport, selected = current.MSE[["AppBeh.Rapport.Maintain"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Guarding", label = "Guarding", inline = FALSE, 
                     choices = appbeh.guarding.minimising, selected = current.MSE[["AppBeh.Rapport.Guarding"]] )
  updateRadioButtons(session, inputId = "AppBeh.Rapport.Minimising", label = "Minimising", inline = FALSE,
                     choices = appbeh.guarding.minimising, selected = current.MSE[["AppBeh.Rapport.Minimising"]] )
  
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

Restore_MSE.tab.Psychomotor <- function( session, current.MSE ){
  
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Alert", label = "Alertness", inline = FALSE,
                       choices = appbeh.psychomotor.alert, selected = current.MSE[["AppBeh.Psychomotor.Alert"]])
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Gesture.Form", #label = HTML("Spontaneous <br> Gestures (Content)"), 
                       inline = FALSE, choices = appbeh.psychomotor.gesture.form,  
                       selected = current.MSE[["AppBeh.Psychomotor.Gesture.Form"]])
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Gesture.Rate", #label = HTML("Spontaneous <br> Gestures (Rate)"), 
                       inline = FALSE, choices = appbeh.psychomotor.gesture.rate,  
                       selected = current.MSE[["AppBeh.Psychomotor.Gesture.Rate"]])
    
    
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Reactions.Form", inline = FALSE,
                       choices = appbeh.psychomotor.reactions.form,  selected = current.MSE[["AppBeh.Psychomotor.Reactions.Form"]])
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Reactions.Rate", inline = FALSE,
                       choices = appbeh.psychomotor.reactions.rate,  selected = current.MSE[["AppBeh.Psychomotor.Reactions.Rate"]])
    
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Disorganisation", label = "Degree", inline = FALSE,
                       choices = appbeh.psychomotor.disorganisation, selected = current.MSE[["AppBeh.Psychomotor.Disorganisation"]])
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Mannerism", inline = FALSE,
                       choices = appbeh.psychomotor.mannerisms,  selected = current.MSE[["AppBeh.Psychomotor.Mannerism"]])
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Stereotypy", inline = FALSE,
                       choices = appbeh.psychomotor.stereotypy, selected = current.MSE[["AppBeh.Psychomotor.Stereotypy"]])
    updateRadioButtons(session, inputId = "AppBeh.Psychomotor.Tics", inline = FALSE,
                       choices = appbeh.psychomotor.stereotypy, selected = current.MSE[["AppBeh.Psychomotor.Tics"]])
    


    if( is.null( current.MSE ) ) {
      # reset / blank MSE
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Alert.Text", label = "Comments", value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Echopraxia.Text", label = NULL, value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Tremor.Text", label = NULL, value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Gait.Text", label = NULL, value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Persev.Text", label = NULL, value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Disorganisation.Text", label = "Examples", value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Mannerism.Text", label = "Examples", value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Stereotypy.Text", label = "Examples", value = "")
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Tics.Text", label = "Examples", value = "")
      # sort out TRUE / FALSE check boxes
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Persev", label = "Perseveration",
                          value = FALSE)
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Echopraxia", label = "Echopraxia",
                          value = FALSE)
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Tremor", label = "Tremor", 
                          value = FALSE)
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Gait", label = "Abnormal Gait", 
                          value = FALSE)
      
    } else {
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Alert.Text", label = "Comments", value = current.MSE[["AppBeh.Psychomotor.Alert.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Echopraxia.Text", label = NULL, value = current.MSE[["AppBeh.Motor.Echopraxia.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Tremor.Text", label = NULL, value = current.MSE[["AppBeh.Motor.Tremor.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Gait.Text", label = NULL, value = current.MSE[["AppBeh.Motor.Gait.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Motor.Persev.Text", label = NULL, value = current.MSE[["AppBeh.Motor.Persev.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Disorganisation.Text", label = "Examples", value = current.MSE[["AppBeh.Psychomotor.Disorganisation.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Mannerism.Text", label = "Examples", 
                          value = current.MSE[["AppBeh.Psychomotor.Mannerism.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Stereotypy.Text", label = "Examples", 
                          value = current.MSE[["AppBeh.Psychomotor.Stereotypy.Text"]])
      updateTextAreaInput(session, inputId = "AppBeh.Psychomotor.Tics.Text", label = "Examples", 
                          value = current.MSE[["AppBeh.Psychomotor.Tics.Text"]])
      
      
      # sort out TRUE / FALSE check boxes
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Persev", label = "Perseveration",
                          value = current.MSE[["AppBeh.Motor.Persev"]])
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Echopraxia", label = "Echopraxia",
                          value = current.MSE[["AppBeh.Motor.Echopraxia"]])
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Tremor", label = "Tremor", 
                          value = current.MSE[["AppBeh.Motor.Tremor"]])
      updateCheckboxInput(session, inputId = "AppBeh.Motor.Gait", label = "Abnormal Gait", 
                          value = current.MSE[["AppBeh.Motor.Gait"]])
      
    }
}
              

####################################################################
# Narrative building from structured data 

NarrativeAppBeh <- function( current.mse ) {
  return(
    paste(
      NarrativeAppEyeRapport( current.mse),
      NarrativePsychomotor( current.mse )
    )
  )
}

NarrativeAppEyeRapport <- function( current.mse) {

  
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


# TO DO
NarrativePsychomotor <- function( current.mse) {

  # debug only  
  current.mse <- current.scope.MSE
  psychomotor.df <- current.mse[ , grep("AppBeh.Psycho|Motor", names(current.mse), value = TRUE) ]
  
  if( DataFrameRowEmpty( psychomotor.df ) == TRUE ) {
     psychomotor.str <- "There was no evidence of psychomotor abnormalities."
   } else {
     psychomotor.str <- "In terms of psychomotor features: "
   }
  
  psychomotor.str <- paste0( psychomotor.str, switch(psychomotor.df$AppBeh.Psychomotor.Alert,
                                                     "sedated" = {"they appeared sedated "},
                                                     "drowsy" = {"they appeared drowsy "},
                                                     "lethargic" = {"they were lethargic "},
                                                     "stuporous" = {"they were stuporous "},
                                                      {"they were alert "}
  )) 
  
  if( psychomotor.df$AppBeh.Psychomotor.Alert.Text != "" ) {
    psychomotor.str <- paste0( psychomotor.str, "(e.g. ", psychomotor.df$AppBeh.Psychomotor.Alert.Text, ") " )  
  }
  
  
  gesture.matrix <- matrix( c( "with normal gestures", "with an absence of gestures", "with minimal gesturing", "with excessive gestures",
                               "with normal, but slow, gestures", "with an absence of gestures", "with slow, minimal gestures", "with slow but excessive gestures",
                               "with normal, but fast, gestures", "with an absence of gestures", "with fast, minimal gestures", "with fast and excessive gestures"),
                                nrow = 3, ncol = 4, byrow = TRUE,
                                dimnames = list( appbeh.psychomotor.gesture.rate, appbeh.psychomotor.gesture.form))
  
  psychomotor.str <- paste0( psychomotor.str, 
                             gesture.matrix[psychomotor.df$AppBeh.Psychomotor.Gesture.Rate, psychomotor.df$AppBeh.Psychomotor.Gesture.Form],
                             " ")
  
  reaction.matrix <- matrix( c( "and normal reactions", "and absent reactions", "and minimal reactions", "and excessive reactions",
                               "and slow but otherwise normal reactions", "and absent reactions", "and slow, minimal reactions", 
                               "and slow but excessive reactions",
                               "and fast but otherwise normal reactions", "and absent reactions", "and fast but minimal reactions", 
                               "and fast, excessive reactions"),
                            nrow = 3, ncol = 4, byrow = TRUE,
                            dimnames = list( appbeh.psychomotor.reactions.rate, appbeh.psychomotor.reactions.form))
  
  psychomotor.str <- paste0( psychomotor.str, 
                             reaction.matrix[psychomotor.df$AppBeh.Psychomotor.Reactions.Rate, psychomotor.df$AppBeh.Psychomotor.Reactions.Form],
                             ".")
  
  psychomotor.str <- paste0( psychomotor.str,
                             "  Goal-directed behaviours were ",
                             switch(psychomotor.df$AppBeh.Psychomotor.Disorganisation,
                                    "mild" = {"mildly disorganised"},
                                    "moderate" = {"moderately disorganised"},
                                    "complete" = {"completely disorganised"},
                                    {"normal"}
                                    )
                             )
  if( psychomotor.df$AppBeh.Psychomotor.Disorganisation != "normal" &
      psychomotor.df$AppBeh.Psychomotor.Disorganisation.Text != "" ) {
    psychomotor.str <- paste0( psychomotor.str, " (e.g. ", psychomotor.df$AppBeh.Psychomotor.Disorganisation.Text, ")" )
  } 
  
  psychomotor.str <- paste0(psychomotor.str, switch(psychomotor.df$AppBeh.Psychomotor.Mannerism,
                                                    "occasional" = {", with occasional mannerisms"},
                                                    "frequent" = {", with frequent mannerisms"},
                                                    "regular" = {", with regular mannerisms"},
                                                    {""}
                                                    ))
  
  if( psychomotor.df$AppBeh.Psychomotor.Mannerism != "normal" &
      psychomotor.df$AppBeh.Psychomotor.Mannerism.Text != "" ) {
    psychomotor.str <- paste0( psychomotor.str, " (e.g. ", psychomotor.df$AppBeh.Psychomotor.Mannerism.Text, ")" )
  }
  
  psychomotor.str <- paste0(psychomotor.str, switch(psychomotor.df$AppBeh.Psychomotor.Stereotypy,
                                                    "occasional" = {" and there were occasional stereotyped movements"},
                                                    "frequent" = {" and there were frequent stereotyped movements"},
                                                    "regular" = {" and there were regular stereotyped movements"},
                                                    {""}
  ))
  
  if( psychomotor.df$AppBeh.Psychomotor.Stereotypy != "normal" & 
      psychomotor.df$AppBeh.Psychomotor.Stereotypy.Text != "" ) {
    psychomotor.str <- paste0( psychomotor.str, " (e.g. ", psychomotor.df$AppBeh.Psychomotor.Stereotypy.Text, ")." )
  } else {
    psychomotor.str <- paste0( psychomotor.str, "." )
  }

  psychomotor.str <- paste0(psychomotor.str, switch(psychomotor.df$AppBeh.Psychomotor.Tics,
                                                    "occasional" = {"  They displayed occasional tics"},
                                                    "frequent" = {"  They displayed frequent tics"},
                                                    "regular" = {"  They displayed regular tics"},
                                                    {""}
  ))
  
  if( psychomotor.df$AppBeh.Psychomotor.Tics != "normal" &
      psychomotor.df$AppBeh.Psychomotor.Tics.Text != "" ) {
    psychomotor.str <- paste0( psychomotor.str, " (e.g. ", psychomotor.df$AppBeh.Psychomotor.Tics.Text, ")." )
  } else {
    psychomotor.str <- paste0( psychomotor.str, "." )
  }

  
  
  return( psychomotor.str )
  
}

#############################################################
# Server Code
MSE.tab.AppBeh.Server <- function(input,output,session,fields) {
  observeEvent(input$Gen.Narrative.AppBeh, {
    current.scope.MSE <<- ScrapeData(input, fields)
    output$Narrative.AppBeh <- renderText( NarrativeAppBeh( current.scope.MSE ) )
  })
}
