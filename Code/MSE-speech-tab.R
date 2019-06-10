# Speech Tab Code

####################################################################
# Speech UI Fields : to scrape from form
fields.speech <- c("Speech.Rate.1", "Speech.Rate.2", "Speech.Process.Interrupt",
                   "Speech.Process.Circum", "Speech.Process.Reorient", 
                   "Speech.Prosody", "Speech.Rhythm", "Speech.Content.Features.1",
                   "Speech.Content.Persev",
                   "Speech.Free.Content")  



#############################################################
# Speech Data Entry
  speech.rate.features.1 <- c( "Normal" = "",
                               "Fast" = "fast",
                               "Slow" = "slow")
  
  speech.rate.features.2 <- c("Normal" = "",
                              "Mildly Pressured" = "mildly pressured",
                              "Moderately Pressured" = "moderately pressured",
                              "Severely Pressured" = "severely pressured",
                              "Mild Initiation Latency" = "mild initiation latency",
                              "Moderate Initiation Latency" = "moderate initiation latency",
                              "Severe Initiation Latency" = "severe initiation latency")
  
  speech.process.interrupt <- c("No (normal)" = "",
                                "Occasionally" = "occasionally",
                                "Frequently" = "frequently",
                                "Completely" = "completely")
  
  speech.process.circum <- c("Normal" = "",
                             "Occasionally" = "occasionally",
                             "Frequently" = "frequently",
                             "Completely" = "consistently")
  
  speech.process.reorient <- c("Normal" = "",
                               "Occasional" = "occasional",
                               "Frequent" = "frequent",
                               "Unable" = "unable")
  
  speech.prosody.features <- c( "Normal" = "",
                                "Monotone" = "monotonic",
                                "Exaggerated" = "exaggerated"
  )
  
  speech.rhythm.features <- c("Normal" = "",
                              "Halting" = "halting",
                              "Stammering" = "stammering",
                              "Slurred" = "slurred"
  )
  
  speech.content.features.1 <- c("Normal" = "",
                               "Impoverished" = "impoverished",
                               "Mute" = "mute")
  
  speech.content.persev <- c("None" = "",
                                 "Mild" = "mild",
                                 "Moderate" = "moderate",
                                 "Severe" = "severe")
####################################################################
# Speech Tab UI Definition
  MSE.tab.Speech <- function() {
    fluidRow(
      box(
        title = "Speech Rate", status = "primary", width = 3, collapsible = TRUE, solidHeader = TRUE,
        radioButtons( inputId = "Speech.Rate.1", label = NULL, inline = TRUE, 
                      choices = speech.rate.features.1 ),
        tags$hr(style="border-color: grey;"),
        radioButtons( inputId = "Speech.Rate.2", label = NULL, inline = FALSE, 
                      choices = speech.rate.features.2 )
        
      ),
      box(
        title = "Speech Rhythm/Process", status = "primary", width = 5, collapsible = TRUE, solidHeader = TRUE,
        radioButtons(inputId = "Speech.Process.Interrupt", label = "Uninterruptible", inline = TRUE,
                     choices = speech.process.interrupt),
        radioButtons(inputId = "Speech.Process.Circum", label = "Circumstantial", inline = TRUE,
                     choices = speech.process.circum),
        radioButtons(inputId = "Speech.Process.Reorient", label = "Requiring Reorientation", inline = TRUE,
                     choices = speech.process.reorient),
        radioButtons(inputId = "Speech.Prosody", label = "Prosody", inline = TRUE,
                     speech.prosody.features),
        radioButtons(inputId = "Speech.Rhythm", label = "Rhythm", inline = TRUE,
                     speech.rhythm.features)
        
      ),
      box(
        title = "Speech Content", status = "primary", width = 4, collapsible = TRUE, solidHeader = TRUE,
        radioButtons(inputId = "Speech.Content.Features.1", label = "Content", inline = TRUE, 
                     choices = speech.content.features.1),
        radioButtons(inputId = "Speech.Content.Persev", label = "Perseveration", inline = TRUE,
                     choices = speech.content.persev),
        textAreaInput(
          inputId = "Speech.Free.Content", label = "Examples", value = ""),
        actionButton(inputId = "Gen.Narrative.Speech", "Generate"),
        textOutput("Narrative.Speech")
      )
    )
  }

####################################################################
# Restore a speech tab UI from saved data
# pass current.MSE = NULL for a reset
  Restore_MSE.tab.Speech <- function(session, current.MSE){
    updateRadioButtons(session, "Speech.Rate.1", label = NULL, inline = TRUE, 
                       choices = speech.rate.features.1, selected = current.MSE[["Speech.Rate.1"]])
    updateRadioButtons(session, "Speech.Rate.2", label = NULL, inline = FALSE,
                       choices = speech.rate.features.2, selected = current.MSE[["Speech.Rate.2"]])
    updateRadioButtons(session, "Speech.Process.Interrupt", label = "Interruptible", inline = TRUE, 
                       choices = speech.process.interrupt, selected = current.MSE[["Speech.Process.Interrupt"]])
    updateRadioButtons(session, "Speech.Process.Circum", label = "Circumstantial", inline = TRUE, 
                       choices = speech.process.circum, selected = current.MSE[["Speech.Process.Circum"]])
    updateRadioButtons(session, "Speech.Process.Reorient", label = "Requiring Reorientation", inline = TRUE, 
                       choices = speech.process.reorient, selected = current.MSE[["Speech.Process.Reorient"]])
    updateRadioButtons(session, "Speech.Prosody", label = "Prosody", inline = TRUE, 
                       choices = speech.prosody.features, selected = current.MSE[["Speech.Prosody"]])
    updateRadioButtons(session, "Speech.Rhythm", label = "Rhythm", inline = TRUE, 
                       choices = speech.rhythm.features, selected = current.MSE[["Speech.Rhythm"]])
    updateRadioButtons(session, "Speech.Content.Features.1", label = "Content", inline = TRUE, 
                       choices = speech.content.features.1, selected = current.MSE[["Speech.Content.Features.1"]])
    updateRadioButtons(session, "Speech.Content.Persev", inline = TRUE,
                       choices = speech.content.persev, selected = current.MSE[["Speech.Content.Persev"]])
    
    # if current.MSE == NULL, be sure to reset the free text areas
    if( is.null( current.MSE ) ) {
      updateTextAreaInput(session, "Speech.Free.Content", label = "Examples", value = "")
    } else {
      updateTextAreaInput(session, "Speech.Free.Content", label = "Examples", value = current.MSE[["Speech.Free.Content"]])
    }
  }
  
  

####################################################################
# Narrative building from structured data 
  
NarrativeSpeech <- function( current.mse) {
  # rules for building the narrative description of speech process
  speech.df <- current.mse[ , grep("Speech", names(current.mse), value = TRUE) ]
  
  speech.str <- ""
  
  # Was completely normal / unremarkable ?
  if( DataFrameRowEmpty( speech.df ) )
  {
    speech.str <- "Speech was normal in rate, rhythm and tone with content appropriate to context."
    return( speech.str )
  } else {
    speech.str <- "Speech was "
  }
  
  # abnormal ?
  speech.str <- paste0( speech.str, 
    switch( speech.df$Speech.Rate.1,
            "slow" = {"slow in rate"},
            "fast" = {"fast in rate"},
            {"normal in rate"}
    )
  )
  
  # pressured ? latency ?
  speech.str <- paste0( speech.str, 
    switch( speech.df$Speech.Rate.2,
            "mildly pressured" = {" , mildly pressured"},
            "moderately pressured" = {" , moderately pressured"},
            "severely pressured" = {" , severely pressured"},
            "mild initiation latency" = {" with mild initiation latency."},
            "moderate initiation latency" = {" with moderate initiation latency."},
            "severe initiation latency" = {" with severe initiation latency."},
            {"."}
    )
  )
  
    # interruptible ? 
  if( length( grep("pressure", current.mse$Speech.Rate.2, value = TRUE) ) > 0 ) {
    speech.str <- paste0( speech.str, 
                          switch( speech.df$Speech.Process.Interrupt,
                                  "occasionally" = {" but only occasionally uninterruptible."},
                                  "frequently" = {" and frequently uninterruptible."},
                                  "completely"= {" and was completely uninterruptible."},
                                  {" but interruptible."}
                          )
    )
  }
  
  # circumstantial
  speech.str <- paste0( speech.str, 
                        switch( speech.df$Speech.Process.Circum,
                                "occasionally" = {"  There was occasional circumstantiality"},
                                "frequently" = {"  There was frequent circumstantiality"},
                                "consistently"= {"  Speech was consistently circumstantial"},
                                {""}
                        )
  )
  
  # if circumstantial, reorientable ?
  if( speech.df$Speech.Process.Circum != "" ) {
    speech.str <- paste0( speech.str, 
                          switch( speech.df$Speech.Process.Reorient,
                                  "occasional" = {" at times requiring reorientation to topic."},
                                  "frequent" = {" requiring reorientation to topic much of the time."},
                                  "unable"= {" and they were unable to be reoriented to topic."},
                                  {"."}
                          )
    )
  }

  # Prosody / tone
  speech.str <- paste0( speech.str, 
                        switch( speech.df$Speech.Prosody,
                                "monotonic" = {"  Tone was monotonic and lacking in prosody"},
                                "exaggerated" = {"  Tone was exaggerated"},
                                {"  Tone was normal."}
                        )
  )
  
    
  # Rhythm
  speech.str <- paste0( speech.str, 
                        switch( speech.df$Speech.Rhythm,
                                "halting" = {"  with halting rhythm."},
                                "stammering" = {"  with stammering rhythm."},
                                "slurred" = {"  with slurred rhythm."},
                                {""}
                        )
  )
  
  # Content
  if( speech.df$Speech.Content.Features.1 == "mute" ) {
    speech.str <- "Speech process and content was inaccesible as they were mute throughout."
    return( speech.str )
  }
  
  # content normal ?
  if( speech.df[ ,"Speech.Free.Content"] == "" & 
      speech.df[ , "Speech.Content.Features.1" ] == "" &
      speech.df[ , "Speech.Content.Persev"] == "" ) {
    # normal content features (not impoverished or mute) and no content examples
    speech.str <- paste0( speech.str, "  Content was normal and appropriate to context.")
  } else {
    speech.str <- paste0( speech.str, 
                          switch( speech.df$Speech.Content.Features.1,
                                  "impoverished" = {"  Content was impoverished; "},
                                  {"  Content was normal; "}
                          )
    )
    
    speech.str <- paste0( speech.str, 
                          switch( speech.df$Speech.Content.Persev,
                                  "mild" = {"  reflected some mild perseveration; "},
                                  "moderate" = {"  reflected moderate perseveration; "},
                                  "severe" = {" reflected severe perseveration; "},
                                  {""}
                          )
    )
    
    speech.str <- paste0( speech.str, speech.df$Speech.Free.Content
                          )
  }
  return( speech.str )
}