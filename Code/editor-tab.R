# Editor Tab Code

# to install shinyMCE
# install.packages("RJSONIO")
# devtools::install_github("mul118/shinyMCE")


####################################################################
# Control Tab UI Definition
Editor.tab.UI <- function() {
  fluidRow(
    box(
      title = "Note Editor", status = "warning", width = NULL, collapsible = FALSE, solidHeader = TRUE,
      tinyMCE('note.editor', 'Update button has been pressed 0 times',
              options = "width: 600, 
                                height: 300, 
                                plugins : ['advlist autolink image lists charmap'],
                                menubar: 'edit insert view format',
                                content_style: '.mce-content-body {font-size:26px;font-family:Arial,sans-serif;}'
                                "),
      br(),
      actionButton('update_editor1', 'Update Editor'),
      hr(),
      h2('Editor Content:'),
      verbatimTextOutput('note.editor_content'),
      hr(),
      textAreaInput("plain.text", "test type some stuff here", value = "")
    )
  )
}

####################################################################
# Server functions
Editor.Server <- function( input, output, session ) {
  observeEvent( input$update_editor1, {
    updateTinyMCE(session, 'note.editor', input$plain.text)
  })  
  
  output$note.editor_content <- renderPrint({input$note.editor})
}    



####################################################################
# Functionality for actionButtons

