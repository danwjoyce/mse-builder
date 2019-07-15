library(shiny)
library(shinyMCE)

ui <- fluidPage(
  fluidRow(
    column(6, offset = 3,
           hr(),
           h2('tinyMCE Editor:'),
           tinyMCE('editor1', 'Update button has been pressed 0 times',
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
           verbatimTextOutput('editor1_content'),
           hr(),
           textAreaInput("plain.text", "type some shit", value = "")
    )
  )
)

server <- function(input, output, session) { 
    
    # observe({
    #   updateTinyMCE(session, 'editor1', input$plain.text)
    # })
  
    observeEvent( input$update_editor1, {
      updateTinyMCE(session, 'editor1', input$plain.text)
    })  
  
    output$editor1_content <- renderPrint({input$editor1})
  }

shinyApp(ui = ui, server = server)