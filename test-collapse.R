library(shiny)
library(shinydashboard)
library(shinyjs)

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


collapseInput <- function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("collapse")),

    actionButton("bt1", "Collapse box1"),
    actionButton("bt2", "Collapse box2"),
    br(), br(),
    box(id = "box1", collapsible = TRUE, p("Box 1")),
    box(id = "box2", collapsible = TRUE, p("Box 2")),
    collapseInput(inputId = "iscollapsebox1", boxId = "box1"),
    verbatimTextOutput(outputId = "res")
  )
)

server <- function(input, output) {
  observeEvent(input$bt1, {
    print( js$collapse.state("box1") )
    print( js$collapse.state("box2") )
  })
  observeEvent(input$bt2, {
    js$collapse("box2")
  })

  output$res <- renderPrint({
    input$iscollapsebox1
  })
}

shinyApp(ui, server)  