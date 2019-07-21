# Patients tab

Patients.tab.UI <- function() {
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

####################################################################
# Patients tab server code

Patients.tab.Server <- function(input, output, session) {

  # the datatable for displaying the index (all patients + MSEs)
  output$pts.table <- RefreshPatientTable(index.df)
  
  observeEvent(input$MSEload, {
    # establish row selected, then build filename to retrieve
    row.idx  <- input$pts.table_rows_selected
    if( !is.null( row.idx ) ) {
      this.row <- index.df[ row.idx, ]
      this.fname <- paste0( data.path, this.row$Hash.fname )
      current.scope.MSE <<- LoadMSE(input, this.fname)
  
      current.scope.fname <<- this.fname
      current.scope.fname.hash <<- this.row$Hash.fname
  
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
      suppressWarnings( file.remove(del.fname) )
      index.df <<- SortIndexTable( index.df[ -row.idx, ] )
      output$pts.table <- RefreshPatientTable(index.df)
      saveRDS( index.df, index.path )
    }
  })
}