library(shiny)
library(sortable)
source("Analyze/analyze-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
descriptivesUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),

    titlePanel("Descriptives"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 8,
        uiOutput(ns("sortable"))),
        textOutput(ns("numeric")),
      column(
        # Buttons
        width = 4,
        actionButton(ns("options"), "Options")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        # Should hide the OK button until the user has moved at least one variable....
        actionButton(ns("ok"), "OK"),
        actionButton(ns("cancel"), "Cancel")
      )
    ),
    fluidRow (
      column (
        width = 10,
        span(textOutput(ns("errors")), style="color:red"),
        h5("Descriptives"),
        verbatimTextOutput(ns("results")),
        h5("Central Tendency"),
        verbatimTextOutput(ns("centResults")),
        h5("Dispersion"),
        verbatimTextOutput(ns("dispResults")),
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
    
  )
}

descriptivesServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    output$sortable <- renderUI({
      ns <- NS(id)
      bucket_list(
        header = NULL,
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "Variables",
          labels = vars(),
          input_id = ns("rank_list_1")),
        add_rank_list(
          text = "Calculate descriptives for:",
          labels = NULL,
          input_id = ns("rank_list_2")
        ))
      
    })
    
  
    observeEvent(input$options, {
      showModal(descOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      # Extract the columns of data
      cols <- extractCols(input$rank_list_2, data())
      
      # Check that each variable selected is a numeric variable - if not,
      # do not continue and print an error message 
      num <- lapply(input$rank_list_2, check_condition, data(), is.numeric)
      if (FALSE %in% num) {
        output$errors <- renderText({error_text("categorical", "numeric")})
        
      } else {
      
        # Descriptives ---------------------------------------------------------
        descriptives <- lapply(cols, summary)
        output$results <- renderPrint({descriptives})
        
        # Central Tendency -----------------------------------------------------
        centen_results <- c()
        if (!is.null(input$desc)) {
          centen_results <- centraltendency(cols, input$desc)
        }
        
        if (!is.null(centen_results)) {
          output$centResults <- renderPrint({centen_results})
        }
        
        # Dispersion -----------------------------------------------------------
        disp_results <- c()
        if (!is.null(input$disp)) {
          disp_results <- dispersion(cols, input$disp)
        }
        
        if (!is.null(disp_results)) {
          output$dispResults <- renderPrint({disp_results})
        }
        
        # Generate the downloadable pdf report ---------------------------------
        params <- list(descr = descriptives, centen = centen_results, 
                       disp = disp_results)
        
        output$report <- generate_report("descriptives_report", params)
      
      }
      
    })
    
  })

}

