library(shiny)
library(sortable)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
descriptivesUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    shinyFeedback::useShinyFeedback(),
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
        actionButton(ns("options"), "Options"),
        actionButton(ns("style"), "Style"),
        actionButton(ns("bootstrap"), "Bootstrap")
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
        h5("Descriptives"),
        verbatimTextOutput(ns("results")),
        h5("Central Tendency"),
        verbatimTextOutput(ns("centResults")),
        h5("Dispersion"),
        verbatimTextOutput(ns("dispResults"))
        
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
    
    observeEvent(input$rank_list_2, {
      
      req(input$rank_list_2)
      if (checkNumeric(input$rank_list_2, data()) == FALSE) {
        output$numeric <- renderPrint({data() %>% subset(select=input$rank_list_2) %>% class()})
      }
      
    })
  
    
    observeEvent(input$options, {
      showModal(descOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      cols <- extractCols(input$rank_list_2, data())
      
      # Descriptives -----------------------------------------------------------
      descriptives <- lapply(cols, summary)
      
      # Central Tendency -------------------------------------------------------
      centen_results <- NULL
      if (!is.null(input$desc)) {
        centen_results <- centraltendency(cols, input$desc)
      }
      
      # Dispersion -------------------------------------------------------------
      disp_results <- NULL
      if (!is.null(input$disp)) {
        disp_results <- dispersion(cols, input$disp)
      }
      
      # Come back to this, make the output an R Markdown file
      # Should store results as a dictionary of the function/variable and the result
      output$results <- renderPrint({descriptives})
      
      if (!is.null(centen_results)) {
        output$centResults <- renderPrint({centen_results})
      }
      if (!is.null(disp_results)) {
        output$dispResults <- renderPrint({disp_results})
      }
      
    })
    
  })

}

