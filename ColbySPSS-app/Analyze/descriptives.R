library(shiny)
library(sortable)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
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
        textOutput(ns("results"))
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
      bucket_list(
        header = "Variables",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "variables",
          labels = vars(),
          input_id = "rank_list_1"),
        add_rank_list(
          text = "Calculate descriptives for:",
          labels = NULL,
          input_id = "rank_list_2"
        ))
      
    })
    observeEvent(input$stat, {
      results_stat <- statsModal(input, output, session, input$rank_list_2)
      output$results <- renderPrint(results_stat)
    })
  })
}

