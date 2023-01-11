library(shiny)
library(sortable)
library(purrr)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------

freqUI <- function(id) {
  
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    
    titlePanel("Frequencies"),
  
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 8,
        uiOutput("sortable")),
      column(
        # Buttons
        width = 4,
        actionButton(NS("stat"), "Statistics"),
        actionButton(NS("chart"), "Charts"),
        actionButton(NS("format"), "Format"),
      )
    ), 
    fluidRow (
      column (
        width = 10,
        textOutput("results")
      )
    )
    
  )
}



freqServer <- function(id, df) {
  
  stopifnot(is.reactive(df))
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({names(df())})
    #print(vars())
    
    output$sortable <- renderUI({
      bucket_list(
        header = "Drag the items in any desired bucket",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "variables",
          labels = NULL,
          input_id = NS("rank_list_1")
        ),
        add_rank_list(
          text = "Calculate frequencies for:",
          labels = NULL,
          input_id = NS("rank_list_2")
        ))
    
  })
    observeEvent(input$stat, {
      results_stat <- statsModal(input, output, session, input$rank_list_2)
      output$results <- renderPrint(results_stat)
    })
  })
}

