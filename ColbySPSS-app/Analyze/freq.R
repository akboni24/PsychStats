library(shiny)
library(sortable)

# User Interface ---------------------------------------------------------------
freqUI <- function(id) {
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    
    titlePanel("Frequencies"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 8,
        bucket_list(
          header = "Drag the items in any desired bucket",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          add_rank_list(
            text = "variables",
            labels = list(
              "variable1",
              "variable2",
              "variable3"
            ),
            input_id = ns("rank_list_1")
          ),
          add_rank_list(
            text = "Calculate frequencies for:",
            labels = NULL,
            input_id = ns("rank_list_2")
          ))
      ),
      column(
        # Buttons
        width = 4,
        actionButton(ns("stat"), "Statistics"),
        actionButton(ns("chart"), "Charts"),
        actionButton(ns("format"), "Format"),
      )
    )
  )
}

freqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    freq_args <- as.list(formals(freq))
    percSelections <- reactive({input$percValues})
    centSelections <- reactive({input$centen})
    dispSelections <- reactive({input$disp})
    distSelections <- reactive({input$dist})
    
    if ("Mean" %in% percSelections) {
      for (i in input$rank_list_2) {
        renderPrint(mean(i))
      }
    }
    
    renderPrint(
      input$rank_list_1 # This matches the input_id of the first rank list
    )
    output$results_2 <-
      renderPrint(
        input$rank_list_2 # This matches the input_id of the second rank list
      )
    output$results_3 <-
      renderPrint(
        input$bucket_list_group # Matches the group_name of the bucket list
      )
  })
}

