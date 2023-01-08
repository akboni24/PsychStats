library(shiny)
library(sortable)
freqUI <- function(id) {
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    
    titlePanel("Frequencies"),
      
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
      width = 4,
      actionButton(ns("stat"), "Statistics"),
      actionButton(ns("chart"), "Charts"),
      actionButton(ns("format"), "Format"),
    )
    ),
    fluidRow(
      column(
        width = 12,
        tags$b("Result"),
        column(
          width = 12,
          
          tags$p("input$rank_list_1"),
          verbatimTextOutput("results_1"),
          
          tags$p("input$rank_list_2"),
          verbatimTextOutput("results_2"),
          
          tags$p("input$bucket_list_group"),
          verbatimTextOutput("results_3")
        )
      )
    
    )
  )
}

freqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
     observeEvent(input$show, {
       showModal(modalDialog(
         title = "Frequencies: Statistics",
         checkboxGroupInput(ns("percValues"), label = "Percentile Values", c("Quartiles", "Cut points for 10 equal groups", "Percentiles")),
         checkboxGroupInput(ns("centen"), label = "Central Tendency", c("Mean", "Median", "Mode", "Sum")),
         checkboxGroupInput(ns("disp"), label = "Dispersion", c("Std. Deviation", "Variance", "Range", "Minimum", "Maximum")),
         checkboxGroupInput(ns("dist"), label = "Distribution", c("Skewness", "Kurtosis")),
         footer = tagList(modalButton("Cancel"), actionButton("submit", "Submit"))
         
       ))
     })
    
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
