library(shiny)
library(sortable)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
oneSampleTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("One Sample T Test"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 8,
        uiOutput(ns("sortable"))),
      column(
        # Buttons
        width = 4,
        actionButton(ns("options"), "Options")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        numericInput(ns("testValue"), label = "Test Value: ", value = 0),
        checkboxInput(ns("es"), label = "Estimate effect sizes", value=TRUE)
      )
    ),
    fluidRow(
      column(
        width = 10,
        # Should hide the OK button until the user has moved at least one variable....
        actionButton(ns("ok"), "OK")
      )
    ),
    fluidRow (
      column (
        width = 10,
        h3("One-Sample Statistics"),
        tableOutput(ns("descr")),
        h3("One-Sample Test"),
        verbatimTextOutput(ns("results"))
      )
    )
    
  )
}

oneSampleTServer <- function(id, data) {
  
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
          text = "Test Variable(s):",
          labels = NULL,
          input_id = ns("rank_list_2")
        ))
      
    })
    
    observeEvent(input$rank_list_2, {
      num <- checkNumeric(input$rank_list_2, data())
      shinyFeedback::feedbackWarning("rank_list_2", !num, text = "Please select a numeric variable")
    })
    
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      cols <- data() %>% pull(input$rank_list_2)
      
      output$descr <- renderTable({
        ttestStats(data() %>% pull(input$rank_list_2))
      })
      
      
      if(is.null(input$confint)) {
        confint = 0.95
      } else {
        confint = input$confint
      }

      results <- t.test(cols, mu=input$testValue, alternative="two.sided", conf.level = confint)
      
      # Come back to this, make the output an R Markdown file
      # Should store results as a dictionary of the function/variable and the result
      output$results <- renderPrint({results})
      
    })
    
  })
  
}