library(shiny)
library(sortable)
library(lsr)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
pairedSamplesTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Paired Samples T Test"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable"))
      ),
      column(
        # Buttons
        width = 2,
        actionButton(ns("options"), "Options"),
      )
    ), 
    fluidRow(
      column(
        width = 10,
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
        h5("Paired-Samples Statistics"),
        tableOutput(ns("descr")),
        h5("Paired Samples Test"),
        verbatimTextOutput(ns("results")),
        h5("Effect Sizes"),
        verbatimTextOutput(ns("esResults"))
      )
    )
  )
}

pairedSamplesTServer <- function(id, data) {
  
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
          text = NULL,
          labels = vars(),
          input_id = ns("rank_list_1")),
        add_rank_list(
          text = "Variable 1: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Variable 2: ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
    
    
    observeEvent(input$rank_list_2, {
      num <- checkNumeric(input$rank_list_2, data())
      shinyFeedback::feedbackWarning("rank_list_2", !num, text = "Please select a numeric variable")
    })
    
    observeEvent(input$rank_list_3, {
      num <- checkNumeric(input$rank_list_3, data())
      shinyFeedback::feedbackWarning("rank_list_3", !num, text = "Please select a numeric variable")
      
    })
    
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      if(is.null(input$confint)) {
        confint = 0.95
      } else {
        confint = input$confint
      }
      
      col1 <- data() %>% pull(input$rank_list_2)
      col2 <- data() %>% pull(input$rank_list_3)
      
      output$descr <- renderTable({
        ttestStats(col1, col2)
      })
      
      results <- t.test(col1, col2, paired = TRUE, conf.level = confint)
      
      if(input$es == TRUE) {
        # Check this
        output$esResults <- renderPrint({cohensD(col1, col2)})
      }
      
      # Come back to this, make the output an R Markdown file
      # Should store results as a dictionary of the function/variable and the result
      output$results <- renderPrint({results})
      
      
    })
    
  })
  
}