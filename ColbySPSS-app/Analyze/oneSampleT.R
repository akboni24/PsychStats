library(shiny)
library(sortable)
library(rmarkdown)
library(knitr)
library(tidyverse)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/reports/reports-functions.R")
# User Interface ---------------------------------------------------------------
oneSampleTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    shinyFeedback::useShinyFeedback(),
    titlePanel("One Sample T Test"),
    
    # Creates two drag and drop buckets ----------------------------------------
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
        span(textOutput(ns("errors")), style="color:red"),
        h5("One-Sample Statistics"),
        tableOutput(ns("descr")),
        h5("One-Sample Test"),
        verbatimTextOutput(ns("results")),
        h5("Effect Sizes"),
        verbatimTextOutput(ns("esResults")),
        downloadButton(ns("report"), label = "Generate PDF")
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
    
    # Show Options modal if selected -------------------------------------------
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Extract the variable from the data -------------------------------------
      col <- data() %>% pull(input$rank_list_2)
      
      # Check to see if the variable is numeric - if not, do not continue and 
      # print an error message
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      
      if (numeric == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
        
      } else {
        
        descriptives <- ttestStats(col)
        output$descr <- renderTable({
          descriptives
        })
        
        # Grab confidence interval ---------------------------------------------
        if (is.null(input$confint)) {
          confint = 0.95
        } else {
          confint = input$confint
        }
        
        if(input$es == TRUE) {
          # Check this
          output$esResults <- renderPrint({cohensD(col, input$testValue)})
        }

        results <- t.test(col, mu=input$testValue, alternative="two.sided", 
                          conf.level = confint)
        
       
        output$results <- renderPrint({results})
        results_df <- tidy(results)
        
        params <- list(descr = descriptives, results = results_df, var = col,
                                           test = input$testValue)
        
        output$report <- generate_report("t_test_report", params)
      }
      
      
    })
  
    
    
  })
  
}