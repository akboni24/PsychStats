library(shiny)
library(sortable)
library(knitr)
library(tidyverse)
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/analyze-functions.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/reports/reports-functions.R")
# User Interface ---------------------------------------------------------------
oneSampleTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    titlePanel("One Sample T Test"),
    useShinyjs(),
    fluidRow (
      column (
        width = 12,
        h5("Please drag over one variable to conduct a one sample t test.")
      )
    ),
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
        disabled(actionButton(ns("ok"), "OK")),
        br()
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
    
    observe({ toggleState(id="ok", condition=length(input$rank_list_2) == 1) })
    
    # Show Options modal if selected -------------------------------------------
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Clear all previous outputs ---------------------------------------------
      output$descr <- renderTable({c()})
      output$results <- renderPrint({c()})
      output$esResults <- renderPrint({c()})
      
      # Extract the variable from the data -------------------------------------
      col <- data() %>% pull(input$rank_list_2)
      
      # Check to see if the variable is numeric 
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      
      # If variable is not numeric, print an error message
      if (numeric == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
      
      # Otherwise, calculate the t test and display the results ----------------
      } else {
        
        # Calculate and display descriptives -----------------------------------
        descriptives <- ttestStats(col, c(input$rank_list_2))
        output$descr <- renderTable({
          descriptives
        })
        
        # Grab confidence interval ---------------------------------------------
        if (is.null(input$confint)) {
          confint = 0.95
        } else {
          confint = input$confint
        }
        
        # Calculate effect sizes -----------------------------------------------
        if(input$es == TRUE) {
          d <- cohens_d(col, mu=input$testValue, ci=confint)
          output$esResults <- renderPrint({d})
        } else {
          d <- "Not calculated"
        }
        
        x <- data() %>% pull(input$rank_list_2)
        # Conduct the one sample t test ----------------------------------------
        results_df <- t.test(x=x, mu=input$testValue, 
                             conf.level = confint)

        output$results <- renderPrint({results_df})
        
        # Generate the downloadable pdf report ---------------------------------
        params <- list(descr = descriptives, results = results_df, d = d)
        
        output$report <- generate_report("t_test_report", params)
      }
      
      
    })
  
    
    
  })
  
}