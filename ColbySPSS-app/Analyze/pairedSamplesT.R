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
        span(textOutput(ns("errors")), style="color:red"),
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
    
  
    # Show options modal if selected -------------------------------------------
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      # Grab the confidence interval -------------------------------------------
      if(is.null(input$confint)) {
        confint = 0.95
      } else {
        confint = input$confint
      }
      
      
      # Check that each variable is numeric, if not, stop calc and print error -
      numeric1 <- check_condition(input$rank_list_2, data(), is.numeric)
      numeric2 <- check_condition(input$rank_list_3, data(), is.numeric)
      if (numeric1 == FALSE | numeric2 == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
      } else {
      
        # Pull each variable from the dataset ----------------------------------
        col1 <- data() %>% pull(input$rank_list_2)
        col2 <- data() %>% pull(input$rank_list_3)
      
        
        # Make descriptives table ----------------------------------------------
        output$descr <- renderTable({
          ttestStats(col1, col2)
        })
        
        # Calculate cohen's d --------------------------------------------------
        if(input$es == TRUE) {
          # Check this
          output$esResults <- renderPrint({cohensD(col1, col2)})
        }
        
        # Calculate t test and print results -----------------------------------
        results <- t.test(col1, col2, paired = TRUE, conf.level = confint)
        output$results <- renderPrint({results})
        
        
      }
      
      
    })
    
  })
  
}