library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
oneWayAnovaUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("One Way ANOVA"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable"))
      ),
      column(
        # Buttons
        width = 2,
        # actionButton(ns("contrasts"), "Contrasts"),   Didn't do contrasts in PS215, so I wasn't going to include
        actionButton(ns("posthoc"), "Post Hoc"),
        actionButton(ns("options"), "Options")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        checkboxInput(ns("es"), label = "Estimate effect sizes for overall tests", value=FALSE)
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
        h3("Statistics"),
        verbatimTextOutput(ns("statsresults")),
        h3("ANOVA"),
        tableOutput(ns("results")),
        h3("Effect Sizes"),
        tableOutput(ns("esResults")),
        h3("Post Hoc Tests"),
        verbatimTextOutput(ns("phTests"))
      )
    )
  )
}

oneWayAnovaServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    # Display the drag and drop buckets ----------------------------------------
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
          text = "Dependent List: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Factor: ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
    
    # Check that the variables are numeric and factor --------------------------
    observeEvent(input$rank_list_2, {
      num <- checkNumeric(input$rank_list_2, data())
      shinyFeedback::feedbackWarning("rank_list_2", !num, text = "Please select a numeric variable")
    })
    
    observeEvent(input$rank_list_3, {
      factor <- checkFactor(input$rank_list_3, data())
      shinyFeedback::feedbackWarning("rank_list_3", !factor, text = "Please select a categorical variable")
      
    })
    
    # Show post hoc and options modals if selected -----------------------------
    observeEvent(input$posthoc, {
      showModal(anovaPostHocModal(input, output, session))
    }) 
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$options, {
      showModal(anovaOptionsModal(input, output, session))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Calculate the ANOVA and display the results in a table -----------------
      col1 <- data() %>% pull(input$rank_list_2)
      col2 <- as.factor(data() %>% pull(input$rank_list_3))
      anovaResults <- anova(lm(col1 ~ col2))
      
      output$results <- renderTable({
        return(anovaResults)
      })
      
      # Calculate chosen statistics --------------------------------------------
      if (!is.null(input$stat)) {
        output$statsresults <- renderPrint({
          anovaOptionsCalc(input$stat, col1 ~ col2, col1, col2)
        })
      }
      
      # Calculate effect sizes -------------------------------------------------
      esResults <- list()
      if (input$es == TRUE) {
        options(es.use_symbols = TRUE)
        output$esResults <- renderTable({
          return(etaSquared(lm(col1 ~ col2)))
        })
      }
      
      # Calculate post hoc tests -----------------------------------------------
      if(is.null(input$confint)) {
        confint = 0.95
      } else {
        confint = input$confint
      }
      
      if (!is.null(input$eva)) {
        output$phTests <- renderPrint({
          postHocCalc(input$eva, col1, col2, confint)
        })
      }
      
    })
    
  })
  
}