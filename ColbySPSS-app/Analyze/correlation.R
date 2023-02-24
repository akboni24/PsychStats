library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(ppcor)
library("Hmisc")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/regression-functions.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
correlationUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Correlation: Bivariate"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable")),
      ),
      column(
        # Buttons
        width = 2,
        actionButton(ns("options"), "Options")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        checkboxGroupInput(ns("coef"), label="Correlation Coefficients: ", 
                           c("Pearson", "Spearman"),
                           selected="Pearson"),
        radioButtons(ns("sig"), label="Test of Significance", 
                     c("Two-tailed", "One-tailed")),
        # Should hide the OK button until the user has moved at least one variable....
        actionButton(ns("ok"), "OK")
      )
    ),
    fluidRow (
      column (
        width = 10,
        h3("Descriptives"),
        verbatimTextOutput(ns("descr")),
        h3("Correlations"),
        verbatimTextOutput(ns("corr")),
        h3("Statistics"),
        verbatimTextOutput(ns("stats"))
      )
    )
  )
}

regressionServer <- function(id, data) {
  
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
          text = "Variables: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ))
      
    })
    
    
    # Show options modal if selected -----------------------------
    observeEvent(input$options, {
      showModal(lrOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Calculate the correlation matrix ---------------------------------------
      x <- extractCols(input$rank_list_2, data())
      d <- data.frame(x)
      
      if (input$coef == "Spearman") {
        results <- rcorr(as.matrix(d), type="spearman")
      } else {
        results <- rcorr(as.matrix(d), type="pearson")
      }
      
      output$corr <- renderPrint({
        results
      })
      
      if ("Descriptives" %in% input$other) {
        output$descr <- renderPrint({
          summary(d)
        })
      }
      
      if ("Part and partial correlations" %in% input$other) {
        output$corr <- renderPrint ({
          pcor(d)
        })
      }
      
      stats_results <- list()
      if ("Confidence Intervals (95%)" %in% input$regcoef) {
        stats_results <- append(stats_results, "Confidence Intervals")
        stats_results <- append(stats_results, confint(model))
      }
      
      if ("Covariance Matrix" %in% input$regcoef) {
        stats_results <- append(stats_results, "Covariance Matrix")
        stats_results <- append(stats_results, cov(d))
      }
      
      if ("Collinearity diagnostics" %in% input$regcoef) {
        stats_results <- append(stats_results, "Collinearity Diagnostics")
        stats_results <- append(stats_results, vif(model))
      }
      
      
      output$stats <- renderPrint ({
        stats_results
      })
      
      
      
      
      
    })
    
  })
  
}