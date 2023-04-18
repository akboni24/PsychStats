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
        h3("Covariances"),
        verbatimTextOutput(ns("cov"))
      )
    ),
    fluidRow(
      column(
        width=12,
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
  )
}

correlationServer <- function(id, data) {
  
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
      showModal(corrOptionsModal(input, output, session))
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
      
      if ("Means and standard deviations" %in% input$stats) {
        descr <- sapply(d, means_and_sd)
        output$descr <- renderPrint({
          descr
        })
      } else {
        descr <- "Not Calculated"
      }
      
      if ("Covariances" %in% input$stats) {
        cov <- cov(d)
        output$cov <- renderPrint ({
          cov
        })
      } else {
        cov <- "Not Calculated"
      }

      params <- list(descr=descr, results=results, cov=cov)
      
      output$report <- generate_report("correlation_report", params)
      
      
    })
    
  })
  
}