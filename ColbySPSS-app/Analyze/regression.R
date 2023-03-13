library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(ppcor)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/regression-functions.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
regressionUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Linear Regression"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable")),
      ),
      column(
        # Buttons
        width = 2,
        actionButton(ns("stats"), "Statistics"),
        # Note - got rid of options bc R does stepwise regression by AIC
        # maybe include a message about this in the output
      )
    ), 
    fluidRow(
      column(
        width = 10,
        selectInput(ns("method"), label="Method: ", choice=c("Enter", "Stepwise")),
        # find more options
        # Should hide the OK button until the user has moved at least one variable....
        actionButton(ns("ok"), "OK")
      )
    ),
    fluidRow (
      column (
        width = 10,
        span(textOutput(ns("errors")), style="color:red"),
        h3("Descriptives"),
        verbatimTextOutput(ns("descr")),
        h3("Correlations"),
        verbatimTextOutput(ns("corr")),
        h3("Linear Regression Model"),
        verbatimTextOutput(ns("linreg")),
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
          text = "Dependent: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Independent(s): ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
  
    
    # Show stats and options modals if selected -----------------------------
    observeEvent(input$stats, {
      showModal(lrStatsModal(input, output, session))
    }) 
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    observeEvent(input$options, {
      showModal(lrOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Make sure selected dependent variable is numeric -----------------------
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      
      if (numeric == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
      } else {
      
        # Calculate the linear regression model ----------------------------------
        x <- extractCols(input$rank_list_3, data())
        y <- data() %>% pull(input$rank_list_2)
        
        d <- data.frame(y, x)
        
        if (input$method == "Stepwise") {
          # Conduct forward stepwise procedure ---------------------------------
          intercept_only <- lm(y ~ 1, data=data())
          all <- lm(d)
          model <- step(intercept_only, direction='forward', scope=formula(all))
          toPrint <- list(model$anova, model$coefficients)
        } else {
          model <- lm(d)
          toPrint <- summary(model)
        }
        
        # Print results of regression ------------------------------------------
        output$linreg <- renderPrint({
          toPrint
        })
        
        # Calculate and display descriptives -----------------------------------
        if ("Descriptives" %in% input$other) {
          output$descr <- renderPrint({
            summary(d)
          })
        }
        
        # Calculate and display partial correlations ---------------------------
        if ("Part and partial correlations" %in% input$other) {
          output$corr <- renderPrint ({
            pcor(d)
          })
        }
        
        # Calculate and display chose statistics -------------------------------
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
        
      }
      
    })
    
  })
  
}