library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(ppcor)
source("Analyze/regression-functions.R", local=TRUE)
source("Analyze/analyze-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
regressionUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    useShinyjs(),
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
        h5("Confidence Intervals"),
        verbatimTextOutput(ns("ci")),
        h5("Covariance Matrix"),
        verbatimTextOutput(ns("covariances")),
        h5("Collinearity Diagnostics"),
        verbatimTextOutput(ns("coll"))
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
  
    observe({ 
      toggleState(id="ok", 
        condition=length(input$rank_list_2)==1&&length(input$rank_list_3>=1)) })
    
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
          descriptives <- summary(d)
          output$descr <- renderPrint({
            descriptives
          })
        } else {
          descriptives <- "Not Calculated"
        }
        
        # Calculate and display partial correlations ---------------------------
        if ("Part and partial correlations" %in% input$other) {
          pcor <- pcor(d)
          output$corr <- renderPrint ({
            pcor
          })
        } else {
          pcor <- "Not Calculated"
        }
        
        # Calculate and display chose statistics -------------------------------
        if ("Confidence Intervals (95%)" %in% input$regcoef) {
          confints <- confint(model)
          output$ci <- renderPrint({
            confints
          })

        } else {
          confints <- "Not Calculated"
        }
        
        if ("Covariance matrix" %in% input$regcoef) {
          covs <- cov(d)
          output$covariances <- renderPrint({
            covs
          })
        } else {
          covs <- "Not Calculated"
        }
        
        if ("Collinearity diagnostics" %in% input$other) {
          colls <- vif(model)
          output$coll <- renderPrint({
            colls
          })
        } else {
          colls <- "Not Calculated"
        }
      
        params <- list(descr=descriptives, reg=toPrint, pcor=pcor, ci=confints, 
                       cov=covs, coll=colls)
        
        output$report <- generate_report("regression_report", params)
        
      }
      
    })
    
  })
  
}