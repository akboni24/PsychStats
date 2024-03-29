library(shiny)
library(sortable)
library(gtsummary)
library(rempsyc)
source("Analyze/analyze-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
indSamplesTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    useShinyjs(),
    titlePanel("Independent Samples T Test"),
    
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
        actionButton(ns("ok"), "OK")
      )
    ),
    fluidRow (
      column (
        width = 10,
        span(textOutput(ns("errors")), style="color:red"),
        textOutput(ns("warning")),
        h3("Two-Sample Statistics"),
        tableOutput(ns("descr")),
        h3("Levene's Test for Equality of Variances"),
        verbatimTextOutput(ns("levene")),
        h3("Two-Sample Test"),
        h5("Equal Variances Assumed"),
        verbatimTextOutput(ns("results")),
        h5("Equal Variances Not Assumed"),
        verbatimTextOutput(ns("results2")),
        h5("Effect Sizes"),
        verbatimTextOutput(ns("esResults")),
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
  )
}

indSamplesTServer <- function(id, data) {
  
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
          text = "Test Variable:",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Grouping Variable:",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
    
    observe({ 
      toggleState(id="ok", 
      condition=length(input$rank_list_2)==1&&length(input$rank_list_3==1)) })
    
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Check that each variable selected is the right type --------------------
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      factor <- check_condition(input$rank_list_3, data(), is.numeric)
      
      if (factor == TRUE) {  # Display warning for the factor variable 
        output$warning <- renderText({factor_warning(input$rank_list_3)})
      }
      
      # Show error message if the dependent variable is not numeric ------------
      if (numeric == FALSE) {  
        output$errors <- renderText({errorText("categorical", "numeric")})
      
      # Otherwise calculate the the t test and display the results -------------
      } else {
        
        # Clear all previous outputs -------------------------------------------
        output$descr <- renderTable({c()})
        output$levene <- renderPrint({c()})
        output$results <- renderPrint({c()})
        output$results2 <- renderPrint({c()})
        output$esResults <- renderPrint({c()})
        
        # Grab the confidence interval -----------------------------------------
        if(is.null(input$confint)) {
          confint = 0.95
        } else {
          confint = input$confint
        }
        
        # Grab each variable: col is dependent variable and grouping is the 
        # factor/grouping variable ---------------------------------------------
        var <- data() %>% pull(input$rank_list_2)
        grouping <- data() %>% pull(input$rank_list_3) %>% as.factor()

        
        # Warning if there are more than two groups ----------------------------
        if (nlevels(grouping) > 2) {
          
          output$results <- renderText({paste("Grouping variable has more than 2 groups. Please select a different variable or conduct a One Way ANOVA.")})
          
        } else {   # Otherwise conduct independent samples test ----------------
          
          
          # Calculate and display descriptives
          descriptives <- indttestStats(var, grouping)
          
          output$descr <- renderTable({
            descriptives
          })
          
          # Calculate levene's test for equality of variances
          levenes <- leveneTest(y=var, group=grouping, center=mean)
          output$levene <- renderPrint({levenes})
          
          # Calculate t test with both equal variances assumed...
          
          results1_df <- t.test(var ~ grouping, conf.level = confint,
                                     var.equal=TRUE)
          
          # ... and equal variances not assumed
          
          results2_df <- t.test(var ~ grouping, conf.level = confint,
                                     var.equal=FALSE) 
          
          # Display the results in two tables
          output$results <- renderPrint({results1_df})
          output$results2 <- renderPrint({results2_df})
          
          # Calculate effect sizes ---------------------------------------------
          if(input$es == TRUE) {
            d <- cohens_d(var ~ grouping, data=data(), ci=confint)
            output$esResults <- renderPrint({d})
          } else {
            d <- "Not Calculated"
          }
          
          # Generate pdf report ------------------------------------------------
        
          # Make parameters to pass to rMarkdown doc
          params <- list(descr = descriptives, results1 = results1_df, 
                         results2 = results2_df, d=d, l = levenes)
          
          # Generate pdf
          output$report <- generate_report("ind_t_test_report", params)
        }
      
      }
    })
      
    
  })
  
}