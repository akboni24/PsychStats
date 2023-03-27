library(shiny)
library(sortable)
library(gtsummary)
library(rempsyc)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
indSamplesTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
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
        # Should hide the OK button until the user has moved at least one variable....
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
          text = "Test Variable(s):",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Grouping Variable(s):",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
  
    
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      factor <- check_condition(input$rank_list_3, data(), is.numeric)
      
      if (factor == TRUE) {
        output$warning <- renderText({factor_warning(input$rank_list_3)})
      }
      
      if (numeric == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
      } else {
        
        if(is.null(input$confint)) {
          confint = 0.95
        } else {
          confint = input$confint
        }
        
        col <- data() %>% pull(input$rank_list_2)
        grouping <- data() %>% pull(input$rank_list_3) %>% as.factor()
        
        descriptives <- indttestStats(col, grouping)
        
        output$descr <- renderTable({
          descriptives
        })
        
        
        # Warning if there are more than two groups ----------------------------
        if (nlevels(grouping) > 2) {
          
          output$results <- renderText({paste("Grouping variable has more than 2 groups. 
                            Please select a different variable or conduct a One Way ANOVA.")})
          
        } else{   # Otherwise conduct independent samples test -----------------
          
          # Calculate t test with both equal variances assumed...
          results <- t.test(col ~ grouping, alternative="two.sided", data = data(), 
                            na.rm = TRUE, conf.level = confint, var.equal = TRUE)
          # ... and equal variances not assumed
          results2 <- t.test(col ~ grouping, alternative="two.sided", data = data(), 
                             na.rm = TRUE, conf.level = confint, var.equal = FALSE)

          output$results <- renderPrint({results})
          output$results2 <- renderPrint({results2})
          
          # Calculate effect sizes
          if(input$es == TRUE) {
            # Check this
            d <- cohensD(col ~ grouping)
            output$esResults <- renderPrint({cohensD(col ~ grouping)})
          } else {
            d <- "Not Calculated"
          }
          
          # Generate pdf report ------------------------------------------------
          # Tidy results
          # results1_df <- tidy(results)
          results2_df <- tidy(results2)
          
          results1_df <- nice_t_test(data=data(), response=input$rank_list_2,
                                     group=input$rank_list_3, var.equal=TRUE)
          
          # Make parameters to pass to rMarkdown doc
          params <- list(descr = descriptives, results1 = results1_df, 
                         results2 = results2_df, cohens = d)
          
          # Generate pdf
          output$report <- generate_report("ind_t_test_report", params)
        }
      
      }
    })
      
    
  })
  
}