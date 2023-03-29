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
        tableOutput(ns("results")),
        h5("Effect Sizes"),
        verbatimTextOutput(ns("esResults")),
        downloadButton(ns("report"), label = "Generate PDF")
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
        
      # Otherwise, calculate the t test and print the results ------------------
      } else {
      
        # Pull each variable from the dataset ----------------------------------
        col1 <- data() %>% pull(input$rank_list_2)
        col2 <- data() %>% pull(input$rank_list_3)
      
        
        # Make descriptives table ----------------------------------------------
        descriptives <- ttestStats(col1, col2)
        output$descr <- renderTable({
          descriptives
        })
        
        # Calculate cohen's d --------------------------------------------------
        if(input$es == TRUE) {
          d <- cohens_d(col1, col2, paired=TRUE, ci=confint)
          output$esResults <- renderPrint({d})
        } else {
          d <- "Not calculated"
        }
        
        # Preparing the data (Converting to long format) -----------------------
        var_list <- c(input$rank_list_2, input$rank_list_3)
        
        data_prepared <- data() %>%
          gather(key="within_var", value="dependent_var", var_list) %>%
          convert_as_factor(within_var)
      
        
        # Conduct the paired samples t test ------------------------------------
        results_df <- nice_t_test(data=data_prepared, response="dependent_var",
                                  group="within_var", paired=TRUE,
                                  conf.level = confint)
        
        output$results <- renderTable({results_df})
        
        # Generate the downloadable pdf report ---------------------------------
        params <- list(descr = descriptives, results = results_df)
        
        output$report <- generate_report("paired_samples_report", params)
        
        
      }
      
      
    })
    
  })
  
}