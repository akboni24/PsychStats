library(shiny)
library(sortable)
library(purrr)
library(epiDisplay)
source("Analyze/analyze-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------

freqUI <- function(id) {

  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    useShinyjs(),
    titlePanel("Frequencies"),
    fluidRow(
      column (
        width = 12,
        h5("Note: This page is best used for summarizing categorical or discrete variables.")
      )
    ),
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 8,
        uiOutput(ns("sortable"))),
      column(
        # Buttons
        width = 4,
        actionButton(ns("chart"), "Charts")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        checkboxInput(ns("apa"), label="APA style tables", 
                      value=FALSE)
      )
    ),
    fluidRow(
      column(
        width = 10,
        actionButton(ns("ok"), "OK"),
        actionButton(ns("cancel"), "Cancel")
      )
    ),
    fluidRow (
      column (
        width = 10,
        span(textOutput(ns("errors")), style="color:red"),
        textOutput(ns("warning")),
        h3("Frequency Table"),
        verbatimTextOutput(ns("frequencies")),
        h3("Charts"),
        plotOutput(ns("chart_results")),
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
    
  )
}

# Server Function --------------------------------------------------------------
freqServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    output$sortable <- renderUI({
      ns <- NS(id)
      bucket_list(
        header = "Drag over the categorical variable you would like to calculate frequencies for:",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "Variables",
          labels = vars(),
          input_id = ns("rank_list_1")),
        add_rank_list(
          text = "Calculate frequencies for:",
          labels = NULL,
          input_id = ns("rank_list_2")
        ))
    
  })
    
  # Enable OK button once the user has selected at least one variable ----------
  observe({ toggleState(id="ok", condition=length(input$rank_list_2) == 1) })
  
  # Display pop-up menus if selected -------------------------------------------
  observeEvent(input$chart, {
    showModal(freqChartsModal(input, output, session))
  })
  
  observeEvent(input$submit, {
    removeModal()
  })
  
  # Wait for the user to hit submit --------------------------------------------
  observeEvent(input$ok, {
    
    # Clear all previous outputs -------------------------------------------
    output$frequencies <- renderPrint({c()})
    output$chart_results <- renderPlot({c()})
    
    # Extract variables --------------------------------------------------------
    var <- data() %>% pull(input$rank_list_2) %>% as.factor()
    
    factor <- check_condition(input$rank_list_2, data(), is.numeric)
    
    # Display warning if chosen factor variable is numeric ---------------------
    if (factor == TRUE) {
      output$warning <- renderText({factor_warning(input$rank_list_3)})
    }
    
    # Calculate frequencies ----------------------------------------------------
    if (input$apa == TRUE) {
      results <- tab1(var, cum.percent = FALSE)
    } else {
      results <- tab1(var, cum.percent=TRUE)
    }
    
    output$frequencies <- renderPrint({print(results)})
    

    # Plot Charts --------------------------------------------------------------
    if (!is.null(input$chart)) {
      
        if (!is.null(input$type)) {
          
          chart <- freqCharts(data(), var, c(input$rank_list_2), input$type, input$values, input$normal)
          
          output$chart_results <- renderPlot({chart})
          
        } else {
          chart <- c()
        }
        
    } else {
      chart <- c()
    }
    
    # Generate pdf report ------------------------------------------------------
    
    # Make parameters to pass to rMarkdown doc
    params <- list(freq = results, chart = chart)
    
    # Generate pdf
    output$report <- generate_report("frequencies_report", params)

  })
  
  })
}


