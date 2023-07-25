library(shiny)
library(sortable)
library(purrr)
library(epiDisplay)
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------

freqUI <- function(id) {

  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    useShinyjs(),
    titlePanel("Frequencies"),
  
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
        plotOutput(ns("chart_results"))
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
          
          chart <- freqCharts(data(), var, input$type, input$values)
          
          output$chart_results <- renderPlot({chart})
          
        }
        
    }

  })
  
  })
}


