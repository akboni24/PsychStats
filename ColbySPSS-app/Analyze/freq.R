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
        actionButton(ns("stat"), "Statistics"),
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
        h3("Frequency Table"),
        tableOutput(ns("frequencies")),
        h3("Statistics"),
        h5("Central Tendency"),
        verbatimTextOutput(ns("stat_results")),
        h5("Dispersion"),
        verbatimTextOutput(ns("chart_results")),
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
        header = "Drag the items in any desired bucket",
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
  observeEvent(input$stat, {
    showModal(statsModal(input, output, session))
  })
  
  observeEvent(input$submit, {
    removeModal()
  })
  
  observeEvent(input$chart, {
    showModal(freqChartsModal(input, output, session))
  })
  
  observeEvent(input$submit, {
    removeModal()
  })
  
  # Wait for the user to hit submit --------------------------------------------
  observeEvent(input$ok, {
    
    # Extract variables
    cols <- extractCols(input$rank_list_2, data())
    
    # Calculate frequencies ----------------------------------------------------
    if (input$apa == TRUE) {
      results <- tab1(cols, cum.percent = FALSE)
    } else {
      results <- tab1(cols, cum.percent=TRUE)
    }
    
    output$frequencies <- renderTable({results})
    
    # Calculate chosen statistics ----------------------------------------------
    # Central Tendency
    centen_results <- NULL
    if (!is.null(input$centen)) {
      centen_results <- centraltendency(cols, input$centen)
    }
    
    # Dispersion
    disp_results <- NULL
    if (!is.null(input$disp)) {
      disp_results <- dispersion(cols, input$disp)
    }
    
  
    output$stat_results <- renderPrint({ centen_results })
    
    output$disp_results <- renderPrint({ disp_results })

  })
  
  })
}


