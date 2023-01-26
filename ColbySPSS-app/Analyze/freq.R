library(shiny)
library(sortable)
library(purrr)
library(shinyFeedback)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------

freqUI <- function(id) {

  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    
    shinyFeedback::useShinyFeedback(),
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
        actionButton(ns("chart"), "Charts"),
        actionButton(ns("format"), "Format")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        # Should hide the OK button until the user has moved at least one variable....
        actionButton(ns("ok"), "OK"),
        actionButton(ns("cancel"), "Cancel")
      )
    ),
    fluidRow (
      column (
        width = 10,
        textOutput(ns("frequencies")),
        textOutput(ns("stat_results")),
        textOutput(ns("chart_results")),
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
          text = "variables",
          labels = vars(),
          input_id = ns("rank_list_1")),
        add_rank_list(
          text = "Calculate frequencies for:",
          labels = NULL,
          input_id = ns("rank_list_2")
        ))
    
  })
    
    observeEvent(input$rank_list_2, {
      factor <- checkFactor(input$rank_list_2, data())
      shinyFeedback::feedbackWarning("rank_list_2", !factor, text = "Please select a categorical variable")
    })

    
    observeEvent(input$stat, {
      showModal(statsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
  
  # Wait for the user to hit submit
  observeEvent(input$ok, {
    
    # close the pop-up window
    removeModal()
    
    cols <- extractCols(input$rank_list_2, data())
    
    # Central Tendency ---------------------------------------------------------
    centen_results <- NULL
    if (!is.null(input$centen)) {
      centen_results <- centraltendency(cols, input$centen)
    }
    
    # Dispersion ---------------------------------------------------------------
    disp_result <- NULL
    if (!is.null(input$disp)) {
      disp_results <- dispersion(cols, input$disp)
    }
    
    # Come back to this, make the output an R Markdown file
    # Should store results as a dictionary of the function/variable and the result
    output$stat_results <- renderText({paste0("Central Tendency", centen_results,
                                                "Dispersion", disp_results)})

  })
  
  })
}


