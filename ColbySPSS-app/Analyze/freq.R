library(shiny)
library(sortable)
library(purrr)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------

freqUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))
    ),
    
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
        actionButton(ns("format"), "Format"),
      )
    ), 
    fluidRow (
      column (
        width = 10,
        textOutput(ns("results"))
      )
    )
    
  )
}

freqServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    output$sortable <- renderUI({
      bucket_list(
        header = "Drag the items in any desired bucket",
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "variables",
          labels = vars(),
          input_id = "rank_list_1"),
        add_rank_list(
          text = "Calculate frequencies for:",
          labels = NULL,
          input_id = "rank_list_2"
        ))
    
  })
    observeEvent(input$stat, {
      showModal(statsModal(input, output, session, input$rank_list_2))
    })
  
  # Wait for the user to hit submit
  observeEvent(input$submit, {
    
    # close the pop-up window
    removeModal()
    
    # Central Tendency ---------------------------------------------------------
    centenSelect <- mget(input$centen)
    print("here")
    
    results <- list()
    # use lapply to apply each function selected to each variable chosen
    if (c("Mean") %in% centenSelect) {
      print("got here")
      results.append(lapply(vars_selected, mean))
    }
    if ("Median" %in% centenSelect) {
      results.append(lapply(vars_selected, median))
    }
    if ("Mode" %in% centenSelect) {
    # Have to create own mode function
      results.append(lapply(vars_selected, function(x) {
      uniqx <- unique(x)
      uniqx[which.max(tabulate(match(x, uniqx)))]
    }))
    }
    if ("Sum" %in% centenSelect) {
     results.append(lapply(vars_selected, sum))
    }
    
    output$results <- renderPrint(results)
    
  })
  
  })
}

# Helper Function - find_vars() ------------------------------------------------
# Extracts the variables from the given dataset
find_vars <- function(data) {
  stopifnot(is.data.frame(data))
  names(data)
}

