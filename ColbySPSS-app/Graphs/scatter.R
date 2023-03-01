library(shiny)
library(sortable)
library(ggplot2)
library(dplyr)
library(ggplot2)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Graphs/graphs-functions.R")
# User Interface ---------------------------------------------------------------
scatterUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Simple Scatter"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable"))
      ),
      column(
        # Buttons
        width = 2,
        actionButton(ns("titles"), "Titles")
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
        width = 12,
        plotOutput(ns("plotResults"))
      )
    )
  )
}

scatterServer <- function(id, data) {
  
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
          text = "Y Axis: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "X Axis: ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
    
    # Show plot, post hoc, and options modals if selected ----------------------
    observeEvent(input$titles, {
      showModal(scatterTitlesModal(input, output, session))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })

    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Pull the x and y variables from the dataset ----------------------------
      y <- data() %>% pull(input$rank_list_2)
      x <- data() %>% pull(input$rank_list_3)
      
      # Check titles -----------------------------------------------------------
      if(!is.null(input$main)) {
        main <- input$main
      } else {
        main = "Scatterplot"
      }
      
      if(!is.null(input$sub)) {
        sub <- input$sub
      } else {
        sub = ""
      }
      
      title <- main %c% "\n" %c% sub
      
      if(!is.null(input$foot)) {
        footnote <- input$foot
      } else {
        footnote = ""
      }
      
      
      # Render the plot --------------------------------------------------------
      output$plotResults <- renderPlot({
        ggplot(data(), aes(x=x, y=y)) + geom_point() +
          labs(title=title, caption=footnote) +
          theme_classic()
      })
      
      
      
    
  })
    
  })
  
}