library(shiny)
library(sortable)
library("ggvis")
library(dplyr)
source("Graphs/graphs-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
scatterUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    useShinyjs(),
    titlePanel("Simple Scatter"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable")),
        checkboxInput(ns("reg_line"), label="Add regression line?", value=FALSE)
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
        width = 10,
        plotOutput(ns("plotResults")),
        downloadButton(ns("report"), label = "Generate PDF")
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
    
    observe({ 
      toggleState(id="ok", 
      condition=length(input$rank_list_2)==1&&length(input$rank_list_3==1)) })
    
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
      if (is.null(input$main)) {
        main <- "Scatterplot"
      } else {
        main <- input$main
      }

      
      # Render the plot --------------------------------------------------------
       plot <- ggplot(data(), aes(x=x, y=y)) +
                    geom_point() +
                    labs(title=main, x=input$rank_list_3, y=input$rank_list_2)
      
      if (!is.null(input$sub)) {
        plot <- plot + labs(subtitle=input$sub)
      }
      
      if (!is.null(input$foot)) {
        plot <- plot + labs(caption=input$foot)
      }
      
      if (input$reg_line == TRUE) {
        plot <- plot + geom_smooth(method=lm)
      }

      output$plotResults <- renderPlot(plot)
    
      # Generate the downloadable pdf report -----------------------------------
      params <- list(plot = plot)
      
      output$report <- generate_report("scatter_report", params)
  })
    
  })
  
}