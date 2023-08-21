library(shiny)
library(sortable)
library("ggvis")
library(dplyr)
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Graphs/graphs-functions.R")
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
        width = 10,
        ggvisOutput(ns("plotResults")),
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
      
      if (is.null(input$sub)) {
        sub <- NULL
        include1 <- FALSE
      } else {
        sub <- input$sub
        include1 <- TRUE
      }

      if (is.null(input$foot)) {
        footnote <- NULL
        include2 <- FALSE 
      } else {
        footnote <- input$foot
        include2 <- TRUE
      }
     
      
      
      # Render the plot --------------------------------------------------------
       plot <- data() %>% 
                    ggvis(~x, ~y) %>% 
                    layer_points() %>%
                    add_axis("x", title = input$rank_list_3) %>%
                    add_axis("y", title = input$rank_list_2) %>%
                    add_title(title=main) %>%
                    add_title(title=sub, subtitle=include1) %>%
                    add_title(title=footnote, footnote=include2)
                      

      plot %>% bind_shiny("scat-plotResults")
    
      # Generate the downloadable pdf report -----------------------------------
      params <- list(plot = plot)
      
      output$report <- generate_report("scatter_report", params)
  })
    
  })
  
}