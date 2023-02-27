library(shiny)
library(sortable)
library(ggplot2)
library(dplyr)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Graphs/graphs-functions.R")
# User Interface ---------------------------------------------------------------
scatterUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Univariate"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable"))
      ),
      column(
        # Buttons
        width = 2,
        actionButton(ns("titles"), "Titles"),  
        actionButton(ns("options"), "Options")
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
      showModal(scatterTitlesModal(input, output, session, input$rank_list_3))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$options, {
      showModal(scatterOptionsModal(input, output, session, input$rank_list_3))
    }) 
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Pull the x and y variables from the dataset ----------------------------
      y <- data() %>% pull(input$rank_list_2)
      x <- data() %>% pull(input$rank_list_3)
      
      # Check options ----------------------------------------------------------
      if(!is.null(input$titles)) {
        main <- input$main
      } else {
        main = "Scatterplot"
      }
      
      if(!is.null(input$errorBars)) {
        if(is.null(input$ebOptions)) {
          errorBars <- "mean_ci"
        } else {
          errorBars <- "mean_se"
        }
      } else {
        errorBars <- "none"
      }
      if (!is.null(input$plotXAxis)) {
        output$plotResults <- renderPlot({
          uniMakePlot(data(), input$plotXAxis, input$plotSepLines, input$rank_list_2, input$type, errorBars)
        })
      }
      
      # Calculate chosen statistics --------------------------------------------
      if (!is.null(input$stat)) {
        output$statsresults <- renderPrint({
          anovaOptionsCalc(input$stat, col1 ~ col2 * col3, col1, col2, col3)
        })
      }
      
      # Calculate effect sizes -------------------------------------------------
      esResults <- list()
      if (input$es == TRUE) {
        options(es.use_symbols = TRUE)
        output$esResults <- renderTable({
          return(etaSquared(lm(col1 ~ col2 + col3 + col2:col3)))
        })
      }
      
      # Conduct Post Hoc Tests -------------------------------------------------
      if (!is.null(input$eva)) {
        if (input$rank_list_3[1] %in% input$postHocVars && input$rank_list_3[2] %in% input$postHocVars) {
          output$phTests <- renderPrint({
            uniPostHocCalc(input$eva, col1, col2, col3)
          })
        } else if (input$rank_list_3[1] %in% input$postHocVars) {
          output$phTests <- renderPrint({
            postHocCalc(input$eva, col1, col2, 0.95)
          })
        } else if (input$rank_list_3[2] %in% input$postHocVars) {
          output$phTests <- renderPrint({
            postHocCalc(input$eva, col1, col3, 0.95)
          })
        }
        
      }
      
      # Calculate EM Means -----------------------------------------------------
      if (!is.null(input$EMVars)) {
        output$emResults <- renderPrint({
          uniEMCalc(input$EMVars, input$ciadj, lm(col1 ~ col2 + col3 + col2:col3), col2, col3)
        })
      }
    })
    
    observeEvent(input$rank_list_3, {
      updateSelectInput(session, "setestvar", choices = input$rank_list_3)
      # NEED TO FIX IF USER WANTS FIRST OPTION (ALREADY SELECTED)
    })
    
    observeEvent(input$seOK,
                 {
                   req(input$setest)
                   req(input$setestvar)
                   req(input$setestadj)
                   
                   for (var in input$rank_list_3) {
                     if (var != input$setestvar) {
                       not_selected = var
                     }
                   }
                   
                   
                   se_results <- test_simple_effects(data(), not_selected, input$rank_list_2, input$setestvar)
                   
                   
                   output$seResults <- renderPrint({
                     se_results
                   })
                 })
    
  })
  
}