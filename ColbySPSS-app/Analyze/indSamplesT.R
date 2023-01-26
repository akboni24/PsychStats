library(shiny)
library(sortable)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/analyze-functions.R")
# User Interface ---------------------------------------------------------------
indSamplesTUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Independent Samples T Test"),
    
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
        h3("Two-Sample Statistics"),
        tableOutput(ns("descr")),
        h3("Two-Sample Test"),
        h5("Equal Variances Assumed"),
        verbatimTextOutput(ns("results")),
        h5("Equal Variances Not Assumed"),
        verbatimTextOutput(ns("results2")),
        h5("Effect Sizes"),
        verbatimTextOutput(ns("esResults"))
      )
    )
  )
}

indSamplesTServer <- function(id, data) {
  
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
          text = "Variables",
          labels = vars(),
          input_id = ns("rank_list_1")),
        add_rank_list(
          text = "Test Variable(s):",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Grouping Variable(s):",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
   
    
    observeEvent(input$rank_list_2, {
      num <- checkNumeric(input$rank_list_2, data())
      shinyFeedback::feedbackWarning("rank_list_2", !num, text = "Please select a numeric variable")
    })
    
    observeEvent(input$rank_list_3, {
      factor <- checkFactor(input$rank_list_3, data())
      shinyFeedback::feedbackWarning("rank_list_3", !factor, text = "Please select a categorical variable")

    })
    
    observeEvent(input$options, {
      showModal(ttestOptionsModal(input, output, session))
    })
    
    observeEvent(input$submit, {
      removeModal()
    })
    
    # Wait for the user to hit submit
    observeEvent(input$ok, {
      
      if(is.null(input$confint)) {
        confint = 0.95
      } else {
        confint = input$confint
      }
      
      col <- data() %>% pull(input$rank_list_2)
      grouping <- data() %>% pull(input$rank_list_3)
      
      output$descr <- renderTable({
        indttestStats(col, grouping)
      })
      
      if(input$es == TRUE) {
        # Check this
        output$esResults <- renderPrint({cohensD(col ~ grouping)})
      }
      
      # Warning if there are more than two groups
      if (nlevels(grouping) > 2) {
        output$results <- renderText({paste("Grouping variable has more than 2 groups. Please select a different variable or conduct a One Way ANOVA.")})
      } else{
        results <- t.test(col ~ grouping, alternative="two.sided", data = data(), na.rm = TRUE, conf.level = confint, var.equal = TRUE)
        results2 <- t.test(col ~ grouping, alternative="two.sided", data = data(), na.rm = TRUE, conf.level = confint, var.equal = FALSE)
        # Come back to this, make the output an R Markdown file
        # Should store results as a dictionary of the function/variable and the result
        output$results <- renderPrint({results})
        output$results2 <- renderPrint({results2})
        
      }

    })
    
  })
  
}