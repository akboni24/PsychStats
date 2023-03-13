library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(tidyr)
library(lsmeans)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/anova-functions.R")
# User Interface ---------------------------------------------------------------
univariateUI <- function(id) {
  
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
        actionButton(ns("plots"), "Plots"),  
        actionButton(ns("posthoc"), "Post Hoc"),
        actionButton(ns("emmeans"), "EM Means"),
        actionButton(ns("options"), "Options")
      )
    ), 
    fluidRow(
      column(
        width = 10,
        checkboxInput(ns("es"), label = "Estimate effect sizes for overall tests", value=FALSE)
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
        span(textOutput(ns("errors")), style="color:red"),
        textOutput(ns("warning")),
        h3("Descriptive Statistics"),
        verbatimTextOutput(ns("statsresults")),
        h3("ANOVA"),
        verbatimTextOutput(ns("results")),
        h3("Effect Sizes"),
        tableOutput(ns("esResults")),
        h3("Estimated Marginal Means"),
        verbatimTextOutput(ns("emResults")),
        h3("Post Hoc Tests"),
        verbatimTextOutput(ns("phTests")),
        h3("Plots"),
        plotOutput(ns("plotResults"))
      )
    ),
    fluidRow (
      column (
        width = 12,
        checkboxInput(ns("setest"), "Test for simple effects?", value = FALSE),
        selectInput(ns("setestvar"), label = "COMPARE", choices = character(0)),
        selectInput(ns("setestadj"), label = "ADJ", choices = c("Bonferroni", "LSD")),
        actionButton(ns("seOK"), label = "OK"),
        verbatimTextOutput(ns("seResults"))
      )
    )
  )
}

univariateServer <- function(id, data) {
  
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
          text = "Dependent Variable: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Fixed Factor(s): ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
    
    
    # Show plot, post hoc, and options modals if selected ----------------------
    observeEvent(input$plots, {
      showModal(uniPlotsModal(input, output, session, input$rank_list_3))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$posthoc, {
      showModal(uniPostHocModal(input, output, session, input$rank_list_3))
    }) 
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$options, {
      showModal(anovaOptionsModal(input, output, session))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$emmeans, {
      showModal(uniEMModal(input, output, session, input$rank_list_3))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Check that the user selected the right kinds of variables --------------
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      factor <- lapply(input$rank_list_3, check_condition, data(), is.numeric)
      
      # Display warning if chosen factor variable is numeric -------------------
      if (TRUE %in% factor) {
        output$warning <- renderText({factor_warning("Fixed Factors")})
      }
      
      # Stop calculations and print error message if other variable is not numeric
      if (numeric == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
      } else {
      
        # Calculate the ANOVA and display the results in a table -----------------
        col1 <- data() %>% pull(input$rank_list_2)
        col2 <- as.factor(data() %>% pull(input$rank_list_3[1]))
        col3 <- as.factor(data() %>% pull(input$rank_list_3[2]))
        anovaResults <- aov(col1 ~ col2 + col3 + col2:col3, data=data())
        
        output$results <- renderPrint({
          summary(anovaResults)
        })
        
        # Make plots -------------------------------------------------------------
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
      
        observeEvent(input$rank_list_3, {
          updateSelectInput(session, "setestvar", choices = input$rank_list_3)
          # NEED TO FIX IF USER WANTS FIRST OPTION (ALREADY SELECTED)
        })
        
        observeEvent(input$seOK,
         {
           req(input$setest)
           
           aov_lm <- lm(col1 ~ col2 + col3 + col2:col3)
           
           if (is.null(input$setestvar)) {
             lsm <- emmeans(aov_lm, ~ col3 | col2)
           } else {
             lsm <- emmeans(aov_lm, ~ col2 | col3)
           }
           
           
           
           se_results <- test(contrast(lsm, "poly"), joint = TRUE)
           #se_results <- test_simple_effects(data(), not_selected, input$rank_list_2, 
           #sefactor)
           
           
           output$seResults <- renderPrint({
             se_results
           })
         })

      }
      })
      
      
    
  })
  
}