library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/anova-functions.R")
# User Interface ---------------------------------------------------------------
oneWayAnovaUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("One Way ANOVA"),
    
    # Creates two drag and drop buckets
    fluidRow(
      column (
        width = 10,
        uiOutput(ns("sortable"))
      ),
      column(
        # Buttons
        width = 2,
        # actionButton(ns("contrasts"), "Contrasts"),   Didn't do contrasts in PS215, so I wasn't going to include
        actionButton(ns("posthoc"), "Post Hoc"),
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
        width = 10,
        span(textOutput(ns("errors")), style="color:red"),
        textOutput(ns("warning")),
        h3("Descriptive Statistics"),
        tableOutput(ns("descr")),
        h3("Levene's Test for Homogeneity of Variances"),
        verbatimTextOutput(ns("levene")),
        h3("Welch Test"),
        verbatimTextOutput(ns("welch")),
        h3("ANOVA"),
        verbatimTextOutput(ns("results")),
        h3("Effect Sizes"),
        tableOutput(ns("esResults")),
        h3("Post Hoc Tests"),
        verbatimTextOutput(ns("phTests")),
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
  )
}

oneWayAnovaServer <- function(id, data) {
  
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
          text = "Dependent List: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ), 
        add_rank_list(
          text = "Factor: ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })

    # Show post hoc and options modals if selected -----------------------------
    observeEvent(input$posthoc, {
      showModal(anovaPostHocModal(input, output, session))
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
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Check that the user selected the right kinds of variables --------------
      numeric <- check_condition(input$rank_list_2, data(), is.numeric)
      factor <- check_condition(input$rank_list_3, data(), is.numeric)
      
      # Display warning if chosen factor variable is numeric -------------------
      if (factor == TRUE) {
        output$warning <- renderText({factor_warning(input$rank_list_3)})
      }
      
      # Stop calculations and print error message if other variable is not numeric
      if (numeric == FALSE) {
        output$errors <- renderText({errorText("categorical", "numeric")})
      } else {
      
      
        # Calculate the ANOVA and display the results in a table -----------------
        col1 <- data() %>% pull(input$rank_list_2)
        col2 <- as.factor(data() %>% pull(input$rank_list_3))
        anovaResults <- aov_ez(colnames(data())[1], input$rank_list_2, data=data(),
                               between=c(input$rank_list_3))
        
        output$results <- renderPrint({
          summary(anovaResults)
        })
        
        # Calculate chosen statistics ------------------------------------------
        
        if (!is.null(input$stat)) {
          
          if ("Descriptives" %in% input$stat) {
            
            descriptives <- anovaDescriptives(data(), input$rank_list_2, 
                                              input$rank_list_3)
            output$descr <- renderTable({
              descriptives
            })
          } else {
            descriptives <- c()
          }
          
          if ("Homogeneity of variance test" %in% input$stat) {
            
            levene <- leveneTest(col1 ~ col2, center=mean)
            output$levene <- renderPrint({
              levene
            })
          } else {
            levene <- c()
          }
          
          if ("Welch test" %in% input$stat) {
            
            welch <- oneway.test(col1 ~ col2, data=data())
            output$welch <- renderPrint({
              welch
            })
          } else {
            welch <- c()
          }
      
        } else {
          descriptives <- c()
          levene <- c()
          welch <- c()
        }
        
        # Calculate effect sizes -----------------------------------------------
        esResults <- etaSquared(lm(col1 ~ col2))
        if (input$es == TRUE) {
          options(es.use_symbols = TRUE)
          output$esResults <- renderTable({
            esResults
          })
        }
        
        # Calculate post hoc tests ---------------------------------------------
        if(is.null(input$confint)) {
          confint = 0.95
        } else {
          confint = input$confint
        }
        
        posthoc <- postHocCalc(input$eva, col1, col2, confint)
        if (!is.null(input$eva)) {
          output$phTests <- renderPrint({
            posthoc
          })
        }
        
        # Generate the downloadable pdf report ---------------------------------
        em_over <- emmeans_descr(data(), input$rank_list_2, levels=FALSE)
        emmeans <- emmeans(lm(col1 ~ col2), specs= ~ col2)
        params <- list(descr = descriptives, levene=levene, welch=welch,
                       anova=anovaResults, n2=esResults, em_o=em_over, em=emmeans,
                       posthoc=posthoc)
        
        output$report <- generate_report("one_way_anova_report", params)
      }
      
    })
    
  })
  
}