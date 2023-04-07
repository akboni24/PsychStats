library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(afex)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/anova-functions.R")
# User Interface ---------------------------------------------------------------
repeatedMeasuresUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Repeated Measures"),
    
    fluidRow(
      column (
        width = 12,
        textInput(ns("ws"), label = "Within-Subject Factor Name: "),
        numericInput(ns("numlvls"), label = "Number of Levels: ", value=0),
        actionButton(ns("define"), "Define")
      )
    ),
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
        width = 10,
        span(textOutput(ns("errors")), style="color:red"),
        h3("Descriptive Statistics"),
        tableOutput(ns("descr")),
        h3("Levene's Test for Homogeneity of Variances"),
        verbatimTextOutput(ns("levene")),
        h3("ANOVA"),
        verbatimTextOutput(ns("results")),
        h3("Effect Sizes"),
        tableOutput(ns("esResults")),
        h3("Estimated Marginal Means"),
        verbatimTextOutput(ns("emResults")),
        h3("Post Hoc Tests"),
        verbatimTextOutput(ns("phTests")),
        h3("Plots"),
        plotOutput(ns("plotResults")),
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
  )
}

repeatedMeasuresServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    # Display the drag and drop buckets ----------------------------------------
    output$sortable <- renderUI({
      # JUST IMPLEMENT A CHECK THAT WARNS THEM IF THEY HAVENT MAPPED A VARIABLE TO 
      # EACH FACTOR LEVEL YET
      req(input$define)
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
          text = "Within-Subjects Variable Levels: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ),
        add_rank_list(
          text = "Between-Subjects Factor(s): ",
          labels = NULL,
          input_id = ns("rank_list_3")
        ))
      
    })
    
  
    
    # Show plot, post hoc, and options modals if selected ----------------------
    
    observeEvent(input$plots, {
      showModal(uniPlotsModal(input, output, session, c(input$ws, input$rank_list_3)))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$posthoc, {
      showModal(uniPostHocModal(input, output, session, c(input$ws, input$rank_list_3)))
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
      showModal(uniEMModal(input, output, session, c(input$ws, input$rank_list_3)))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Check that the user pulled over as many variables to within subjects vars
      # as they said were levels in define
      if (input$numlvls != length(input$rank_list_2)) {
        output$errors <- renderText({"You did not drag over the same number of 
                                      variables to the Within Subject Variable
                                      Levels box as the number of levels you 
                                      defined at the top of this page. Please
                                      make sure the number of levels you define
                                      and the number of variables in the Within 
                                      Subject Variable Levels box match before
                                      proceeding."})
      } else {
      
        # Calculate the ANOVA and display the results in a table ---------------
        mixed <- length(input$rank_list_3) > 0
        two_way <- length(input$rank_list_2) > 1
        # Preparing the data
        if (two_way == FALSE) {
          data_prepared <- data() %>%
            gather(key="within_var", value="dependent_var", input$rank_list_2) %>%
            convert_as_factor(within_var)
          
          within_var <- data_prepared %>% pull("within_var")
        } else {
          data_prepared <- data() %>%
            gather(key="within_var1", value="dependent_var1", input$rank_list_2[1]) %>%
            convert_as_factor(within_var1)
          
          data_prepared <- data_prepared() %>%
            gather(key="within_var2", value="dependent_var2", input$rank_list_2[2]) %>%
            convert_as_factor(within_var2)
          
          within_var <- data_prepared %>% pull("within_var1")
          
          within_var2 <- data_prepared %>% pull("within_var2")
        }
        
       
        if (mixed == FALSE && two_way == FALSE) {
          anovaResults <- aov_ez(colnames(data_prepared)[1], "dependent_var", 
                                 within = c("within_var"), data=data_prepared)
          all_vars <- within_var
        } else {
          # Pulling between subjects var
          between_var <- as.factor(data_prepared %>% pull(input$rank_list_3))
          anovaResults <- aov_ez(colnames(data_prepared)[1], "dependent_var", 
                                 between = c(input$rank_list_3),
                                 within = c("within_var"), data=data_prepared)
          all_vars <- append(within_var, between_var)
        }
        
        
        output$results <- renderPrint({
          return(anovaResults)
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
            uniMakePlot(data_prepared, input$plotXAxis, input$plotSepLines, all_vars, input$type, errorBars)
          })
        }
        
        # Calculate chosen statistics --------------------------------------------
        if (!is.null(input$stat)) {
          
          if ("Descriptives" %in% input$stat) {
            if (mixed == FALSE) {
              descriptives <- anovaDescriptives(data_prepared(), "dependent var",
                                                "within_var")
            } else {
              descriptives <- two_way_anovaDescriptives(data_prepared(), "dependent var",
                                                "within_var", input$rank_list_3)
            }
            
            output$descr <- renderTable({
              descriptives
            })
          } else {
            descriptives <- "Not Calulated"
          }
          
          if ("Homogeneity of variance test" %in% input$stat) {
            if (mixed == TRUE) {
              levene <- leveneTest(anova_lm, center=mean)
              output$levene <- renderPrint({
                levene
            })
          } else {
            levene <- "Not Calulated"
          } 
          
        } else {
            levene <- "Not Calulated"
        }
          
        } else {
          descriptives <- "Not Calculated"
          levene <- "Not Calulated"
          welch <- "Not Calulated"
        }
          
        
        # Calculate effect sizes -------------------------------------------------
        esResults <- list()
        if (input$es == TRUE) {
          options(es.use_symbols = TRUE)
          if (is.null(input$rank_list_3)) {
          output$esResults <- renderTable({
            return(etaSquared(lm(data_prepared$dependent_var ~ data_prepared$within_var)))
          })
          } else {
            output$esResults <- renderTable({
              return(etaSquared(lm(data_prepared$dependent_var ~ data_prepared$within_var +
                                     between_var + data_prepared$within_var:between_var)))
            })
          }
        }
        
        # Conduct Post Hoc Tests -------------------------------------------------
        if (!is.null(input$eva)) {
          
          if (is.null(input$rank_list_3)) {
            output$phTests <- renderPrint({
              postHocCalc(input$eva, data_prepared$dependent_var, data_prepared$within_var,
                          0.95)
            })
          } else {
            output$phTests <- renderPrint({
              test_simple_effects(data_prepared, names(data_prepared$dependent_var), 
                                  names(data_prepared$within_var), 0.95)
            })
          }
         
        }
        
        # Calculate EM Means -----------------------------------------------------
        if (!is.null(input$EMVars)) {
          if (is.null(input$rank_list_3)) {
            output$emResults <- renderPrint({
              uniEMCalc(input$EMVars, input$ciadj, lm(data_prepared$dependent_var ~ data_prepared$within_var), 
                        data_prepared$within_var)
            })
          } else {
            output$emResults <- renderPrint({
              uniEMCalc(input$EMVars, input$ciadj, 
                        lm(data_prepared$dependent_var ~ data_prepared$within_var +
                        between_var + data_prepared$within_var:between_var), 
                        data_prepared$within_var, between_var)
            })
          }
          
        }
        params <- list(descr=descriptives, levene=levene, welch=welch,
                       anova=anovaResults, n2=esResults, em=emmeans,
                       posthoc=posthoc, se=se_results)
        
        output$report <- generate_report("univariate_report", params)
        
      }
    })
    
  })
  
}