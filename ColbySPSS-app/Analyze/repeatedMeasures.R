library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(afex)
library(rstatix)
library(effectsize)
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/anova-functions.R")
# User Interface ---------------------------------------------------------------
repeatedMeasuresUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    
    titlePanel("Repeated Measures"),
    fluidRow (
      column (
        width = 12,
        h5("Note: You must have a Participant ID column as the first column
           in your dataset in order to conduct a Repeated Measures ANOVA.")
      )
    ),
    
    fluidRow(
      column (
        width = 6,
        textInput(ns("ws1"), label = "Within-Subject Factor 1 Name: "),
        numericInput(ns("numlvls1"), label = "Number of Levels: ", value=0),
        actionButton(ns("define1"), "Define")
      ),
      column (
        width = 6,
        textInput(ns("ws2"), label = "Within-Subject Factor 2 Name: "),
        numericInput(ns("numlvls2"), label = "Number of Levels: ", value=0),
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
        tableOutput(ns("data")),
        textOutput(ns("datawarning")),
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
    ),
    fluidRow(
      column (
        width = 12,
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
      req(input$define1)
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
          text = "Within-Subjects Variable 1 Levels: ",
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
      if (!is.null(input$rank_list_3)) {
        showModal(uniPlotsModal(input, output, session, c(input$ws1, input$rank_list_3)))
      } else if (input$ws2 != "") {
        showModal(uniPlotsModal(input, output, session, c(input$ws1, input$ws2)))
      } else {
        showModal(uniPlotsModal(input, output, session, c(input$ws1)))
      }
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    observeEvent(input$posthoc, {
      if (input$ws2 != "") {
        showModal(uniPostHocModal(input, output, session, c(input$ws1, input$ws2)))
      } else if (!is.null(input$rank_list_3)) {
        showModal(uniPostHocModal(input, output, session, c(input$ws1, input$rank_list_3)))
      } else {
        showModal(uniPostHocModal(input, output, session, c(input$ws1)))
      }
      
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
      if (input$ws2 != "") {
        showModal(uniEMModal(input, output, session, c(input$ws1, input$ws2)))
      } else if (!is.null(input$rank_list_3)) {
        showModal(uniEMModal(input, output, session, c(input$ws1, input$rank_list_3)))
      } else {
        showModal(uniEMModal(input, output, session, c(input$ws1)))
      }
      
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Check that the user pulled over as many variables to within subjects vars
      # as they said were levels in define
      if (input$numlvls2 > 0) {
        total_lvls = input$numlvls1 * input$numlvls2
      } else {
        total_lvls = input$numlvls1
      }
      
      
      if (total_lvls != length(input$rank_list_2)) {
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
        two_way <- input$ws2 != ""
        
        # MIXED ANOVA ----------------------------------------------------------
        if (mixed == TRUE) {
          
          data_prepared <- one_way_data(data(), input$rank_list_2)
          between_var <- data_prepared %>% pull(input$rank_list_3) %>% as.factor()
          
          # Update choices for the test for simple effects
          updateSelectInput(session, "setestvar", 
                            choices = c(input$ws1, input$rank_list_3))

          results <- aov_ez(colnames(data_prepared)[1], "dependent_var",
                                 between = c(input$rank_list_3),
                                 within = c("within_var"), data=data_prepared,
                                es="pes")

          output$results <- renderPrint({
            summary(results)
          })

          anova_lm <- lm(data_prepared$dependent_var ~ data_prepared$within_var +
                           between_var + data_prepared$within_var:between_var)

          # Calculate chosen statistics ----------------------------------------
          if (!is.null(input$stat)) {

            if ("Descriptives" %in% input$stat) {
              descriptives <- two_way_anovaDescriptives(data_prepared, "dependent_var",
                                                        "within_var", input$rank_list_3)
              output$descr <- renderTable({
                descriptives
              })
            } else {
              descriptives <- "Not Calculated"
            }

            if ("Homogeneity of variance test" %in% input$stat) {
              levene <- leveneTest(anova_lm, center=mean)
                output$levene <- renderPrint({
                  levene
                })
            } else {
              levene <- "Not Calculated"
            }
          }
          # Calculate effect sizes ---------------------------------------------
          if (input$es == TRUE) {
            esResults <- etaSquared(anova_lm)
            output$esResults <- renderTable({
              esResults
            })
          } else {
            esResults <- "Not Calculated"
          }
          # Conduct Post Hoc Tests ---------------------------------------------
          if (!is.null(input$eva)) {
            posthoc <- two_way_posthoc(data_prepared, "dependent_var",
                                       input$postHocVars, input$eva)
            output$phTests <- renderPrint({
              posthoc
            })

          } else {
            posthoc <- "Not Calculated"
          }

          # Calculate EM Means -------------------------------------------------
          if (!is.null(input$EMVars)) {
            emmeans <- emmeans(anova_lm, specs = pairwise ~ within_var:between_var)

            output$emResults <- renderPrint({
              emmeans
            })
          } else {
            emmeans <- "Not Calculated"
          }
          
          # Make plots ---------------------------------------------------------
          if(!is.null(input$errorBars)) {
            if(is.null(input$ebOptions)) {
              errorBars <- "mean_ci"
            } else {
              errorBars <- "mean_se"
            }
          } else {
            errorBars <- "none"
          }
          
          all_vars <- c(input$ws1, input$rank_list_3)
          if (!is.null(input$plotXAxis)) {
            output$plotResults <- renderPlot({
              uniMakePlot(data_prepared, input$plotXAxis, input$plotSepLines, 
                          all_vars, input$type, errorBars)
            })
          }

        # ONE WAY WITHIN SUBJECTS ANOVA ----------------------------------------
        } else if (two_way == FALSE) {
          
          data_prepared <- one_way_data(data(), input$rank_list_2)

          results <- one_way_within(data_prepared, input$es)
          
          p_id <- data_prepared %>% pull(1)

          anova_lm <- lmer(data_prepared$dependent_var ~ data_prepared$within_var 
                         + (1 | p_id))

          output$results <- renderPrint({
            summary(results)
          })

          # Calculate chosen statistics ----------------------------------------
          if (!is.null(input$stat)) {

            if ("Descriptives" %in% input$stat) {
              descriptives <- anovaDescriptives(as.data.frame(data_prepared), "dependent_var",
                                                "within_var")
              output$descr <- renderTable({
                descriptives
              })
            } else {
              descriptives <- "Not Calculated"
            }
          } else {
            descriptives <- "Not Calculated"
          }
          levene <- "Not Calculated"

          if (!is.null(input$eva)) {

            posthoc <- postHocCalc(input$eva, data_prepared$dependent_var,
                                   data_prepared$within_var, 0.95)
            output$phTests <- renderPrint({
              posthoc
            })
          } else {
            posthoc <- "Not Calculated"
          }
          
          emmeans <- emmeans(anova_lm, specs= ~ within_var)
          output$emResults <- renderPrint({
            emmeans
          })
          
          # Calculate effect sizes ---------------------------------------------
          if (input$es == TRUE) {
            esResults <- eta_squared(anova_lm, partial=TRUE)
            output$esResults <- renderTable({
              esResults
            })
          } else {
            esResults <- "Not Calculated"
          }


        # TWO WAY WITHIN SUBJECTS ANOVA ----------------------------------------
        } else {
          
      
          

          data_prepared <- two_way_data(data(), c(input$ws1), input$numlvls1, 
                                        c(input$ws2), input$numlvls2)
          
          output$data <- renderTable({
            data_prepared
          })
          
          # Update selections for test for simple effects
          updateSelectInput(session, "setestvar", choices = c(input$ws1, input$ws2))
          
          results <- aov_ez(colnames(data_prepared)[1], "dependent_var",
                            within = c(input$ws1, input$ws2), data=data_prepared)
          
          factor1 <- data_prepared %>% pull(input$ws1) %>% as.factor()
          factor2 <- data_prepared %>% pull(input$ws2) %>% as.factor()
          p_id <- data_prepared %>% pull(1)
          anova_lm <- lmer(data_prepared$dependent_var ~ factor1 * factor2 +
                          (1 | p_id))

          output$results <- renderPrint({
            summary(results)
          })

          # Calculate chosen statistics ----------------------------------------
          if (!is.null(input$stat)) {

            if ("Descriptives" %in% input$stat) {
              descriptives <- two_way_anovaDescriptives(data_prepared, "dependent_var",
                                                        input$ws1, input$ws2)
              output$descr <- renderTable({
                descriptives
              })
            } else {
              descriptives <- "Not Calculated"
            }
          } else {
            descriptives <- "Not Calculated"
          }
          
          levene <- "Not Calculated"

          # Calculate effect sizes ---------------------------------------------
          if (input$es == TRUE) {
            options(es.use_symbols = TRUE)
            esResults <- eta_squared(anova_lm, partial=TRUE)
            output$esResults <- renderTable({
              esResults
            })

          } else {
            esResults <- "Not Calculated"
          }
          # 
          # Conduct Post Hoc Test ----------------------------------------------
          if (!is.null(input$eva)) {

            posthoc <- two_way_posthoc(data_prepared, "dependent_var",
                                   input$postHocVars, input$eva)
            output$phTests <- renderPrint({
              posthoc
            })

          } else {
            posthoc <- "Not Calculated"
          }

          # Calculate EM Means -------------------------------------------------
          if (!is.null(input$EMVars)) {
            if (!is.null(input$ciadj)) {
              ciadj <- input$ciadj
            } else {
              ciadj <- "none"
            }
            emmeans <- emmeans(anova_lm, specs = pairwise ~ factor1:factor2)
            output$emResults <- renderPrint({
              emmeans
            })
          } else {
            emmeans <- "Not Calculated"
          }
          
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
          
          all_vars <- c(input$ws1, input$ws2)
          if (!is.null(input$plotXAxis)) {
            output$plotResults <- renderPlot({
              uniMakePlot(data_prepared, input$plotXAxis, input$plotSepLines, 
                          all_vars, input$type, errorBars)
            })
          }


        }
        
        se_results <- "Not Calculated"
        
        observeEvent(input$seOK,
         {
           req(input$setest)
           
           if (mixed == TRUE) {
             if (is.null(input$setestvar)) {
               lsm <- emmeans(anova_lm, ~ between_var | data_prepared$within_var)
             } else {
               lsm <- emmeans(anova_lm, ~ data_prepared$within_var | between_var)
             }
           } else {
             if (is.null(input$setestvar)) {
               lsm <- emmeans(anova_lm, ~ factor2 | factor1)
             } else {
               lsm <- emmeans(anova_lm, ~ factor1 | factor2)
             }
           }
           
           
           se_results <- test(contrast(lsm, "poly"), joint = TRUE)
           
          
           output$seResults <- renderPrint({
             se_results
           })


        params <- list(descr=descriptives, levene=levene,
                       anova=results, n2=esResults, em=emmeans,
                       posthoc=posthoc, se=se_results)

        output$report <- generate_report("repeated_measures_report", params)

        })
        
        params <- list(descr=descriptives, levene=levene,
                       anova=results, n2=esResults, em=emmeans,
                       posthoc=posthoc, se=se_results)
        
        output$report <- generate_report("repeated_measures_report", params)
      }
    })
    
  })
  
}