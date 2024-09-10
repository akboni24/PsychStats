library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(tidyr)
library(lsmeans)
source("Analyze/anova-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
univariateUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    useShinyjs(),
    titlePanel("Univariate"),
    
    fluidRow (
      column (
        width = 12,
        h5("Note: Use this page for two-way between subjects ANOVAs. To conduct
           a one-way ANOVA, use the 'One Way ANOVA' page instead.")
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
        width = 12,
        span(textOutput(ns("errors")), style="color:red"),
        textOutput(ns("warning")),
        h3("Descriptive Statistics"),
        h5("Note: 'X1' refers to the first level of the first factor you selected."),
        tableOutput(ns("descr")),
        h3("Levene's Test for Homogeneity of Variances"),
        verbatimTextOutput(ns("levene")),
        h3("ANOVA"),
        verbatimTextOutput(ns("results")),
        h3("Effect Sizes"),
        tableOutput(ns("esResults")),
        h3("Estimated Marginal Means"),
        verbatimTextOutput(ns("emResults_overall")),
        verbatimTextOutput(ns("emResults1")),
        verbatimTextOutput(ns("emResults2")),
        verbatimTextOutput(ns("emResults_inter")),
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
      column(
        width=12,
        downloadButton(ns("report"), label = "Generate PDF")
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
  
    observe({ 
      toggleState(id="ok", 
      condition=length(input$rank_list_2)==1&&length(input$rank_list_3>=1)) })
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
        dependent_var <- data() %>% pull(input$rank_list_2)
        between1 <- as.factor(data() %>% pull(input$rank_list_3[1]))
        between2 <- as.factor(data() %>% pull(input$rank_list_3[2]))
        dep_name <- c(input$rank_list_2)
        anova_lm <- aov_ez(colnames(data())[1], dep_name,
                                       between = c(input$rank_list_3),
                                       data=data(),
                                       return=c("aov"))
        anovaResults <- aov_ez(colnames(data())[1], dep_name,
                               between = c(input$rank_list_3[1], input$rank_list_3[2]),
                               data=data(),
                               es="pes")
        
        output$results <- renderPrint({
          anovaResults
        })
        
        # Make plots -------------------------------------------------------------
        if (input$plots == TRUE) {
          if (input$errorBars == TRUE) {
            if (is.null(input$ebOptions)) {
              errorBars <- "mean_ci"
            } else {
              errorBars <- "mean_se"
            }
          } else {
            errorBars <- "none"
          }
          
          
          if (!is.null(input$plotXAxis)) {
            
            if (!is.null(input$plotSepLines)) {
              
            output$plotResults <- renderPlot({
      
              uniMakePlot(data(), as.factor(data() %>% pull(input$plotXAxis)),
                          as.factor(data() %>% pull(input$plotSepLines)), dependent_var,
                          c(input$plotXAxis), dep_name, c(input$plotSepLines), errorBars, input$type)
            })
          }
          }
      
        }
        
        # Calculate chosen statistics --------------------------------------------
        if (!is.null(input$stat)) {
          
          if ("Descriptives" %in% input$stat) {
           
            descriptives <- two_way_anovaDescriptives(data(), input$rank_list_2,
                                                      input$rank_list_3[1], input$rank_list_3[2])
    
           
            output$descr <- renderTable({
              descriptives
            })
            
            
          } else {
            descriptives <- "Not Calulated"
          }
          
          if ("Homogeneity of variance test" %in% input$stat) {
            levene <- leveneTest(lm(dependent_var ~ between1*between2), center=mean)
            output$levene <- renderPrint({
              levene
            })
          } else {
            levene <- "Not Calulated"
          }
          
          if ("Welch Test" %in% input$stat) {
            if (len(input$rank_list_3) == 1) {
              welch <- oneway.test(anova_lm)
            } else {
              welch <- "Not Calculated"
            }
            
          } else {
            welch <- "Not Calulated"
          }
          
          
        } else {
          descriptives <- "Not Calculated"
          levene <- "Not Calulated"
          welch <- "Not Calulated"
        }
        
        # Calculate effect sizes -------------------------------------------------
        if (input$es == TRUE) {
          options(es.use_symbols = TRUE)
          esResults <- etaSquared(anova_lm)
          output$esResults <- renderTable({
            return(esResults)
          })
        } else {
          esResults <- "Not Calculated"
        }
        
        # Conduct Post Hoc Tests -------------------------------------------------
        if (!is.null(input$eva)) {
          if (input$rank_list_3[1] %in% input$postHocVars && input$rank_list_3[2] %in% input$postHocVars) {
            posthoc <- uniPostHocCalc(input$eva, dependent_var, between1, between2, 0.95)
            output$phTests <- renderPrint({
              posthoc
        })
        } else if (input$rank_list_3[1] %in% input$postHocVars) {
          posthoc <- postHocCalc(input$eva, dependent_var, between1, 0.95)
          output$phTests <- renderPrint({
            posthoc
          })
        } else if (input$rank_list_3[2] %in% input$postHocVars) {
          posthoc <- postHocCalc(input$eva, dependent_var, between2, 0.95)
          output$phTests <- renderPrint({
            posthoc
          })
        } else {
          posthoc <- "Not Calculated"
        }
        
        } else {
          posthoc <- "Not Calculated"
        }
        
        # Calculate EM Means ---------------------------------------------------
        if (!is.null(input$EMVars)) {
          if (!is.null(input$ciadj)) {
            ciadj <- 'bonferroni'
          } else {
            ciadj <- "none"
          }
          
          if ("OVERALL" %in% input$EMVars) {
            em1 <- emmeans_descr(data(), input$rank_list_2)
            output$emResults_overall <- renderPrint({
              print("Grand Mean");
              em1
            })
          } else {
            em1 <- c()
          }
          
          if (input$rank_list_3[1] %in% input$EMVars) {
            em_fit1 <- emmeans(anova_lm, input$rank_list_3[1])
            output$emResults1 <- renderPrint({
              print(c(input$rank_list_3[1]));
              print(summary(em_fit1));
              if ("Compare main effects" %in% input$cme) {
                print(pairs(em_fit1, adjust=ciadj));
                print(test(em_fit1, adjust=ciadj, joint=TRUE))
              } else {
                em1_tests <- c()
              }
              
            })
            if ("Compare main effects" %in% input$cme) {
              em1_tests <- c(pairs(em_fit1, adjust=ciadj),
                             test(em_fit1, adjust=ciadj, joint=TRUE))
            } else {
              em1_tests <- c()
            }
            
          } else {
            em_fit1 <- c()
            em1_tests <- c()
          }
          
          if (input$rank_list_3[2] %in% input$EMVars) {
            em_fit2 <- emmeans(anova_lm, input$rank_list_3[2])
            output$emResults2 <- renderPrint({
              print(c(input$rank_list_3[2]));
              print(summary(em_fit2));
              if ("Compare main effects" %in% input$cme) {
                print(pairs(em_fit2, adjust=ciadj));
                print(test(em_fit2, adjust=ciadj, joint=TRUE))
              } else {
                em2_tests <- c()
              }
            })
            
            if ("Compare main effects" %in% input$cme) {
              em2_tests <- c(pairs(em_fit2, adjust=ciadj),
                             test(em_fit2, adjust=ciadj, joint=TRUE))
            } else {
              em2_tests <- c()
            }
            
          } else {
            em_fit2 <- c()
            em2_tests <- c()
          }
          
          interaction <- paste(input$rank_list_3, collapse = "*")
          if (interaction %in% input$EMVars) {
            em_fit3 <- emmeans(anova_lm, input$rank_list_3[1], 
                               by=input$rank_list_3[2])
            em_fit4 <- emmeans(anova_lm, input$rank_list_3[2], 
                               by=input$rank_list_3[1])
            output$emResults_inter <- renderPrint({
              print("Interaction");
              print("Estimates")
              print(summary(em_fit3));
              print(summary(em_fit4));
              if ("Compare simple main effects" %in% input$cme) {
                print("Pairwise Comparisons");
                print(pairs(em_fit3, adjust=ciadj));
                print("Univariate Test");
                print(joint_tests(em_fit3, by=input$rank_list_3[2]));
                print("Pairwise Comparisons");
                print(pairs(em_fit4, adjust=ciadj));
                print("Univariate Test");
                print(joint_tests(em_fit4, by=input$rank_list_3[1]))
              }
            })
            if ("Compare simple main effects" %in% input$cme) {
              em3_tests <- c(pairs(em_fit3, adjust=ciadj), 
                             summary(joint_tests(em_fit3, by=input$rank_list_3[2])))
              em4_tests <- c(pairs(em_fit4, adjust=ciadj), 
                             summary(joint_tests(em_fit4, by=input$rank_list_3[1])))
            } else {
              em3_tests <- c()
              em4_tests <- c()
            }
          } else {
            em_fit3 <- c()
            em_fit4 <- c()
            em3_tests <- c()
            em4_tests <- c()
          }
          
        } else {
          em1 <- c()
          em_fit1 <- c()
          em1_tests <- c()
          em_fit2 <- c()
          em2_tests <- c()
          em_fit3 <- c()
          em_fit4 <- c()
          em3_tests <- c()
          em4_tests <- c()
        }
      
        observeEvent(input$rank_list_3, {
          updateSelectInput(session, "setestvar", choices = input$rank_list_3)
        })
        
        
        observeEvent(input$seOK,
         {
           req(input$setest)
    
           
           if (is.null(input$setestvar)) {
             lsm <- emmeans(anova_lm, input$rank_list_3[1], 
                            by=input$rank_list_3[2])
             by_var <- input$rank_list_3[2]
           } else {
             lsm <- emmeans(anova_lm, input$rank_list_3[2], 
                            by=input$rank_list_3[1])
             by_var <- input$rank_list_3[1]
           }
           
           if (is.null(input$setestadj)) {
             ciadj <- "bonferroni"
           } else {
             ciadj <- "none"
           }
           
           se_results1 <- pairs(lsm, adjust=ciadj)
           se_results2 <- joint_tests(lsm, by=by_var)
           
           
           output$seResults <- renderPrint({
             print("Pairwise Comparisons");
             print(se_results1);
             print("Univariate Test");
             print(se_results2)
           })
           
           # Generate the downloadable pdf report ---------------------------------
           params <- list(descr=descriptives, levene=levene, welch=welch,
                          anova=anovaResults, n2=esResults, em=emmeans,
                          posthoc=posthoc, se=c(se_results1, se_results2))
           
           output$report <- generate_report("univariate_report", params)
         })
        
        # Generate the downloadable pdf report ---------------------------------
        se_results <- "Not Calculated"
        params <- list(descr=descriptives, levene=levene, welch=welch,
                       anova=anovaResults, n2=esResults, em=emmeans,
                       posthoc=posthoc, se=se_results)
      
        output$report <- generate_report("univariate_report", params)

      }
      })
      
      
    
  })
  
}