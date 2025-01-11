library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
library(tidyr)
library(lsmeans)
source("Analyze/anova-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
twoWayWithinUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    useShinyjs(),
    titlePanel("Two Way Within Subjects ANOVA"),
    
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
        tableOutput(ns("data")),
        textOutput(ns("datawarning")),
        h3("Descriptive Statistics"),
        h5("Note: 'X0' refers to the first level of the first factor you selected."),
        tableOutput(ns("descr")),
        h3("ANOVA"),
        verbatimTextOutput(ns("results")),
        h3("Effect Sizes"),
        verbatimTextOutput(ns("esResults")),
        h3("Estimated Marginal Means"),
        verbatimTextOutput(ns("emResults_overall")),
        verbatimTextOutput(ns("emResults1")),
        verbatimTextOutput(ns("emResults2")),
        verbatimTextOutput(ns("emResults_inter")),
        h3("Plots"),
        plotOutput(ns("plotResults"))
      )
    ),
    fluidRow (
      column (
        width = 12,
        checkboxInput(ns("setest"), "Test for simple effects?", value = FALSE),
        selectInput(ns("setestvar"), label = "COMPARE", choices = character(0)),
        selectInput(ns("setestadj"), label = "ADJ", choices = list("Bonferroni"=1, "LSD"=2), selected=1),
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

twoWayWithinServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    # Display the drag and drop buckets ----------------------------------------
    output$sortable <- renderUI({
      req(input$ws1)
      req(input$ws2)
      req(input$numlvls1)
      req(input$numlvls2)
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
          text = "Within-Subjects Variable Levels: ",
          labels = NULL,
          input_id = ns("rank_list_2")
        ))
      
    })
  
    observe({ 
      toggleState(id="ok", 
      condition=length(input$rank_list_2)>=1) })
    # Show plot, post hoc, and options modals if selected ----------------------
    observeEvent(input$plots, {
      showModal(uniPlotsModal(input, output, session, c(input$ws1, input$ws2)))
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
      showModal(uniEMModal(input, output, session, c(input$ws1, input$ws2)))
    })
    
    observeEvent(input$continue, {
      removeModal()
    })
    
    
    # Wait for the user to hit submit ------------------------------------------
    observeEvent(input$ok, {
      
      # Check that the user pulled over as many variables to within subjects vars
      # as they said were levels in define
      total_lvls = input$numlvls1 * input$numlvls2
      
      
      
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
        
      
        check <- data_check(input$rank_list_2, c(input$ws1), input$numlvls1,
                            c(input$ws2), input$numlvls2)
        
        output$data <- renderTable({
          check
        })
        
        output$datawarning <- renderText({"Make sure that the above mapping
                                between the levels of both factors and the columns
                                of your data is correct before continuing. If it
                                is not, rearrange the columns in the above box
                                and hit ok again."})
        
        
        data_prepared <- two_way_data(data(), input$rank_list_2, c(input$ws1),
                                      input$numlvls1, c(input$ws2), input$numlvls2)
        
        
        # Update selections for test for simple effects
        updateSelectInput(session, "setestvar", choices = c(input$ws1, input$ws2))
        
        results <- aov_ez(colnames(data_prepared)[1], "dependent_var",
                          within = c(input$ws1, input$ws2), data=data_prepared)
        
        factor1 <- data_prepared %>% pull(input$ws1) %>% as.factor()
        factor2 <- data_prepared %>% pull(input$ws2) %>% as.factor()
        p_id <- data_prepared %>% pull(1)
        anova_lm <- aov_ez(colnames(data_prepared)[1], "dependent_var",
                           within = c(input$ws1, input$ws2), data=data_prepared,
                           return=c("aov"))
        
        output$results <- renderPrint({
          summary(results)
        })
        
        # Calculate chosen statistics ----------------------------------------
        if (!is.null(input$stat)) {
          
          descriptives <- "Not Calculated"
          if ("Descriptives" %in% input$stat) {
            descriptives <- two_way_anovaDescriptives(data_prepared, "dependent_var",
                                                      c(input$ws1), c(input$ws2))
            output$descr <- renderTable({
              descriptives
            })
          }
        }
        
        
        # Calculate effect sizes ---------------------------------------------
        if (input$es == TRUE) {
          esResults <- eta_squared(anova_lm, partial=TRUE, ci=input$confint)
          output$esResults <- renderPrint({
            esResults
          })
        } else {
          esResults <- "Not Calculated"
        }
      
        
        # Calculate EM Means -------------------------------------------------
        if (!is.null(input$EMVars)) {
          if (input$ciadj == "Bonferroni") {
            ciadj <- 'bonferroni'
          } else {
            ciadj <- "none"
          }
          
          if ("OVERALL" %in% input$EMVars) {
            em1 <- emmeans_descr(data_prepared, "dependent_var")
            output$emResults_overall <- renderPrint({
              print("Grand Mean");
              em1
            })
          } else {
            em1 <- c()
          }
          
        if (input$ws1 %in% input$EMVars) {
          em_fit1 <- emmeans(anova_lm, c(input$ws1))
          output$emResults1 <- renderPrint({
            print(c(input$ws1));
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
        
        if (input$ws2 %in% input$EMVars) {
          em_fit2 <- emmeans(anova_lm, input$ws2)
          # emResults1 <- pairs(em_fit2, adjust=ciadj)
          output$emResults2 <- renderPrint({
            print(c(input$ws2));
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
        
        interaction <- paste(c(input$ws1, input$ws2), collapse = "*")
        if (interaction %in% input$EMVars) {
          em_fit3 <- emmeans(anova_lm, c(input$ws1), by=c(input$ws2), 
                             model="multivariate")
          em_fit4 <- emmeans(anova_lm, c(input$ws2), by=c(input$ws1))
          output$emResults_inter <- renderPrint({
            print("Interaction");
            print("Estimates")
            print(summary(em_fit3));
            print(summary(em_fit4));
            if ("Compare simple main effects" %in% input$cme) {
              print("Pairwise Comparisons");
              print(pairs(em_fit3, adjust=ciadj));
              print("Multivariate Tests - Pillai's Trace Test Statistic");
              print(joint_tests(em_fit3, by=input$ws2));
              print("Pairwise Comparisons");
              print(pairs(em_fit4, adjust=ciadj));
              print("Univariate Test");
              print(joint_tests(em_fit4, by=c(input$ws1)))
            }
          })
          if ("Compare simple main effects" %in% input$cme) {
            em3_tests <- c(pairs(em_fit3, adjust=ciadj), 
                           summary(joint_tests(em_fit3, by=input$rank_list_3)))
            em4_tests <- c(pairs(em_fit4, adjust=ciadj), 
                           summary(joint_tests(em_fit4, by=c(input$ws1))))
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
          plot <- uniMakePlot(data_prepared, input$plotXAxis, input$plotSepLines, 
                              "dependent_var", input$type, errorBars)
          output$plotResults <- renderPlot({
            plot
          })
        } else {
          plot <- c()
        }
        
        
        observeEvent(input$seOK,
         {
           req(input$setest)
    
           if (input$setestvar == input$rank_list_3[1]) {
             by_var <- input$rank_list_3[2]
           } else {
             by_var <- input$rank_list_3[1]
           }
           
           if (input$setestadj == 1) {
             ciadj <- "bonferroni"
           } else {
             ciadj <- "none"
           }
        
           lsm <- emmeans(anova_lm, input$setestvar, by=by_var, adjust=ciadj)
           
           se_results1 <- pairs(lsm, adjust=ciadj)
           se_results2 <- joint_tests(lsm, by=by_var)
           
           output$seResults <- renderPrint({
             print("Pairwise Comparisons");
             print(se_results1);
             print("Univariate Test");
             print(se_results2)
           })
         })
           
        # Generate the downloadable pdf report ---------------------------------
        se_results <- "Not Calculated"
        # params <- list(descr=descriptives, levene=levene, welch=welch,
        #                anova=anovaResults, n2=esResults, em=emmeans,
        #                posthoc=posthoc, se=se_results)
        # 
        # output$report <- generate_report("univariate_report", params)

        }

  })
    
  })
  
}