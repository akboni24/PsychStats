library(shiny)
library(sortable)
library(effectsize)
library(DescTools)
source("Analyze/anova-functions.R", local=TRUE)
# User Interface ---------------------------------------------------------------
oneWayWithinUI <- function(id) {
  
  ns <- NS(id)
  tagList (
    tags$head(
      tags$style(HTML(".bucket-list-container {min-height: 350px;}"))),
    useShinyjs(),
    titlePanel("One Way Within-Subjects ANOVA"),
    
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
        textInput(ns("ws1"), label = "Within-Subject Factor Name: "),
        numericInput(ns("numlvls1"), label = "Number of Levels: ", value=0),
        actionButton(ns("define1"), "Define")
      )),
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
        h3("ANOVA"),
        span(verbatimTextOutput(ns("results"))),
        h3("Effect Sizes"),
        span(verbatimTextOutput(ns("esResults"))),
        h3("Estimated Marginal Means"),
        span(verbatimTextOutput(ns("emResults_overall"))),
        span(verbatimTextOutput(ns("emResults1"))),
        h3("Post Hoc Tests"),
        span(verbatimTextOutput(ns("phTests"))),
        downloadButton(ns("report"), label = "Generate PDF")
      )
    )
  )
}

oneWayWithinServer <- function(id, data) {
  
  stopifnot(is.reactive(data))
  vars <- NULL
  
  moduleServer(id, function(input, output, session) {
    
    vars <- reactive({find_vars(data())})
    
    # Display the drag and drop buckets ----------------------------------------
    output$sortable <- renderUI({
      req(input$ws1)
      req(input$numlvls1)
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
    
    observe({ toggleState(id="ok", condition=length(input$rank_list_2)>=1) })

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
      
      # Clear all previous outputs ---------------------------------------------
      output$results <- renderPrint({c()})
      output$descr <- renderTable({c()})
      output$esResults <- renderTable({c()})
      output$phTests <- renderPrint({c()})
      
      # Check that the user pulled over as many variables to within subjects vars
      # as they said were levels in define
      if (input$numlvls1 != length(input$rank_list_2)) {
        output$errors <- renderText({"You did not drag over the same number of
                                      variables to the Within Subject Variable
                                      Levels box as the number of levels you
                                      defined at the top of this page. Please
                                      make sure the number of levels you define
                                      and the number of variables in the Within
                                      Subject Variable Levels box match before
                                      proceeding."})
        
      } else {
      
        vars <- c(colnames(data())[1], c(input$rank_list_2))
        data_prepared <- one_way_data(data(), c(input$rank_list_2), c(input$ws1),
                                      vars)
        within_var <- data_prepared %>% pull(input$ws1) %>% as.factor()
        
        anovaResults <- one_way_within(data_prepared, input$es, input$ws1)
        
        p_id <- data_prepared %>% pull(1)
        
        anova_lm <- lmer(data_prepared$dependent_var ~ within_var + (1 | p_id))
        
        output$results <- renderPrint({
          summary(anovaResults)
        })
        
        # Calculate chosen statistics ----------------------------------------
        if (!is.null(input$stat)) {

          if ("Descriptives" %in% input$stat) {
            descriptives <- anovaDescriptives(as.data.frame(data_prepared),
                                              "dependent_var", input$ws1)
            output$descr <- renderTable({
              descriptives
            })
          } else {
            descriptives <- "Not Calculated"
          }
        } else {
          descriptives <- "Not Calculated"
        }

        # Handle EM Means ----------------------------------------------------
        em_over <- emmeans_descr(as.data.frame(data_prepared),
                                 "dependent_var")
        em_fit1 <- emmeans(anova_lm, specs= ~ within_var, level=input$confint)

        output$emResults_overall <- renderPrint({
          print("Grand Mean");
          em_over
        })

        output$emResults1 <- renderPrint({
          print(c(input$ws1));
          print(summary(em_fit1, level=input$confint));
        })

        # Calculate effect sizes ---------------------------------------------
        if (input$es == TRUE) {
          esResults <- eta_squared(anova_lm, partial=TRUE, ci=input$confint)
          output$esResults <- renderPrint({
            esResults
          })
        } else {
          esResults <- "Not Calculated"
        }


        # Conduct posthoc tests ------------------------------------------------
        if (!is.null(input$eva)) {

          if (input$numlvls1 <= 2) {
            output$phTests <- renderPrint({
              "Not enough levels to conduct post hoc tests. Consult the marginal means instead."
            })
            posthoc <- c()
          } else {
            dep <- data_prepared %>% pull("dependent_var")
            posthoc <- postHocCalc(input$eva, dep, within_var, input$confint)

            output$phTests <- renderPrint({
              posthoc
            })
          }

        } else {
          posthoc <- c()
        }

        # Generate the downloadable pdf report ---------------------------------
        params <- list(descr = descriptives, anova=anovaResults, n2=esResults,
                       em_o=em_over, em=em_fit1, posthoc=posthoc)

        output$report <- generate_report("one_way_within_report", params)
       }
      
     })
    
  })
  
}