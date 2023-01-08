library(shiny)
library(bslib)
source("~/ColbySPSS-app/Analyze/freq.R")
source("~/ColbySPSS-app/Analyze/univariate.R")
ui <- navbarPage(
  theme = bs_theme(bootswatch = "yeti"),
  "SPSS-R",
  # in navBarMenu, will replace the name of the module as the second argument
  navbarMenu("File", "one"),
  navbarMenu("Edit", "two"),
  navbarMenu("View", "three"),
  navbarMenu("Data", "four"),
  navbarMenu("Transform", "five"),
  navbarMenu("Analyze", 
             "Descriptive Stats", 
              tabPanel("Frequencies", fluidPage(freqUI("freq1"))),
              tabPanel("Descriptives", "desc"),
             "Compare Means",
             tabPanel("Means", "m"),
             tabPanel("One Sample T Test", "o"),
             tabPanel("Ind Samples T Test", "I"),
             tabPanel("Paired Samples T Test", "P"),
             tabPanel("One Way ANOVA", "anova"),
             "General Linear Model",
             tabPanel("Univariate", fluidPage(univariateUI("uni1"))),
             tabPanel("Multivariate", "m"),
             "Regression",
             tabPanel("Linear", "lm")),
  navbarMenu("Graphs", "seven"),
    
  mainPanel(
    # DT::dataTableOutput("dataView")
  )
)


server <- function(input, output, session) {
  
  freqServer("freq1")
  univariateServer("uni1")
  
  # -1 means no pagination; the 2nd element contains menu labels
  #output$dataView <- DT::renderDataTable(
    #DT::datatable(
      #iris, options = list(
       #lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        #pageLength = 15
      #)
    #)
  #)
  
}

shinyApp(ui = ui, server = server)