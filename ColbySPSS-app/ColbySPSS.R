library(shiny)
library(bslib)

# here is where I am importing all of my other files - there's a better way to do
# this, I just haven't gotten it to work yet
source("~/ColbySPSS-app/Analyze/freq.R")
source("~/ColbySPSS-app/Analyze/univariate.R")

# Main user interface - Navbar at the top, data table and usv import on the main page
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
             "Descriptive Stats", # section headers
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
    # still need to add the csv upload
    DT::dataTableOutput("dataView")
  )
)


server <- function(input, output, session) {
  
  # will need to include a call to all of the server functions of my seperate modules
  freqServer("freq1")
  univariateServer("uni1")
  
  # Data Table ----------------------------------------------------------------
  # -1 means no pagination; the 2nd element contains menu labels
  # has a default dataset in it right now
  output$dataView <- DT::renderDataTable(
    DT::datatable(
      iris, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  )
  
}

shinyApp(ui = ui, server = server)