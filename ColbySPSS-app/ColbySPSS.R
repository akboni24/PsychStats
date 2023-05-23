library(shiny)
library(bslib)
library(rstatix)
library(rmarkdown)
library(knitr)
library(ggvis)
library(readxl)
library(shinyjs)

# here is where I am importing all of my other files - there's a better way to do
# this, I just haven't gotten it to work yet
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/freq.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/descriptives.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/oneSampleT.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/indSamplesT.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/pairedSamplesT.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/oneWayANOVA.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/univariate.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/repeatedMeasures.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/regression.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Analyze/correlation.R")
source("~/Documents/git_repos/PsychStats/ColbySPSS-app/Graphs/scatter.R")

# Main user interface - Navbar at the top, data table and csv import on the main page
ui <- navbarPage(
  theme = bs_theme(bootswatch = "yeti"),
  "PsychStats",
  selected = "Home",
  # in navBarMenu, will replace the name of the module as the second argument
  tabPanel("Home", 
           sidebarLayout(
             # sidebar panel for csv file upload
             sidebarPanel(
               # Input: Select a file ---------------
               fileInput("file1", "Choose a CSV (.csv) or Excel (.xlsx) File",
                         multiple = FALSE,
                         # only accepts csv's
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv", 
                                    ".xlsx"))),
              
            # Main Panel for the data table ---------------
            mainPanel(
             DT::dataTableOutput("table1")
           ))),
  navbarMenu("Analyze", 
             "Descriptive Stats", # section headers
             tabPanel("Frequencies", fluidPage(freqUI("freq"))),
             tabPanel("Descriptives", fluidPage(descriptivesUI("desc"))),
             "Compare Means",
             tabPanel("Means", "m"),
             tabPanel("One Sample T Test", fluidPage(oneSampleTUI("oneT"))),
             tabPanel("Ind Samples T Test", fluidPage(indSamplesTUI("indT"))),
             tabPanel("Paired Samples T Test", fluidPage(pairedSamplesTUI("pT"))),
             tabPanel("One Way ANOVA", fluidPage(oneWayAnovaUI("owanova"))),
             "General Linear Model",
             tabPanel("Univariate", fluidPage(univariateUI("uni"))),
             tabPanel("Repeated Measures", fluidPage(repeatedMeasuresUI("repm"))),
             "Regression and Correlation",
             tabPanel("Linear", fluidPage(regressionUI("linreg"))),
             tabPanel("Bivariate Correlation", fluidPage(correlationUI("corr")))),
  navbarMenu("Graphs", 
             "Legacy Dialogs",
             tabPanel("Simple Scatter", fluidPage(scatterUI("scat")))),
  navbarMenu("R Tutorials", "five")


)



server <- function(input, output, session) {
  
  # Data Table ----------------------------------------------------------------
  # Converts given csv to a data frame and turns it into a data table
  df <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext == "xlsx") {
      read_excel(paste(input$file1$datapath))
    } else{
      load_file(input$file1$name, input$file1$datapath)
    }

  })
  
  
  output$table1 <- DT::renderDataTable({
    
    DT::datatable(df(), editable = "cell", options = list(
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15)
    )
  })
  
  # Updates the checkboxGroupInput with all of the variable names
  #observeEvent(input$file1, {
    #updateCheckboxGroupInput(session, "factors", choices = find_vars(df()))
  #})
  
  
  # will need to include a call to all of the server functions of my separate module
  freqServer("freq", df)
  descriptivesServer("desc", df)
  oneSampleTServer("oneT", df)
  indSamplesTServer("indT", df)
  pairedSamplesTServer("pT", df)
  oneWayAnovaServer("owanova", df)
  univariateServer("uni", df)
  repeatedMeasuresServer("repm", df)
  regressionServer("linreg", df)
  correlationServer("corr", df)
  scatterServer("scat", df)
}

shinyApp(ui = ui, server = server)