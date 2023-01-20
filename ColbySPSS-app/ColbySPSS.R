library(shiny)
library(bslib)

# here is where I am importing all of my other files - there's a better way to do
# this, I just haven't gotten it to work yet
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/freq.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/descriptives.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/oneSampleT.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/indSamplesT.R")
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/pairedSamplesT.R")

# Main user interface - Navbar at the top, data table and csv import on the main page
ui <- navbarPage(
  theme = bs_theme(bootswatch = "yeti"),
  "SPSS-R",
  selected = "Home",
  # in navBarMenu, will replace the name of the module as the second argument
  tabPanel("Home", 
           sidebarLayout(
             # sidebar panel for csv file upload
             sidebarPanel(
               # Input: Select a file ---------------
               fileInput("file1", "Choose a CSV File",
                         multiple = FALSE,
                         # only accepts csv's
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               # Input: Checkbox if file has header ----
               # CURRENTLY NOT BEING USED
               checkboxInput("header", "Header Row", TRUE), 
              # Input: Check all categorical variables
                checkboxGroupInput("factors", "Please check all categorical variables: ", character(0))),
            # Main Panel for the data table ---------------
            mainPanel(
             DT::dataTableOutput("table1")
           ))),
  navbarMenu("File", "one"),
  navbarMenu("Edit", "two"),
  navbarMenu("View", "three"),
  navbarMenu("Data", "four"),
  navbarMenu("Transform", "five"),
  navbarMenu("Analyze", 
             "Descriptive Stats", # section headers
             tabPanel("Frequencies", fluidPage(freqUI("freq1"))),
             tabPanel("Descriptives", fluidPage(descriptivesUI("desc1"))),
             "Compare Means",
             tabPanel("Means", "m"),
             tabPanel("One Sample T Test", fluidPage(oneSampleTUI("oneT"))),
             tabPanel("Ind Samples T Test", fluidPage(indSamplesTUI("indT"))),
             tabPanel("Paired Samples T Test", fluidPage(pairedSamplesTUI("pT"))),
             tabPanel("One Way ANOVA", "anova"),
             "General Linear Model",
             #tabPanel("Univariate", fluidPage(univariateUI("uni1"))),
             tabPanel("Multivariate", "m"),
             "Regression",
             tabPanel("Linear", "lm")),
  navbarMenu("Graphs", "seven"),


)



server <- function(input, output, session) {
  
  # Data Table ----------------------------------------------------------------
  # Converts given csv to a data frame and turns it into a data table
  df <- reactive({
    req(input$file1)
    load_file(input$file1$name, input$file1$datapath)
    #mutate_if(df, is.character, as.factor)
  })
  
  
  output$table1 <- DT::renderDataTable({
    
    DT::datatable(df(), editable = "cell", options = list(
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15)
    )
  })
  
  # Updates the checkboxGroupInput with all of the variable names
  observeEvent(input$file1, {
    updateCheckboxGroupInput(session, "factors", choices = find_vars(df()))
  })
  
  
  # will need to include a call to all of the server functions of my separate module
  freqServer("freq1", df)
  descriptivesServer("desc1", df)
  oneSampleTServer("oneT", df)
  indSamplesTServer("indT", df)
  pairedSamplesTServer("pT", df)
}

shinyApp(ui = ui, server = server)