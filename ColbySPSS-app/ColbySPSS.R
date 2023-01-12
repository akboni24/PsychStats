library(shiny)
library(bslib)

# here is where I am importing all of my other files - there's a better way to do
# this, I just haven't gotten it to work yet
source("~/Documents/git_repos/SPSS-R/ColbySPSS-app/Analyze/freq.R")

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
               checkboxInput("header", "Header Row", TRUE)), 
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
             tabPanel("Descriptives", "desc"),
             "Compare Means",
             tabPanel("Means", "m"),
             tabPanel("One Sample T Test", "o"),
             tabPanel("Ind Samples T Test", "I"),
             tabPanel("Paired Samples T Test", "P"),
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
    ext <- tools::file_ext(input$file1$name)
    switch(ext,
           csv = vroom::vroom(input$file1$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  
  output$table1 <- DT::renderDataTable({
    
    DT::datatable(df(), options = list(
      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
      pageLength = 15)
    )
  })
  
  # will need to include a call to all of the server functions of my separate module
  freqServer("freq1", df)
  #univariateServer("uni1")
}

shinyApp(ui = ui, server = server)