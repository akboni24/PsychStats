library(shiny)
source("~/ColbySPSS-app/chooserInput.R")

univariateUI <- function(id) {
  ns <- NS(id)
  tagList (
    
    titlePanel("Univariate"),
    
    fluidRow(
      column (
        width = 12,
    chooserInput(inputId = ns("uni1"), "Variables", "Dependent Variable",
                 c("variable 1", "variable 2", "variable 3"), c(), size = 10, multiple = TRUE),
    verbatimTextOutput("selection")
      )
  )
  )
}
  
univariateServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

