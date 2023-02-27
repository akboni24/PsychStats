library(dplyr)
library(shiny)
library(car)
library(sortable)
library(ggpubr)


# Regression Modals ------------------------------------------------------------

# Lin Reg Stats Modal ----------------------------------------------------------
# Creates a pop up window that asks the user what statistics they want calculated
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
lrStatsModal <- function(input, output, session) {
  ns <- session$ns
  # need to add numeric input to change conf int level
  modalDialog(
    title = "Linear Regression: Statistics",
    fluidRow( 
    column(
      width = 6, 
      checkboxGroupInput(ns("regcoef"), label = "Regression Coefficients", c("Estimates", "Confidence Intervals (95%)", "Covariance matrix"), selected="Estimates")
    ), column(
      width = 6,
      checkboxGroupInput(ns("other"), label = NULL, c("Descriptives", "Part and partial correlations", "Collinearity diagnostics")),
    )
    ),
    fluidRow(
      column(
        width = 12,
        # should i get rid of this?
        checkboxGroupInput(ns("resid"), label = "Residuals", c("Durbin-Watson", "Casewise Diagnostics"))
      )
    ),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}

# Correlation Options Modal ----------------------------------------------------
# Creates a pop up window that asks the user what statistics they want calculated
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
corrOptionsModal <- function(input, output, session) {
  ns <- session$ns
  # need to add numeric input to change conf int level
  modalDialog(
    title = "Bivariate Correlations: Options",
    fluidRow( 
      column(
        width = 10, 
        checkboxGroupInput(ns("stats"), label = "Statistics", c("Means and standard deviations", "Covariances"))
      )),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}


means_and_sd <- function(x) {
  c(mean = mean(x), sd = sd(x))
}
