library(dplyr)
library(shiny)
# Helper Function - find_vars() ------------------------------------------------
# Extracts the variables from the given dataset
find_vars <- function(data) {
  stopifnot(is.data.frame(data))
  names(data)
}
# Statistics Modal Function ----------------------------------------------------
# Creates a modal (pop-up window) that asks for user input of what stats they want to be calculated
# Takes in a list of variables that the user selected on the main page
# ------------------------------------------------------------------------------
statsModal <- function(input, output, session, vars_selected) {
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "Frequencies: Statistics",
    checkboxGroupInput(ns("percValues"), label = "Percentile Values", c("Quartiles", "Cut points for 10 equal groups", "Percentiles")),
    checkboxGroupInput(ns("centen"), label = "Central Tendency", c("Mean", "Median", "Mode", "Sum")),
    checkboxGroupInput(ns("disp"), label = "Dispersion", c("Std. Deviation", "Variance", "Range", "Minimum", "Maximum")),
    checkboxGroupInput(ns("dist"), label = "Distribution", c("Skewness", "Kurtosis")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}