library(dplyr)
library(shiny)
# Statistics Modal Function ----------------------------------------------------
# Creates a modal (pop-up window) that asks for user input of what stats they want to be calculated
# Takes in a list of variables that the user selected on the main page
# ------------------------------------------------------------------------------
statsModal <- function(input, output, session, vars_selected) {
  ns <- session$ns
  # Create the pop-up window
  showModal(modalDialog(
    title = "Frequencies: Statistics",
    checkboxGroupInput(ns("percValues"), label = "Percentile Values", c("Quartiles", "Cut points for 10 equal groups", "Percentiles")),
    checkboxGroupInput(ns("centen"), label = "Central Tendency", c("Mean", "Median", "Mode", "Sum")),
    checkboxGroupInput(ns("disp"), label = "Dispersion", c("Std. Deviation", "Variance", "Range", "Minimum", "Maximum")),
    checkboxGroupInput(ns("dist"), label = "Distribution", c("Skewness", "Kurtosis")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  ))

  # Wait for the user to hit submit
  observeEvent(input$submit, {
    
    # close the pop-up window
    removeModal()
    
    # Central Tendency ---------------------------------------------------------
    centenSelect <- mget(input$centen)
    
    results <- list()
    # use lapply to apply each function selected to each variable chosen
    if ("Mean" %in% centenSelect) {
      results.append(lapply(vars_selected, mean))
    }
    if ("Median" %in% centenSelect) {
      results.append(lapply(vars_selected, median))
    }
    if ("Mode" %in% centenSelect) {
      # Have to create own mode function
      results.append(lapply(vars_selected, function(x) {
        uniqx <- unique(x)
        uniqx[which.max(tabulate(match(x, uniqx)))]
      }))
    }
    if ("Sum" %in% centenSelect) {
      results.append(lapply(vars_selected, sum))
    }
    
    results
    
  })
  
  
  
  
}