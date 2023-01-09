library(dplyr)
statsModal <- function(vars = list()) {
  # Create the pop-up window
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "Frequencies: Statistics",
      checkboxGroupInput(ns("percValues"), label = "Percentile Values", c("Quartiles", "Cut points for 10 equal groups", "Percentiles")),
      checkboxGroupInput(ns("centen"), label = "Central Tendency", c("Mean", "Median", "Mode", "Sum")),
      checkboxGroupInput(ns("disp"), label = "Dispersion", c("Std. Deviation", "Variance", "Range", "Minimum", "Maximum")),
      checkboxGroupInput(ns("dist"), label = "Distribution", c("Skewness", "Kurtosis")),
      footer = tagList(modalButton("Cancel"), actionButton("submit", "Submit"))
    ))
  })
  
  # Not sure exactly how to proceed here... Here is some rough pseudocode
  centenSelect <- mget(input$centen)
  
  # use lapply to apply each function selected to each variable chosen
  
}