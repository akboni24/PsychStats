# This file contains helper functions for the Graphs Menu

# Titles Modal for Scatter -----------------------------------------------------
# Creates a modal (pop-up window) for optional titles to be added to the plot
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
scatterTitlesModal <- function(input, output, session) {
  ns <- session$ns
  modalDialog (
    title = "Simple Scatter: Titles",
    h3("Title"),
    textInput(ns("main"), label="Line 1: "),
    textInput(ns("sub"), label="Subtitle: "),
    h3("Footnote"),
    textInput(ns("foot"), label="Line 1: "),
    footer = tagList(modalButton("Cancel"), actionButton(ns("continue"), 
                                                         "Continue"))
  )
}

# Titles Modal for ANOVA ------------------------------------------------------
# Creates a modal (pop-up window) for optional titles to be added to the plot
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
scatterOptionsModal <- function(input, output, session) {
  ns <- session$ns
  modalDialog (
    title = "Simple Scatter: Options",
    checkboxInput(ns("errorBars"), "Include Error bars"),
    radioButtons(ns("ebOptions"), label="Error Bars Represent", 
                 c("Confidence Interval (95.0%)", "Standard Error", 
                   "Standard Deviation")),
    numericInput(ns("lvl"), label="Level (%) or Multiplier", value = 0.95),
    footer = tagList(modalButton("Cancel"), actionButton(ns("continue"), 
                                                         "Continue"))
  )
}