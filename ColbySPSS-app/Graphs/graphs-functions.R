# This file contains helper functions for the Graphs Menu

# Titles Modal for Scatter -----------------------------------------------------
scatterTitlesModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) for optional titles to be added to the plot
  #' Arguments: Shiny arguments input, output, and session
  # ----------------------------------------------------------------------------
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

# Not including Options modal bc it just deals with error bars and we
# don't use those in 214...

add_title <- function(vis, ..., properties=NULL, title = "Plot Title", footnote=FALSE,
                                                            subtitle=FALSE) 
{
  # recursively merge lists by name
  # http://stackoverflow.com/a/13811666/1135316
  # edited by Ainsley Bonin to be able to create a title or footnote
  
  # don't step on existing scales.
  vis <- scale_numeric(vis, "title", domain = c(0,1), range = 'width')
  if (footnote == TRUE) {
    axis <- ggvis:::create_axis('x', 'title', orient = "bottom", ticks=0, title = title, 
                                offset=50, properties = axis_props(
                                  axis = list(stroke = "white"),
                                  labels = list(fontSize = 0)
                                ))
  } else if (subtitle == TRUE) {
    axis <- ggvis:::create_axis('x', 'title', orient = "top", ticks = 0, title = title, 
                                offset=20, properties = axis_props(
                                  axis = list(stroke = "white"),
                                  labels = list(fontSize = 0)
                                ))
  } else {
    axis <- ggvis:::create_axis('x', 'title', orient = "top", ticks = 0, title = title, 
                                offset=40, properties = axis_props(
                                  axis = list(stroke = "white"),
                                  labels = list(fontSize = 0)
                                ))
  }
  
  
  
  ggvis:::append_ggvis(vis, "axes", axis)
}

