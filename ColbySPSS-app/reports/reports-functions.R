library(shiny)

# This file contains all of my helper functions for generating RMarkdown reports
# of the statistical results

generate_report <- function(name, params) {
  #' This function generates the downloadable RMarkdown report (as a pdf)
  #' 
  #' Arguments:
  #' -----------
  #'  name: str.
  #'    Name of the report - should follow convention {name of stat test}_report
  #'  params: list.
  #'    List of parameters to pass to the RMarkdown file
  #'
  #' Returns:
  #' ---------
  #' report: pdf document.
  #'    Rendered RMarkdown document (pdf)
  report <- downloadHandler (
    filename <- paste0(name, ".pdf"),
    content = function(file) {
      tempReport <- file.path(tempdir(), paste0(name, ".Rmd"))
      copy_name <- paste0("reports/", name, ".Rmd")
      file.copy(copy_name, tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file, params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  report
}
  

