library(dplyr)
library(shiny)
library(car)
library(sortable)
library(ggpubr)
library(emmeans)

# Helper function - load_file() ------------------------------------------------
# Loads the given csv file and turns it into a dataframe
# Arguments: name (filename) and path (file path)
# Returns: a dataframe object
# ------------------------------------------------------------------------------
load_file <- function(name, path) {
  ext <- tools::file_ext(name)
  switch(ext,
       csv = vroom::vroom(path, delim = ","),
       validate("Invalid file; Please upload a .csv file"))
}

# Helper function - find_vars() ------------------------------------------------
# Extracts the variable names from the given dataset
# Arguments: data (a dataframe object)
# Returns: a character vector of variable names
# ------------------------------------------------------------------------------
find_vars <- function(data) {
  stopifnot(is.data.frame(data))
  names(data)
}

# Helper function - extractCols() ----------------------------------------------
# Extract columns of data based on variable names selected
# Arguments: vars (a list of variable names) and data (a dataframe)
# Returns: a dataframe object
# ------------------------------------------------------------------------------
extractCols <- function(vars, data) {
  stopifnot(is.data.frame(data))
  data %>% subset(select=vars)
}

# Helper function - makeFactor() ----------------------------------------------
# Turns a give variable into a factor
# Arguments: var (variable name) and data (a dataframe)
# Returns: Nothing
# ------------------------------------------------------------------------------
#makeFactor <- function(vars, data) {
 # df[vars] <- lapply(df[vars] , factor)
#}

# Helper function - checkFactor() ----------------------------------------------
# Checks if every given variable is a categorical variable
# Arguments: vars (list of variable names)
# Returns: Boolean
# ------------------------------------------------------------------------------
checkFactor <- function(var, data) {
  col <- data %>% subset(select=var)
  is.factor(col)
}

# Helper function - checkNumeric() ----------------------------------------------
# Checks if every given variable is a numeric variable
# Arguments: vars (list of variable names)
# Returns: Boolean
# ------------------------------------------------------------------------------
checkNumeric <- function(var, data) {
  col <- data %>% subset(select=var)
  return(is.numeric(col))
}

# Statistics Modal Function ----------------------------------------------------
# Creates a modal (pop-up window) that asks for user input of what stats they want to be calculated
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
statsModal <- function(input, output, session) {
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

# Dispersion Function ----------------------------------------------------------
# Creates a checkboxGroupInput for measures of dispersion
# Arguments: input, output, session
# ------------------------------------------------------------------------------
disp <- function(session) {
  ns <- session$ns
  checkboxGroupInput(ns("disp"), label = "Dispersion", c("Std. Deviation", "Variance", "Range", "Minimum", "Maximum", "S.E. Mean"))
}


# Helper function - centraltendency() ------------------------------------------
# Calculates measures of central tendency on a group of variables
# Arguments: cols (dataframe columns) and func (list of functions to calculate)
# Returns: results, a list of the calculated values
# TO ADD: MAKE RESULTS AN RMARKDOWN FILE INSTEAD
# ------------------------------------------------------------------------------
centraltendency <- function(cols, func) {
  results <- list()
  # use lapply to apply each function selected to each variable chosen
  if ("Mean" %in% func) {
    means <- lapply(cols, mean)
    results <- append(results, means)
  }
  
  if ("Median" %in% func) {
    medians <- lapply(cols, median)
    results <- append(results, medians)
  }
  
  if ("Mode" %in% func) {
    # Have to create own mode function
    modes <- lapply(cols, function(x) {
      uniqx <- unique(x)
      uniqx[which.max(tabulate(match(x, uniqx)))]
    })
    results <- append(results, modes)
  }
  
  if ("Sum" %in% func) {
    sums <- lapply(cols, sum)
    results <- append(results, sums)
  }
  
  results

}

# Helper function - dispersion() -----------------------------------------------
# Calculates measures of dispersion on a group of variables
# Arguments: cols (dataframe columns) and func (list of functions to calculate)
# Returns: results, a list of the calculated values
# TO ADD: MAKE RESULTS AN RMARKDOWN FILE INSTEAD
# ------------------------------------------------------------------------------
dispersion <- function(cols, func) {
  results <- list()
  # use lapply to apply each function selected to each variable chosen
  if ("Std. Deviation" %in% func) {
    std <- lapply(cols, sd)
    results <- append(results, std)
  }
  
  if ("Variance" %in% func) {
    variances <- lapply(cols, var)
    results <- append(results, variances)
  }
  
  if ("Range" %in% func) {
    # Have to create own mode function
    ranges <- lapply(cols, range)
    results <- append(results, ranges)
  }
  
  if ("Minimum" %in% func) {
    mins <- lapply(cols, min)
    results <- append(results, mins)
  }
  
  if ("Maximum" %in% func) {
    maxs <- lapply(cols, max)
    results <- append(results, maxs)
  }
  
  if ("S.E. Mean" %in% func) {
    stderror <- function(x) sd(x)/sqrt(length(x))
    se <- lapply(cols, stderror)
    results <- append(results, se)
  }
  
  results
  
}

# Options Modal Function for Descriptives --------------------------------------
# Creates a modal (pop-up window) that asks for user input of what stats they want to be calculated
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
descOptionsModal <- function(input, output, session) {
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "Descriptives: Options",
    checkboxGroupInput(ns("desc"), label = NULL, c("Mean", "Sum"), selected = "Mean"),
    disp(session),
    checkboxGroupInput(ns("dist"), label = "Distribution", c("Skewness", "Kurtosis")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}

# Options Modal Function for T Tests -------------------------------------------
# Creates a modal (pop-up window) that asks for user input of confidence intervals
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
ttestOptionsModal <- function(input, output, session) {
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "T Test: Options",
    numericInput(ns("confint"), label = "Confidence Interval Percentage: ", value = "0.95"),
    radioButtons(ns("mv"), label = "Missing Values", 
                 choices = list("Exclude cases analysis by analysis" = 1, "Exclude cases listwise" = 2), selected = 1),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
}

# Statistic Calculations for T Tests -------------------------------------------
# Calculates one and two sample statistics for t tests
# Arguments: cols (columns of data)
# ------------------------------------------------------------------------------
ttestStats <- function(var1, var2 = NULL) {
  df <- data.frame( "Variable Name" = character(),
                    "N" = numeric(),
                   "Mean" = numeric(),
                   "Std.Deviation" = numeric(),
                   "Std.Error Mean" = numeric())
  N <- length(var1)
  mean <- mean(var1)
  stddev <- sd(var1)
  stderror <- function(x) sd(x)/sqrt(length(x))
  se <- stderror(var1)
  df[1, ] <- list("1", N, mean, stddev, se)
  if (!is.null(var2)) {
    N2 <- length(var2)
    mean2 <- mean(var2)
    stddev2 <- sd(var2)
    se2 <- stderror(var2)
    df[2, ] <- list("2", N2, mean2, stddev2, se2)
  }
  return(df)
}


indttestStats <- function(var1, var2) {
  df <- data.frame( "Variable Name" = character(),
                    "N" = numeric(),
                    "Mean" = numeric(),
                    "Std.Deviation" = numeric(),
                    "Std.Error Mean" = numeric())
  N <- by(var1, var2, length)
  mean <- by(var1, var2, mean)
  stddev <- by(var1, var2, sd)
  stderror <- function(x) sd(x)/sqrt(length(x))
  se <- by(var1, var2, stderror)
  df[1, ] <- list("1", N[1], mean[1], stddev[1], se[1])
  df[2, ] <- list("2", N[2], mean[2], stddev[2], se[2])

  return(df)
}

