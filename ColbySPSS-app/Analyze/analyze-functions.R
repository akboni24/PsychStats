library(dplyr)
library(shiny)
library(car)
library(sortable)
library(ggpubr)
library(emmeans)

# This file contains some helper functions for the main page, as well as functions
# for frequencies and descriptives in the Analyze tab

# Helper function for main page ------------------------------------------------
load_file <- function(name, path) {
  #'Loads the given csv file and turns it into a dataframe
  #'
  #' Parameters:
  #' -----------
  #' name. character vector, filename
  #' path. character vector, file path
  #' 
  #' Returns:
  #' -------- 
  #' dataframe
  #' ---------------------------------------------------------------------------
  ext <- tools::file_ext(name)
  switch(ext,
       csv = vroom::vroom(path, delim = ","),
       validate("Invalid file; Please upload a .csv file"))
}

# Helper function - find_vars() ------------------------------------------------
find_vars <- function(data) {
  #' Extracts the variable names from the given dataset
  #'
  #' Parameters:
  #' -----------
  #' data. dataframe object
  #' 
  #' Returns:
  #' ---------
  #' character vector of variable names
  #' ---------------------------------------------------------------------------
  stopifnot(is.data.frame(data))
  names(data)
}


# Helper function - extractCols() ----------------------------------------------
extractCols <- function(vars, data, func = NULL) {
  #' Extract columns of data based on variable names selected
  #' 
  #' Parameters:
  #' ----------- 
  #' vars. list of variable names
  #' data. dataframe
  #' 
  #' Returns:
  #' --------
  #' dataframe object
  #' ---------------------------------------------------------------------------
  stopifnot(is.data.frame(data))
  cols <- data %>% subset(select=vars)
}

# Helper function - extractCols() ----------------------------------------------
check_condition <- function(var, data, func) {
  #' Check a certain condition on a selected data variable
  #' 
  #' Parameters:
  #' -----------
  #' var. name of data variable 
  #' data. dataframe 
  #' func. function name
  #' 
  #' Returns:
  #' -------- 
  #' boolean
  #' ---------------------------------------------------------------------------
  col <- data %>% pull(var)
  condition <- func(col)
  return(condition)
}


# Helper function - errorText() ------------------------------------------------
error_text <- function(wrong_var_type, right_var_type) {
  #' Creates error text string for when user selects wrong type of variable
  #' 
  #' Parameters:
  #' -----------
  #' wrong_var_type. string 
  #' right_var_type. string
  #' 
  #' Returns:
  #' -------- 
  #' string
  #' ---------------------------------------------------------------------------
  error_text <- sprintf("You cannot conduct this type of statistical test on a %s 
  variable. Please select a %s variable.", wrong_var_type, right_var_type)
  return(error_text)
}

factor_warning <- function(var_name) {
  #' Creates some warning text to make sure the user selected a factor
  #' 
  #' Parameters:
  #' -----------
  #' var_name. str, name of variable selected as factor
  #' 
  #' Returns:
  #' ---------
  #' string
  #' ---------------------------------------------------------------------------
  warning_text <- sprintf("Reminder: Make sure %s is a factor/categorical variable
                          before interpreting the following results.", var_name)
  return(warning_text)
}

# Statistics Modal Function ----------------------------------------------------
statsModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) that asks for user input of what stats 
  #' they want to be calculated
  #' 
  #' Arguments: Shiny arguments input, output, and session
  #' ---------------------------------------------------------------------------
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "Frequencies: Statistics",
    checkboxGroupInput(ns("percValues"), label = "Percentile Values", c("Quartiles", 
                      "Cut points for 10 equal groups", "Percentiles")),
    checkboxGroupInput(ns("centen"), label = "Central Tendency", c("Mean", 
                      "Median", "Mode", "Sum")),
    checkboxGroupInput(ns("disp"), label = "Dispersion", c("Std. Deviation", 
                                  "Variance", "Range", "Minimum", "Maximum")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}

# Statistics Modal Function ----------------------------------------------------
freqChartsModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) that asks for user input of what stats 
  #' they want to be calculated
  #' 
  #' Arguments: Shiny arguments input, output, and session
  #' ---------------------------------------------------------------------------
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "Frequencies: Charts",
    radioButtons(ns("type"), label="Chart Type", c("None", "Bar Charts", 
                "Pie Charts", "Histograms"), selected = "None"),
    checkboxInput(ns("normal"), label="Show normal curve on histogram", 
                  value=FALSE),
    radioButtons(ns("values"), label="Chart Values", c("Frequencies", "Percentages"),
                 selected="Frequencies"),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}

# Dispersion Function ----------------------------------------------------------
disp <- function(session) {
  #' Creates a checkboxGroupInput for measures of dispersion
  #' Arguments: input, output, session
  #' ---------------------------------------------------------------------------
  ns <- session$ns
  checkboxGroupInput(ns("disp"), label = "Dispersion", 
    c("Std. Deviation", "Variance", "Range", "Minimum", "Maximum", "S.E. Mean"))
}

mode <- function(x) {
            uniqx <- unique(x)
            uniqx[which.max(tabulate(match(x, uniqx)))]
          }

# Helper function - centraltendency() ------------------------------------------
centraltendency <- function(var, func) {
  #' Calculates measures of central tendency on a group of variables
  #' 
  #' Parameters:
  #' -----------
  #' var. data variable
  #' func. list of functions to calculate
  #' 
  #' Returns:
  #' --------
  #' dataframe containing the calculated statistics
  #' ---------------------------------------------------------------------------
  if ("Mean" %in% func) {
    Mean <- mean(var)
  } else {
    Mean <- c()
  }
  
  if ("Median" %in% func) {
    Median <- median(var)
  } else {
    Median <- c()
  }
  
  if ("Mode" %in% func) {
    # Have to create own mode function
    Mode <- mode(var)
  } else {
    Mode <- c()
  }
  
  if ("Sum" %in% func) {
    Sum <- sum(var)
  } else {
    Sum <- c()
  }
  
  df <- data.frame(Mean, Median, Mode, Sum)
  names(df)[1] <- vars_name
  df

}

# Helper function - dispersion() -----------------------------------------------
dispersion <- function(cols, func) {
  #' Calculates measures of dispersion on a group of variables
  #' 
  #' Parameters:
  #' -----------
  #' cols. data variable
  #' func. list of functions to calculate
  #' 
  #' Returns:
  #' --------
  #' dataframe containing the calculated statistics
  #' ----------------------------------------------------------------------------
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
descOptionsModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) that asks for user input of what stats they
  #' want to be calculated
  #' 
  #' Arguments: Shiny arguments input, output, and session
  #' ---------------------------------------------------------------------------
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "Descriptives: Options",
    checkboxGroupInput(ns("desc"), label = NULL, c("Mean", "Sum"), selected = "Mean"),
    disp(session),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}

# Options Modal Function for T Tests -------------------------------------------
ttestOptionsModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) that asks for user input of confidence intervals
  #' 
  #' Arguments: Shiny arguments input, output, and session
  #' ---------------------------------------------------------------------------
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "T Test: Options",
    numericInput(ns("confint"), label = "Confidence Interval Percentage: ", value = "0.95"),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
}

# Statistic Calculations for T Tests -------------------------------------------
ttestStats <- function(var1, var2 = NULL) {
  #' Calculates one sample or paired samples statistics for t tests
  #' 
  #' Parameters:
  #' -----------
  #' var1. data variable
  #' var2. data variable or NULL if conducting one sample stats
  #' 
  #' Returns:
  #' --------
  #' dataframe containing all summary statistics
  #' ---------------------------------------------------------------------------
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
  #' Calculates two sample statistics for t tests
  #' 
  #' Parameters:
  #' -----------
  #' var1. data variable
  #' var2. data variable
  #' 
  #' Returns:
  #' --------
  #' dataframe containing all summary statistics
  #' ---------------------------------------------------------------------------
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

