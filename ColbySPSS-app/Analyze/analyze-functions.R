library(dplyr)
library(shiny)
library(car)
library(sortable)
library(ggpubr)
library(emmeans)
library(ggplot2)

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

# Charts Modal Function --------------------------------------------------------
freqChartsModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) that asks for user input of what type of 
  #' chart they want to be created
  #' 
  #' Arguments: Shiny arguments input, output, and session
  #' ---------------------------------------------------------------------------
  ns <- session$ns
  # Create the pop-up window
  modalDialog(
    title = "Frequencies: Charts",
    radioButtons(ns("type"), label="Chart Type", c("Bar Chart", 
                "Pie Chart"), selected = "Bar Chart"),
    radioButtons(ns("values"), label="Chart Values", c("Frequencies", "Percentages"),
                 selected="Frequencies"),
    footer = tagList(modalButton("Cancel"), actionButton(ns("submit"), "Submit"))
  )
  
}

# if (type == "Histogram") {
#   
#   if (values == "Percentages") {
#     #chart <- hist(var, freq=FALSE, plot=FALSE)
#     #chart$density <- chart$counts / sum(chart$counts) * 100
#     if (normal == TRUE){
#       chart <- ggplot(var, aes(x=stat)) +
#         geom_histogram(aes(y=stat(density))) + 
#         scale_y_continuous(labels=scales::percent_format())
#     } else {
#       chart <- ggplot(var, aes(x=stat)) +
#         geom_histogram(aes(y=stat(density))) + 
#         scale_y_continuous(labels=scales::percent_format()) +
#         stat_function(fun=dnorm, args=list(mean=mean(var), sd=sd(var)))
#     }
#   } else {
#     if (normal == TRUE) {
#       chart <- ggplot(var, aes(x)) +
#         geom_histogram() + 
#         stat_function(fun=dnorm, args=list(mean=mean(var), sd=sd(var)))
#     } else {
#       chart <- ggplot(data, aes(var)) +
#         geom_histogram()
#     }
#   }

# Frequency Charts Generator Function ------------------------------------------
freqCharts <- function(data, var, type, values) {
  
  if (type == "Bar Chart") {
    
    if (values == "Percentages") {
      chart <- ggplot(data, aes(var)) +
                  geom_bar(aes(y=(after_stat(count))/sum(after_stat(count)))) +
                  scale_y_continuous(labels = scales::percent) +
                  ylab("Percentages") 
    } else {
      chart <- ggplot(data, aes(var)) +
                geom_bar() +
                ylab("Frequency")
    }
    
  } else if (type == "Pie Chart") {
    
    if (values == "Percentages") {
      chart <- ggplot(data, aes(x=factor(1), fill=var)) +
        geom_bar(aes(y=(after_stat(count))/sum(after_stat(count))), width=1) +
        coord_polar("y") +
        scale_y_continuous(labels = scales::percent) +
        ylab("Percentages") 
    } else {
      chart <- ggplot(data, aes(x=factor(1), fill=var)) +
        geom_bar(width=1) +
        coord_polar("y")
        ylab("Frequency")
    }
  }
  
  chart <- chart +
            xlab(names(var)) +
            guides(color = guide_legend(override.aes=list(shape = 20))) +
            theme(axis.line = element_line(colour = "black"),
                  plot.margin=grid::unit(c(5,85,5,5), "mm"),
                  axis.line.x.bottom=element_line(linewidth=0.5),
                  axis.line.y.left=element_line(linewidth=0.5),
                  axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  plot.title = element_text(hjust = 0.5,size = 30, face = "bold"),
                  axis.text.x = element_text(color="black"),
                  axis.text.y = element_text(color="black"),
                  axis.ticks = element_line(color = "black"))  
  
  return(chart)
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
    Mean <- sapply(var, mean)
  } else {
    Mean <- "NA"
  }
  
  if ("Sum" %in% func) {
    Sum <- sapply(var, sum)
  } else {
    Sum <- "NA"
  }
  
  
  df <- data.frame(Mean, Sum, row.names=names(var))
  
  df

}

stderror <- function(x) sd(x)/sqrt(length(x))
my_range <- function(x) max(x) - min(x)

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

  # use lapply to apply each function selected to each variable chosen
  if ("Std. Deviation" %in% func) {
    Std.Dev <- sapply(cols, sd)
  } else {
    Std.Dev <- "NA"
  }
  
  if ("Variance" %in% func) {
    Variance <- sapply(cols, var)
  } else {
    Variance <- "NA"
  }
  
  if ("Range" %in% func) {
    Range <- sapply(cols, my_range)
  } else {
    Range <- "NA"
  }
  
  if ("Minimum" %in% func) {
    Min <- sapply(cols, min)
  } else {
    Min <- "NA"
  }
  
  if ("Maximum" %in% func) {
    Max <- sapply(cols, max)
  } else {
    Max <- "NA"
  }

  if ("S.E. Mean" %in% func) {
    SE <- sapply(cols, stderror)
  } else {
    SE <- "NA"
  }
  
  df <- data.frame(Std.Dev, Variance, Range, Min, Max, SE, row.names=names(cols))

  df
  
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
    numericInput(ns("confint"), label = "Confidence Interval Percentage: ", 
                 value = "0.95", min=0.01, max=0.99, step=0.01),
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

