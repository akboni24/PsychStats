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
  data %>% select(vars)
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
  col <- data %>% select(var)
  is.factor(col)
}

# Helper function - checkNumeric() ----------------------------------------------
# Checks if every given variable is a numeric variable
# Arguments: vars (list of variable names)
# Returns: Boolean
# ------------------------------------------------------------------------------
checkNumeric <- function(var, data) {
  col <- data %>% select(var)
  is.numeric(col)
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
  
  # add S.E. mean
  
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

# Post Hoc Modal for ANOVA -----------------------------------------------------
# Creates a modal (pop-up window) for multiple comparisons options
# Arguments: Shiny arguments input, output, and session
# ------------------------------------------------------------------------------
anovaPostHocModal <- function(input, output, session) {
  ns <- session$ns
  modalDialog (
    title = "ANOVA: Post Hoc Multiple Comparisons",
    checkboxGroupInput(ns("eva"), label = "Equal Variances Assumed", c("LSD", "Bonferroni", "Tukey's HSD")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("continue"), "Continue"))
  )
}

# Options Modal for ANOVA ------------------------------------------------------
# Creates a modal (pop-up window) for optional statistics to be calculated
# Arguments: Shiny arguments input, output, and session
# COME BACK - could make missing values a function, used multiple times
# ------------------------------------------------------------------------------
anovaOptionsModal <- function(input, output, session) {
  ns <- session$ns
  modalDialog (
    title = "ANOVA: Options",
    checkboxGroupInput(ns("stat"), label = "Statistics", c("Descriptives", "Homogeneity of variance test", "Welch test")),
    radioButtons(ns("mv"), label = "Missing Values", 
                 choices = list("Exclude cases analysis by analysis" = 1, "Exclude cases listwise" = 2), selected = 1),
    numericInput(ns("confint"), label = "Confidence Intervals (Level%)", value = "0.95"),
    footer = tagList(modalButton("Cancel"), actionButton(ns("continue"), "Continue"))
  )
}


# Post Hoc Calculations Function for ANOVA -------------------------------------
# Calculates post hoc tests for an aov object
# Arguments: tests (list of tests), var 1 & var2 (variables for the anova), and conflvl (a confidence level)
# ------------------------------------------------------------------------------
postHocCalc <- function(tests, var1, var2, conflvl) {
  
  if ("Bonferroni" %in% tests) {
    return(pairwise.t.test(var1, var2, p.adj = "bonf"))
  } else if ("LSD" %in% tests) {
    return(pairwise.t.test(var1, var2, p.adj = "lsd", conf.level = conflvl))
  } else if ("Tukey's HSD" %in% tests) {
    return(TukeyHSD(aov(var1 ~ var2)))
  }
  
}

# Options Calculations for ANOVA -----------------------------------------------
# Calculates the optional statistics for an aov object
# Arguments: tests (list of tests), formula (formula for anova)
# ------------------------------------------------------------------------------
anovaOptionsCalc <- function(tests, formula, var1, var2, var3 = NULL) {
  results <- list()
  if (is.null(var3)) {
    df <- data.frame(var1, var2)
  } else {
    df <- data.frame(var1, var2, var3)
  }
  if ("Descriptives" %in% tests) {
    results <- append(results, summary(df))
  }
  if ("Homogeneity of variance test" %in% tests) {
    results <- append(results, leveneTest(formula, center=mean))
  }
  if ("Welch test" %in% tests) {
    results <- append(results, oneway.test(formula))
  }
  results
}

# Plot Modal for Univariate Page -----------------------------------------------
# Pop-up window for making plots for two-way ANOVA
# Arguments: Shiny arguments (input, output, session) and vars (list of variables to display)
# ------------------------------------------------------------------------------
uniPlotsModal <- function(input, output, session, vars) {
  ns <- session$ns
  modalDialog (
    title = "Univariate: Profile Plots",
    fluidRow(
    bucket_list(
      header = NULL,
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Factors:",
        labels = vars,
        input_id = ns("plotFactors")),
      add_rank_list(
        text = "Horizontal Axis: ",
        labels = NULL,
        input_id = ns("plotXAxis")
      ), 
      add_rank_list(
        text = "Separate Lines: ",
        labels = NULL,
        input_id = ns("plotSepLines")
      ))),
    fluidRow(
    radioButtons(ns("type"), label="Chart Type", choices = c("Line Chart", "Bar Chart"))),
    fluidRow(
    checkboxInput(ns("errorBars"), "Include Error bars"),
    radioButtons(ns("ebOptions"), label=NULL, c("Confidence Interval (95.0%)", "Standard Error"))),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"), "Continue"))
  )
}

# Plotting function for Univariate Page ----------------------------------------
# Creates a two-way interaction plot
# Arguments: df (dataframe), x (variable on x-axis), group (variable for separate lines), dep (dependent variable),
# type (type of plot), and eb (Error bars: either "None", "Confint", or "SE)
# ------------------------------------------------------------------------------
uniMakePlot <- function(df, x, group, dep, type = "Line Chart", errorBars) {
  if ("Bar Chart" %in% type) {
    return(ggbarplot(df, x, dep, fill = group, color = group, palette = "Paired", label = TRUE, position = position_dodge(0.9), add = errorBars))
  } else {
    return(ggline(df, x, y = dep, color = group, add = errorBars))
  }
}

# Post Hoc Multiple Comparisons Modal for Univariate Page ----------------------
# Creates a pop-up window for multiple comparisons
# Arguments: Shiny args (input, output, session) and factors (list of factors)
# ------------------------------------------------------------------------------
uniPostHocModal <- function(input, output, session, factors) {
  ns <- session$ns
  modalDialog (
    title = "Univariate: Post Hoc Multiple Comparisons for Observed Means",
      bucket_list(
        header = NULL,
        group_name = "bucket_list_group",
        orientation = "horizontal",
        add_rank_list(
          text = "Factor(s):",
          labels = factors,
          input_id = ns("factors")),
        add_rank_list(
          text = "Post Hoc Tests for: ",
          labels = NULL,
          input_id = ns("postHocVars")
        )),
    checkboxGroupInput(ns("eva"), label = "Equal Variances Assumed", c("LSD", "Bonferroni", "Tukey's HSD")),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"), "Continue"))
  )
}

# Post Hoc Calculations for Two Way ANOVA --------------------------------------
# Calculates post hoc tests for a two way anova
# Arguments: tests (list of tests), y (dependent variable), vars (factors for the anova), and anova (anova object)
# ------------------------------------------------------------------------------
uniPostHocCalc <- function(tests, y, factor1, factor2) {
  
  results <- list()
  if ("Bonferroni" %in% tests) {
    append(results, pairwise.t.test(y, factor1, p.adj = "bonf"))
    append(results, pairwise.t.test(y, factor2, p.adj = "bonf"))
  } else if ("LSD" %in% tests) {
    append(results, pairwise.t.test(y, factor1, p.adj = "lsd"))
    append(results, pairwise.t.test(y, factor2, p.adj = "lsd"))
  } else if ("Tukey's HSD" %in% tests) {
    return(TukeyHSD(aov(lm(y ~ factor1 + factor2 + factor1:factor2))))
  }
  
}


# EM Means Modal for Two Way ANOVA ---------------------------------------------
# Pop-up window that shows options for calculating estimated marginal means
# Arguments: Shiny args (input, output, session) and factors (list of factors in the ANOVA)
# ------------------------------------------------------------------------------
uniEMModal <- function(input, output, session, factors) {
  ns <- session$ns
  interaction <- paste(factors, collapse = "*")
  listOfVars <- c("OVERALL", factors[1], factors[2], interaction)
  modalDialog (
    title = "Univariate: Estimated Marginal Means",
    bucket_list(
      header = NULL,
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Factor(s) and Factor Interactions:",
        labels = listOfVars,
        input_id = ns("EMfactors")),
      add_rank_list(
        text = "Display Means for: ",
        labels = NULL,
        input_id = ns("EMVars")
      )),
    checkboxGroupInput(ns("cme"), label = NULL, c("Compare main effects", "Compare simple main effects")),
    selectInput(ns("ciadj"), label = "Confidence interval adjustment", choices = c("LSD(none)" = 1, "Bonferroni" = 2), selected = 1),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"), "Continue"))
  )
}


# EM Means Calculations for Two Way ANOVA --------------------------------------
# Calculates the estimated marginal means
# Arguments: vars (list of variables/interactions), ciAdj (1 or 2), model (lm)
# ------------------------------------------------------------------------------
uniEMCalc <- function(vars, ciAdj, model, col2, col3) {
  if (ciAdj == 1) {
    ci = "lsd"
  } else {
    ci = "bonf"
  }
  if (length(vars) > 3) {
    return(emmeans(model, specs = pairwise ~ col2:col3))
  }
}

