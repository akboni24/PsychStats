library(dplyr)
library(shiny)
library(car)
library(sortable)
library(ggpubr)
library(emmeans)

# This file contains all of my helper functions and modals for the ANOVA pages

# Post Hoc Modal for ANOVA -----------------------------------------------------
anovaPostHocModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) for multiple comparisons options
  #' Arguments: Shiny arguments input, output, and session
  # ----------------------------------------------------------------------------
  ns <- session$ns
  modalDialog (
    title = "ANOVA: Post Hoc Multiple Comparisons",
    radioButtons(ns("eva"), label = "Equal Variances Assumed", 
                       c("LSD", "Bonferroni", "Tukey's HSD")),
    footer = tagList(modalButton("Cancel"), actionButton(ns("continue"), 
                                                         "Continue"))
  )
}

# Options Modal for ANOVA ------------------------------------------------------
anovaOptionsModal <- function(input, output, session) {
  #' Creates a modal (pop-up window) for optional statistics to be calculated
  #' Arguments: Shiny arguments input, output, and session
  # ------------------------------------------------------------------------------
  ns <- session$ns
  modalDialog (
    title = "ANOVA: Options",
    checkboxGroupInput(ns("stat"), label = "Statistics", c("Descriptives", 
                                "Homogeneity of variance test", "Welch test")),
    radioButtons(ns("mv"), label = "Missing Values", 
                 choices = list("Exclude cases analysis by analysis" = 1, 
                                "Exclude cases listwise" = 2), selected = 1),
    numericInput(ns("confint"), label = "Confidence Intervals (Level%)", 
                                                                value = "0.95"),
    footer = tagList(modalButton("Cancel"), actionButton(ns("continue"), 
                                                                    "Continue"))
  )
}


# Post Hoc Calculations Function for ANOVA -------------------------------------
postHocCalc <- function(tests, var1, var2, conflvl) {
  #' Calculates post hoc tests for an aov object
  #' Arguments: tests (list: tests), var 1 & var2 (str: variables for the anova), 
  #' and conflvl (float: a confidence level)
  # ------------------------------------------------------------------------------
  if ("Bonferroni" %in% tests) {
    return(pairwise.t.test(var1, var2, p.adj = "bonf"))
  } else if ("LSD" %in% tests) {
    return(pairwise.t.test(var1, var2, p.adj = "lsd", conf.level = conflvl))
  } else if ("Tukey's HSD" %in% tests) {
    return(TukeyHSD(aov(var1 ~ var2)))
  }
  
}

# Options Calculations for ANOVA -----------------------------------------------
anovaOptionsCalc <- function(tests, formula, var1, var2, var3 = NULL) {
  #' Calculates the optional statistics for an aov object
  #' Arguments: tests (list: tests), formula (str: formula for anova)
  # ----------------------------------------------------------------------------
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
uniPlotsModal <- function(input, output, session, vars) {
  #' Pop-up window for making plots for two-way ANOVA
  #' Arguments: Shiny arguments (input, output, session) and vars 
  #' (list: variables to display)
  # ------------------------------------------------------------------------------
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
      radioButtons(ns("type"), label="Chart Type", choices = c("Line Chart", 
                                                               "Bar Chart"))),
    fluidRow(
      checkboxInput(ns("errorBars"), "Include Error bars"),
      radioButtons(ns("ebOptions"), label=NULL, c("Confidence Interval (95.0%)", 
                                                  "Standard Error"))),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"),
                                                                 "Continue"))
  )
}

# Plotting function for Univariate Page ----------------------------------------
uniMakePlot <- function(df, x, group, dep, type = "Line Chart", errorBars) {
  #' Creates a two-way interaction plot
  #' Arguments: df (dataframe), x (variable on x-axis), group 
  #' (variable for separate lines), dep (dependent variable),
  #' type (type of plot), and eb (Error bars: either "None", "Confint", or "SE)
  # ------------------------------------------------------------------------------
  if ("Bar Chart" %in% type) {
    return(ggbarplot(df, x, dep, fill = group, color = group, palette = "Paired", 
                label = TRUE, position = position_dodge(0.9), add = errorBars))
  } else {
    return(ggline(df, x, y = dep, color = group, add = errorBars))
  }
}

# Post Hoc Multiple Comparisons Modal for Univariate Page ----------------------
uniPostHocModal <- function(input, output, session, factors) {
  #' Creates a pop-up window for multiple comparisons
  #' Arguments: Shiny args (input, output, session) and factors (list of factors)
  # ------------------------------------------------------------------------------
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
    radioButtons(ns("eva"), label = "Equal Variances Assumed", 
                       c("LSD", "Bonferroni", "Tukey's HSD")),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"),
                                                                 "Continue"))
  )
}

# Post Hoc Calculations for Two Way ANOVA --------------------------------------
uniPostHocCalc <- function(tests, y, factor1, factor2) {
  #' Calculates post hoc tests for a two way anova
  #' Arguments: tests (list of tests), y (dependent variable), 
  #' vars (factors for the anova), and anova (anova object)
  # ----------------------------------------------------------------------------
  results <- list()
  if ("Bonferroni" %in% tests) {
    append(results, pairwise.t.test(y, factor1, p.adj = "bonf"))
    append(results, pairwise.t.test(y, factor2, p.adj = "bonf"))
  } else if ("Tukey's HSD" %in% tests) {
    return(TukeyHSD(aov(lm(y ~ factor1 + factor2 + factor1:factor2))))
  } else {
    append(results, pairwise.t.test(y, factor1, p.adj = "none"))
    append(results, pairwise.t.test(y, factor2, p.adj = "none"))
  }
  results
  
}


# EM Means Modal for Two Way ANOVA ---------------------------------------------
uniEMModal <- function(input, output, session, factors) {
  #' Pop-up window that shows options for calculating estimated marginal means
  #' Arguments: Shiny args (input, output, session) and factors 
  #' (list of factors in the ANOVA)
  # ----------------------------------------------------------------------------
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
    checkboxGroupInput(ns("cme"), label = NULL, c("Compare main effects", 
                                                "Compare simple main effects")),
    selectInput(ns("ciadj"), label = "Confidence interval adjustment", 
                choices = c("LSD(none)" = 1, "Bonferroni" = 2), selected = 1),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"), 
                                                                 "Continue"))
  )
}


# EM Means Calculations for Two Way ANOVA --------------------------------------
uniEMCalc <- function(vars, ciAdj, model, col2, col3) {
  # Calculates the estimated marginal means
  # Arguments: vars (list of variables/interactions), ciAdj (1 or 2), model (lm)
  # ----------------------------------------------------------------------------
  if (ciAdj == 1) {
    ci = "lsd"
  } else {
    ci = "bonf"
  }
  if (length(vars) > 3) {
    return(emmeans(model, specs = pairwise ~ col2:col3))
  }
}

# Helper Functions for Test for Simple Effects ---------------------------------
anova_se <- function(anova_data, factor, dep) {
  dependent_var <- anova_data %>% pull(dep)
  other_factor <- as.factor(anova_data %>% pull(factor))
  y <- lm(dependent_var ~ sefactor, data=anova_data)
  summary(aov(y))
}

test_simple_effects <- function(anova_data, x, y, sefactor) {
  # Calculates a test of simple effects for Two Way ANOVA
  # Arguments: anova_data (df), x (factor for anova), dep (dependent variable name),
  # sefactor (factor to split data on)
  # ------------------------------------------------------------------------------
  se_factor <- as.factor(anova_data %>% pull(sefactor))
  dfs <- split(anova_data, sefactor)
  se_results <- lapply(dfs, anova_se, factor=x, dep=y)
}
  
