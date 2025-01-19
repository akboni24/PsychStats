library(dplyr)
library(shiny)
library(car)
library(sortable)
library(ggpubr)
library(emmeans)
library(lsr)
library(sciplot)
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
                 c("None", "LSD", "Bonferroni", "Tukey's HSD")),
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
    numericInput(ns("confint"), label = "Confidence Intervals (Level%)", 
                 value = "0.95", min=0.01, max=0.99, step=0.01),
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
  } else if ("Tukey's HSD" %in% tests) {
    return(TukeyHSD(aov(var1 ~ var2)))
  } else {
    return(pairwise.t.test(var1, var2, p.adj = "none", conf.level = conflvl))
  }
  
}


descr_helper <- function(dep, factor, func) {
  tapply(dep, factor, func)
}

stderror <- function(x) sd(x)/sqrt(length(x))


two_way_anovaDescriptives <- function(data, dep_name, var1, var2) {
  factor <- as.factor(data %>% pull(var1))
  dfs <- split(data, factor)
  final_dfs <- lapply(dfs, FUN=anovaDescriptives, dep_name, var2)
}

anovaDescriptives <- function(data, dep_name, vars_name) {
  
  dep <- data %>% pull(dep_name)
  vars <- as.factor(data %>% pull(vars_name))
  
  Factor.Levels <- levels(vars)
  Factor.Levels <- append(Factor.Levels, c("Total"))
  
  N <- descr_helper(dep, vars, length)
  N <- append(N, c(length(vars)))
  
  Mean <- descr_helper(dep, vars, mean)
  Mean <- append(Mean, c(mean(dep)))
  
  Std.Dev <- descr_helper(dep, vars, sd)
  Std.Dev <- append(Std.Dev, c(sd(dep)))
  
  Std.Error <- descr_helper(dep, vars, stderror)
  Std.Error <- append(Std.Error, c(stderror(dep)))
  
  df <- data.frame(Factor.Levels, N, Mean, Std.Dev, Std.Error)
  names(df)[1] <- vars_name
  
  return(df)
  
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
    title = "Profile Plots",
    fluidRow(
      bucket_list(
        header = NULL,
        group_name = "plots",
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
      radioButtons(ns("ebOptions"), label=NULL, choices = c("Confidence Interval (95.0%)" = 1, "Standard Error" = 2), selected = 1)),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"),
                                                                 "Continue"))
  )
}

# Plotting function for Univariate Page ----------------------------------------
uniMakePlot <- function(data, x, group, dep, x_name, dep_name, group_name, errorBars, type = "Line Chart") {
  #' Creates a two-way interaction plot
  #' Arguments: df (dataframe), x (variable on x-axis), group 
  #' (variable for separate lines), dep (dependent variable),
  #' type (type of plot), and eb (Error bars: either "None", "Confint", or "SE")
  # ----------------------------------------------------------------------------
  # x_groups <- as.factor(data %>% pull(x))
  # sep_lines <- as.factor(data %>% pull(group))
  # dep <- data %>% pull(dep_name)
  # summary <- data %>% group_by(c(x_groups), c(sep_lines)) %>% 
  #                     dplyr::summarise(means = mean(dep),
  #                                        sd = sd(dep),
  #                                        se = sd(dep)/sqrt(length(dep)),
  #                                        lower = ci_bound_lower(dep),
  #                                        upper = ci_bound_upper(dep))
  # print(summary)
  if ("Bar Chart" %in% type) {
    plot <- ggplot(data, aes(x=x, y=dep)) +
              stat_summary(geom = "col", fun = "mean", size = 3, position = position_dodge(0.9),
                           aes(fill=group, colour=group))
    
    if ("mean_ci" %in% errorBars) {
      plot <- plot + stat_summary(fun.data = mean_sdl, geom = "errorbar", width=0.2,
                                  aes(group=group), position = position_dodge(0.9))
    } else if ("mean_se" %in% errorBars) {
      plot <- plot + stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2,
                                  aes(group=group), position = position_dodge(0.9))
    }
            
    # return(ggbarplot(df, x, dep, fill = group, color = group, palette = "Paired", 
    #                  label = TRUE, position = position_dodge(0.9), add = errorBars))
  } else {
    plot <- ggplot(data, aes(x=x, y=dep)) +
                  stat_summary(geom = "point", fun = "mean", size = 5,
                               aes(group=group, colour=group))+
                  stat_summary(geom = "line", fun = "mean", size = 1,
                               aes(group=group, colour=group))
    
    if ("mean_ci" %in% errorBars) {
      plot <- plot + stat_summary(fun.data = mean_sdl, geom = "errorbar", width=0.2,
                                  aes(group=group, colour=group))
    } else if ("mean_se" %in% errorBars) {
      plot <- plot + stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2,
                                  aes(group=group, colour=group))
    }
                
  }
  
  
  
  plot <- plot +
            xlab(x_name) +
            ylab(dep_name) +
            guides(color = guide_legend(override.aes=list(shape = 20))) +
            labs(color=group_name)
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
  
  return(plot)
}

# Post Hoc Multiple Comparisons Modal for Univariate Page ----------------------
uniPostHocModal <- function(input, output, session, factors) {
  #' Creates a pop-up window for multiple comparisons
  #' Arguments: Shiny args (input, output, session) and factors (list of factors)
  # ------------------------------------------------------------------------------
  ns <- session$ns
  modalDialog (
    title = "Post Hoc Multiple Comparisons for Observed Means",
    bucket_list(
      header = NULL,
      group_name = "posthoc",
      orientation = "vertical",
      add_rank_list(
        text = "Factor(s):",
        labels = factors,
        input_id = ns("factors"),
        options = sortable_options(group="factors", put="phvars")),
      add_rank_list(
        text = "Post Hoc Tests for: ",
        labels = NULL,
        input_id = ns("postHocVars"),
        options = sortable_options(group="phvars")
      )),
    radioButtons(ns("eva"), label = "Equal Variances Assumed", 
                 c("LSD", "Bonferroni", "Tukey's HSD")),
    footer = tagList(modalButton(label = "Cancel"), actionButton(ns("continue"),
                                                                 "Continue"))
  )
}


# Post Hoc Calculations for Two Way ANOVA --------------------------------------
uniPostHocCalc <- function(tests, y, factor1, factor2, conflvl) {
  #' Calculates post hoc tests for a two way anova
  #' Arguments: tests (list of tests), y (dependent variable), 
  #' vars (factors for the anova), and anova (anova object)
  # ----------------------------------------------------------------------------
  factors <- list(factor1, factor2)
  results <- lapply(factors, postHocCalc, tests=tests, var1=y, conflvl=conflvl)
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
    title = "Estimated Marginal Means",
    bucket_list(
      header = NULL,
      group_name = "emmeans",
      orientation = "vertical",
      add_rank_list(
        text = "Factor(s) and Factor Interactions:",
        labels = listOfVars,
        input_id = ns("EMfactors"),
        options = sortable_options(group="vars", put="em")),
      add_rank_list(
        text = "Display Means for: ",
        labels = NULL,
        input_id = ns("EMVars"),
        options = sortable_options(group="em")
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


# Functions for One Way Within Subjects ANOVA ----------------------------------
one_way_data <- function(data, within, name, vars) {
  #' Converts data from wide format to long format for one way within ANOVA
  #' 
  #' Parameters:
  #' -----------
  #' data. data frame in wide format
  #' within. list of the levels of the within subj factors (columns of data)
  #' name. character vector representing the name of the within subj factor
  #' vars. list of all variables needed for analysis
  #' 
  #' Returns:
  #' --------
  #' data_prepared. data frame in long format
  #' ---------------------------------------------------------------------------
  new_data <- data %>% dplyr::select(one_of(vars))
  # gather all levels of within subj variable into one variable
  data_prepared <- new_data %>%
    gather(key=key, value="dependent_var", within) %>%
    convert_as_factor(key)
  
  # make sure name of the variable is what user entered
  names(data_prepared)[names(data_prepared) == 'key'] <- name
  
  data_prepared
}

one_way_within <- function(data_prepared, effectSizes, w_name, table=FALSE) {
  #' Conduct a one way within subjects ANOVA
  #' 
  #' Parameters:
  #' -----------
  #' data_prepared. data frame in long format
  #' effectSizes. boolean representing whether user want effect sizes calculated
  #' w_name. character vector representing name of within subj factor
  #' 
  #' Returns:
  #' --------
  #' aov object
  #' ---------------------------------------------------------------------------
  if (effectSizes == TRUE && table == FALSE) {
    anovaResults <- aov_ez(colnames(data_prepared)[1], "dependent_var", 
                           within = w_name, data=data_prepared, es="pes")
  } else {
    anovaResults <- aov_ez(colnames(data_prepared)[1], "dependent_var", 
                           within = w_name, data=data_prepared)
  }
  
  anovaResults
  
}


two_way_data <- function(data, order, factor1, num_lvls1, factor2, num_lvls2) {
  #' Converts data from wide format to long format for two way within ANOVA
  #' 
  #' Parameters:
  #' -----------
  #' data: data frame.
  #' order: list.
  #'    order that the user put the variables in
  #' factor1: str.
  #'    name of the first within subjects factor
  #' num_lvls1: int.
  #'    number of levels that the first factor has
  #' factor2: str.
  #'    name of the second within subjects factor
  #' num_lvls2: int.
  #'    number of levels that the second factor has
  #'    
  #' Returns:
  #' ---------
  #' long_data: dataframe
  #'    data converted into long format
  # ----------------------------------------------------------------------------
  p_id <- colnames(data)[1]
  col_order <- c(p_id, order)
  data <- data[, col_order]
  key <- paste(factor1, factor2, sep="_")
  num_p <- nrow(data)
  num_times <- num_lvls1 * num_lvls2
  long_data <- data %>% gather(key=key, value="dependent_var", 
                               -p_id) %>%
    separate(col=key,
             into = c(factor1, factor2),
             sep = -1) %>%
    arrange(p_id, factor1, factor2)
  
  f1 <- rep(0:(num_lvls1 - 1), times = 1, each = num_p * num_lvls2)
  f2 <- rep(0:(num_lvls2 - 1), times = num_lvls1, each = num_p)
  
  long_data[, 2] = as.factor(f1)
  long_data[, 3] = as.factor(f2)
  
  return(long_data)
}

data_check <- function(order, factor1, num_lvls1, factor2, num_lvls2) {
  #' Displays a table of how the user mapped the levels of their within subjects
  #' factors for two way within subjects ANOVA
  #' 
  #' Parameters:
  #' ------------
  #' order: list.
  #'    order that the user put the variables in
  #' factor1: str.
  #'    name of the first within subjects factor
  #' num_lvls1: int.
  #'    number of levels that the first factor has
  #' factor2: str.
  #'    name of the second within subjects factor
  #' num_lvls2: int.
  #'    number of levels that the second factor has
  #'    
  #' Returns:
  #' ---------
  #' data frame.
  #' ---------------------------------------------------------------------------
  Factor1 <- rep(0:(num_lvls1 - 1), times = 1, each = num_lvls2)
  Factor2 <- rep(0:(num_lvls2 - 1), times = num_lvls1)
  
  data <- data.frame(Factor1, Factor2, order)
  colnames(data) <- c(factor1, factor2, "Your.Input")
  data
}




two_way_posthoc <- function(data, dep, vars, eva) {
  #' Function for conducting post hoc tests for two way ANOVA
  #' 
  #' Arguments:
  #' ----------
  #' data: df. Data for the ANOVA
  #' dep: str. Name of the dependent variable
  #' vars: list. List of variables to conduct post hoc tests on
  #' eva: str. Either "Bonferroni", "LSD", or "Tukey's HSD"
  #' 
  #' Returns:
  #' ---------
  #' posthoc. Object of class "pairwise.htest" or tibble data frame for HSD
  # ------------------------------------------------------------------------------
  dep_var <- data %>% pull(dep)
  if (length(vars) > 1) {
    
    var1 <- data %>% pull(vars[1])
    var2 <- data %>% pull(vars[2])
    posthoc <- uniPostHocCalc(eva, dep_var, var1, var2, 0.95)
    
  } else {
    
    var1 <- data %>% pull(vars)
    posthoc <- postHocCalc(eva, dep_var, var1, 0.95)
    
  } 
  
  posthoc
}

ci_bound_lower <- function(var) {
  #' Calculates lower bound of a 95% confidence interval
  #' 
  #' Parameters:
  #' -----------
  #' var. data variable
  #' 
  #' Returns:
  #' --------
  #' float
  mean <- mean(var)
  se <- stderror(var)
  ci <- mean - (1.96*se)
  ci
}

ci_bound_upper <- function(var) {
  #' Calculates upper bound of a 95% confidence interval
  #' 
  #' Parameters:
  #' -----------
  #' var. data variable
  #' 
  #' Returns:
  #' --------
  #' float
  mean <- mean(var)
  se <- stderror(var)
  ci <- mean + (1.96*se)
  ci
}

emmeans_descr <- function(data, dep_name) {
  #' Calculates overall grand mean summary statistics
  #' 
  #' Parameters:
  #' -----------
  #' data. data frame in long format
  #' dep_name. character vector, name of dependent variable
  #' 
  #' Returns:
  #' --------
  #' data frame of grand mean summary statistics
  #' ---------------------------------------------------------------------------
  dep <- data %>% pull(dep_name)
  Mean <- mean(dep)
  Std.Error <- stderror(dep)
  Lower.Bound <- Mean - (1.96*Std.Error)
  Upper.Bound <- Mean + (1.96*Std.Error)
  df <- data.frame(Mean, Std.Error, Lower.Bound, Upper.Bound)
  df
  
}


levenes_helper <- function(data, dep, group) {
  #' Helper function for mixed_levenes
  #' 
  #' Parameters:
  #' -----------
  #' data. data frame in long format
  #' dep. name of dependent variable
  #' group. name of grouping variable
  #' 
  #' Returns:
  #' --------
  #' leveneTest object
  #' ---------------------------------------------------------------------------
  dependent_var <- data %>% pull(dep)
  grouping <- data %>% pull(group) %>% as.factor()
  result <- leveneTest(y=dependent_var, group=grouping, center=mean)
  result
}

mixed_levenes <- function(data, dep_name, between, within) {
  #' Conducts levenes test for homogeneity of variance on a dataframe split by 
  #' the levels of the within subjects factor for mixed ANOVA
  #' 
  #' Parameters:
  #' -----------
  #' data. data frame in long format
  #' dep_name. name of dependent variable
  #' between. name of between subjects factor
  #' within. name of within subjects factor
  #' 
  #' Returns:
  #' --------
  #' list of leveneTest objects
  #' ---------------------------------------------------------------------------
  factor <- data %>% pull(within) %>% as.factor()
  dfs <- split(data, factor)
  levenes <- lapply(dfs, FUN=levenes_helper, dep=dep_name, group=between)
  levenes
}
