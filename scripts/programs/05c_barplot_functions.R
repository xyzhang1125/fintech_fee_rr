#--------------------------------------------------------------------------------------------
# File name: 		      05c_barplot_functions.R
# Creation date:      2023-01-31
# Author:          		César Landín
# Files used:
#   - (None)
# Files created:
#   - (None)
# Purpose:
# 	- Define functions for plotting survey bar plots.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(assertthat)
library(rcartocolor)
library(scales)
source(here("scripts", "programs", "set_theme.R"))
group_colors <- read_csv(here("proc", "colors.csv"))
#########################################################

###############################################
##    (1): Define data processing functions.  #
###############################################
# (1.1): Get an exact string match.
str_detect_numeric <- function(string, number) {
  if (string %in% c("-777", "-666")) {
    ifelse(string == as.character(number), 1, 0)
  } else {
    ((str_replace_all(string, " ", "x") %>% 
      str_split("x") %>% 
      .[[1]] %>% 
       as.numeric()) == number) %>% 
      sum() %>% 
      {ifelse(. >= 1, 1, 0)}
  }
}

# (1.2): Generate dummy response variables.
gen_ind_response_dummies <- function(df, resp, var_name) {
   df %>% 
    rowwise() %>% 
    mutate(!!str_c("r_", current_mapping %>% filter(response == resp) %>% pull(response_var)) := 
             str_detect_numeric(eval(as.name(var_name)), 
                                current_mapping %>% 
                                  filter(response == resp) %>% 
                                  pull(all_of(var_name))))
}
gen_all_response_dummies <- function(df, mapping, var) {
  current_responses <- mapping %>% pull(response)
  for (r in current_responses) {
    df %<>% gen_ind_response_dummies(r, var)
  }
  return(df)
}

# (1.3): Reshape data for graph.
proc_graph_data <- function(df, var_name) {
  df %>% 
    select(starts_with("r_")) %>% 
    ungroup() %>% 
    summarise_all(sum, na.rm = TRUE) %>% 
    pivot_longer(cols = everything(), names_to = "response_var", values_to = "n") %>%
    mutate(coef = n / nrow(df)) %>%
    left_join(current_mapping %>%
                mutate(response_var = str_c("r_", response_var)),
              by = "response_var") %>% 
    mutate(lci = 0,
           uci = 0) %>%
    select(response, n, coef, lci, uci) %>%
    rename(term = response) %>%
    arrange(coef) %>% 
    mutate(term = factor(term, levels = term))
}

# (1.4): Relevel data (change denominator to exclude omitted categories).
relevel_data <- function(df, var, ...) {
  current_data <- eval(as.name(str_c("data_", var)))
  df %>% 
    mutate(coef = n / (nrow(current_data) - get_n_omitted(var, data = current_data, ...)))
}

# (1.5): Replace exact number.
str_replace_exact <- function(string, pattern, replacement) {
  # Paste "x" next to each number
  str_replace_all(string, " ", "x") %>% 
    str_c("x", .) %>% 
    str_replace_all(str_c("x", pattern), 
                    str_c("x", replacement)) %>% 
    str_replace_all("x", " ") %>% 
    str_trim()
}

# (1.6): Define function for getting coefficient and confidence intervals from combinations of variables.
get_coef_cis <- function(model, var1, var2) {
  df <- tibble(vars = names(model$coefficients),
               coef = model$coefficients,
               se = model$se)
  df %<>%
    summarise(coef = sum(coef), se = sum(se) - se[1]) %>% 
    mutate(term = var2) %>% 
    bind_rows(df %>%
                 filter(vars == ifelse("(Intercept)" %in% df$vars, "(Intercept)", var1)) %>%
                 select(-vars) %>%
                 mutate(term = var1)) %>%
    mutate(se = ifelse(term == var1, NA, se),
           lci = coef - 1.96 * se,
           uci = coef + 1.96 * se,
           term = factor(term, levels = c(var1, var2)))
  return(df)
}

########################################
##    (2): Define graphing functions.  #
########################################
# (2.1): Graph barplot (same as used in script 22).
graph_barplot <- function(data, str_width = 27) {
  ngroups <- nrow(data)
  # Define current colors
  if (ngroups == 6) {
    current_cols <- group_colors %>% 
      filter(str_detect(group, "anticipated|deadline")) %>% 
      arrange(group) %>% 
      mutate(col2 = ifelse(str_detect(group, "deadline"), NA, col2)) %>% 
      pivot_longer(cols = col1:col2, values_to = "col") %>% 
      filter(!is.na(col)) %>% 
      mutate(order = c(3, 6, 4, 1, 2, 5)) %>% 
      arrange(-order) %>% 
      pull(col)
  } else if (ngroups <= 2) {
    current_cols <- tibble(group = data$term) %>% 
      mutate(group = str_replace(group, "_", " ") %>% str_to_lower()) %>% 
      inner_join(group_colors, by = "group")
    if (nrow(current_cols) == ngroups) {
      current_cols %<>% pull(col1) %>% rev()
    } else {
      current_cols %<>% select(-group) %>% as.vector()
      names(current_cols) <- data$term
    }
  } else {
    current_cols <- rev(c(carto_pal(12, "Safe"), carto_pal(12, "Safe"))[1:ngroups])
  }
  # Generate graph
  g1 <- ggplot(data = data,
               aes(x = term, y = coef, fill = term)) +
    geom_col() +
    set_theme(size = 17) +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = str_width), expand = expansion(add = 0.5)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = current_cols) + 
    theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
          plot.margin = margin(0, 0, 0, 0))
  if (max(data$uci, na.rm = TRUE) > 0) {
    g1 <- g1 + geom_errorbar(aes(ymin = lci, ymax = uci, width = .2))
  }
  return(g1)
}

