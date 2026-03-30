#--------------------------------------------------------------------------------------------
# File name: 		      07b_balance_table_functions.R
# Creation date:      2022-01-07
# Author:          		César Landín
# Files used:
# 	- (none)
# Files created:
#   - (none)
# Purpose:
# 	- Define parameters and functions for balance tables.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(lubridate)
library(fastDummies)
source(here("scripts", "programs", "table_functions.R"))
#########################################################

#############################################################
##    (1): Define balance table parameters and functions.  ##
#############################################################
# (1.1): Set table parameters.
# Vector with outcomes.
# balance_outcomes <- c("owner_sex_female",
#                       "owner_age",
#                       "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals",
#                       "new_buss_type_restaurants", "new_buss_type_small_retailers", "new_buss_type_other",
#                       "months_since_ft", 
#                       "make_sale", 
#                       "log_valid_volume_w5", "log_nr_valid_payments_w5")

# months_since_ft should also be winsorized 
balance_outcomes <- c("owner_sex_female",
                      "owner_age", "new_buss_type_small_retailers", "new_buss_type_professionals",
                      "new_buss_type_beauty", "new_buss_type_clothing",
                      "new_buss_type_restaurants", "new_buss_type_other",
                      "months_since_ft", 
                      "make_sale", 
                      "log_valid_volume_w5", "log_nr_valid_payments_w5")
# Variable names.
# balance_outcomes_varnames <- tibble(shortname = balance_outcomes,
#                                     longname = c("Female", 
#                                                  "Age", 
#                                                  "\\quad Beauty", "\\quad Clothing", "\\quad Professionals", 
#                                                  "\\quad Restaurants", "\\quad Small retailers", "\\quad Other",
#                                                  "\\quad Months since first transaction",
#                                                  "\\quad \\% months business made sales",
#                                                  "\\quad Log monthly card sales volume", 
#                                                  "\\quad Log monthly card transactions"))

balance_outcomes_varnames <- tibble(shortname = balance_outcomes,
                                    longname = c("Female", 
                                                 "Age", 
                                                 "\\quad Small retailers", "\\quad Professionals", 
                                                 "\\quad Beauty", 
                                                 "\\quad Clothing", 
                                                 "\\quad Restaurants", "\\quad Other",
                                                 "\\quad Months since first transaction",
                                                 "\\quad \\% months business made sales",
                                                 "\\quad Log monthly card sales volume", 
                                                 "\\quad Log monthly card transactions"))

# (1.2): Basic regression function.
reg_basic <- function(data, var = "", model, wt = 1) {
  if (var != "") {
    model <- paste(var, model)
  }
  reg <- feols(fml = as.formula(model),
               data = data,
               se = "hetero",
               weights = wt)
  return(reg)
}


# (1.3): Format balance table top and bottom rows.
comma_format_toprow <- function(df) {
  proc_df <- df %>% 
    mutate_all(~comma_format(., override_right_digits = 3))
  return(proc_df)
}

comma_format_bottomrow <- function(df, toprow) {
  proc_df <- df
  right_digits <- 3 - floor(log10(abs(as.numeric(str_replace(toprow[1], fixed(","), "")))))
  if (right_digits < 0) {right_digits <- 0}
  if (right_digits > 3) {right_digits <- 3}
  proc_df %<>%
    mutate_at(vars(colnames(proc_df)[!colnames(proc_df) %in% colnames(proc_df)[ncol(proc_df)]]), 
              ~comma_format(., override_right_digits = right_digits))
  if ("fstat" %in% colnames(proc_df)) {
    proc_df %<>%
      mutate_at(vars(colnames(proc_df)[colnames(proc_df) %in% colnames(proc_df)[ncol(proc_df)]]), 
                ~comma_format(., override_right_digits = 3))
  }
  return(proc_df)
}

# Note: Xinyu modified or removed previous section (1.4) - (1.8) to make it start with tabular

# (1.4): Function for defining significance stars
add_significance_stars <- function(variable, brow, trow) {
  bottom_var <- brow %>% select(all_of(variable)) %>% str_replace(fixed(","), "") %>% as.numeric()
  top_var <- trow %>% select(all_of(variable)) %>% str_replace(fixed(","), "") %>%  as.numeric()
  
  if (variable == "fstat") {
    # For F-stat, don't add stars to the value
    trow %>% 
      select(all_of(variable))
  } else {
    trow %>% 
      mutate(abs_div = abs(top_var / bottom_var),
             !!variable := ifelse(abs_div >= 2.576, str_c("$", eval(as.name(variable)), "^{***}", "$"),
                                  ifelse(abs_div >= 2.326, str_c("$", eval(as.name(variable)), "^{**}", "$"),
                                         ifelse(abs_div >= 1.645,  str_c("$", eval(as.name(variable)), "^{*}", "$"),  
                                                str_c("$", eval(as.name(variable)), "$"))))) %>% 
      select(all_of(variable))
  }
}

# (1.10): Function for generating dataframe rows with variable name, treatment coefficients and f-stats
balance_table_treat_row_fastdl <- function(outcome, data, remove_2.75 = FALSE) {
  # Define model depending on whether 2.75 fee is included or not. (This is from previous version)
  mod <- case_when(!remove_2.75 ~ "~ anticipated_reminder + unanticipated_reminder + fast_deadline + deadline + fee_2.75",
                   remove_2.75 ~ "~ anticipated_reminder + unanticipated_reminder + deadline")
  # Run regression
  mod <- reg_basic(data = data,
                   var = outcome,
                   model = mod)
  
  # coefficients
  top_row <- tribble(~intercept, ~unant, ~ant, ~dl, ~fsdl, ~fee, ~fstat,
                     mod[["coefficients"]][["(Intercept)"]],
                     mod[["coefficients"]][["unanticipated_reminder"]],
                     mod[["coefficients"]][["anticipated_reminder"]],
                     mod[["coefficients"]][["deadline"]],
                     mod[["coefficients"]][["fast_deadline"]],
                     mod[["coefficients"]][["fee_2.75"]],
                     fitstat(mod, "f")[["f"]][["stat"]])
  
  # std. errors and p-value for F-stat
  bottom_row <- tribble(~intercept, ~unant, ~ant, ~dl, ~fsdl, ~fee, ~fstat,
                        mod[["coeftable"]]["(Intercept)", "Std. Error"],
                        mod[["coeftable"]]["unanticipated_reminder", "Std. Error"],
                        mod[["coeftable"]]["anticipated_reminder", "Std. Error"],
                        mod[["coeftable"]]["deadline", "Std. Error"],
                        mod[["coeftable"]]["fast_deadline", "Std. Error"],
                        mod[["coeftable"]]["fee_2.75", "Std. Error"],
                        fitstat(mod, "f")[["f"]][["p"]])
  
  # Modify format depending on first column (intercept), which gives variable magnitude
  top_row <- comma_format_toprow(top_row)
  bottom_row <- comma_format_bottomrow(bottom_row, top_row)
  
  # Add significance stars
  top_row <- lapply(colnames(top_row), add_significance_stars, brow = bottom_row, trow = top_row) %>% bind_cols()
  
  # Format bottom row with parentheses for SE and special format for F-stat p-value
  bottom_row %<>%
    mutate_at(vars(-fstat), ~str_c("(", .,  ")")) %>%
    mutate(fstat = str_c("{}[", fstat, "]"))
  
  # Output
  current_longname <- balance_outcomes_varnames %>% filter(shortname == str_replace(outcome, "trust_scale_[0-9]+_", "")) %>% pull(longname)
  varname_tibble <- tribble(~varname, current_longname, "")
  table_row <- cbind(varname_tibble, rbind(top_row, bottom_row))
  return(table_row)
}

# (1.11): Wide balance table processing - only tabular contents
balance_table_treat_proc_fastdl <- function(table) {
  num_coef_cols <- ncol(table) - 1
  
  # including fee 2.75
  header1 <- "& Intercept & Unannounced & Announced & Deadline & Same-day & 2.75\\% Fee & Joint test \\\\"
  header2 <- "& & reminder & reminder & & deadline & & \\textit{F}-stat \\\\"
  header3 <- "& (1) & (2) & (3) & (4) & (5) & (6) & (7)\\\\"
  
  # generate the basic table
  proc_table <- kable(table, 
                      format = "latex", 
                      align = c("l", rep("c", num_coef_cols)),
                      col.names = NULL, 
                      booktabs = TRUE, 
                      line = FALSE, 
                      escape = FALSE)
  
  table_string <- as.character(proc_table)
  
  # header
  table_string <- str_replace(table_string, 
                              pattern = fixed("\\toprule"), 
                              replacement = paste("\\toprule\n", 
                                                  header1, "\n",
                                                  header2, "\n",
                                                  header3, "\n",
                                                  "\\midrule"))
  
  # remove unnecessary parts
  table_string <- str_replace_all(table_string, pattern = fixed("\nFALSE\n"), replacement = "\n")
  table_string <- str_replace_all(table_string, pattern = fixed("\nTRUE\n"), replacement = "")
  table_string <- str_replace_all(table_string, pattern = fixed("\\begin{table}[H]\n"), replacement = "")
  table_string <- str_replace_all(table_string, pattern = fixed("\n\\end{table}"), replacement = "")
  
  # panel titles
  # table_string <- fix_panel_titles_correct(table_string)
  
  return(table_string)
}

# (1.12) add panel titles
fix_panel_titles_correct <- function(table_string) {
  proc_table <- table_string
  # panel A
  if (str_detect(proc_table, fixed("\\midrule"))) {
    proc_table <- str_replace(
      proc_table,
      fixed("\\midrule"),
      "\\midrule\n\\underline{Panel A: Manager characteristics} \\\\"
    )
  }
  # panel B
  if (str_detect(proc_table, fixed("\\quad Small retailers &"))) {
    proc_table <- str_replace(
      proc_table,
      fixed("\\quad Small retailers &"),
      "\\underline{Panel B: Business characteristics} \\\\\n\\emph{Business type} \\\\\n\\quad Small retailers &"
    )
  }
  # panel C
  if (str_detect(proc_table, fixed("\\quad Months since first transaction &"))) {
    proc_table <- str_replace(
      proc_table,
      fixed("\\quad Months since first transaction &"),
      "\\emph{Pre-treatment sales variables} \\\\\n\\quad Months since first transaction &"
    )
  }
  return(proc_table)
}


# for the no fastdl in script 07d. fastdl is removed due to collinearity
balance_table_treat_row_no_fastdl <- function(outcome, data, remove_2.75 = FALSE) {
  # Define model depending on whether 2.75 fee is included or not.
  mod <- case_when(!remove_2.75 ~ "~ anticipated_reminder + unanticipated_reminder + deadline + fee_2.75",
                   remove_2.75 ~ "~ anticipated_reminder + unanticipated_reminder + deadline")
  
  # Run regression
  mod <- reg_basic(data = data,
                   var = outcome,
                   model = mod)
  
  # coefficients
  top_row <- tribble(~intercept, ~unant, ~ant, ~dl, ~fee, ~fstat,
                     mod[["coefficients"]][["(Intercept)"]],
                     mod[["coefficients"]][["unanticipated_reminder"]],
                     mod[["coefficients"]][["anticipated_reminder"]],
                     mod[["coefficients"]][["deadline"]],
                     mod[["coefficients"]][["fee_2.75"]],
                     fitstat(mod, "f")[["f"]][["stat"]])
  
  # std. errors and p-value for F-stat
  bottom_row <- tribble(~intercept, ~unant, ~ant, ~dl, ~fee, ~fstat,
                        mod[["coeftable"]]["(Intercept)", "Std. Error"],
                        mod[["coeftable"]]["unanticipated_reminder", "Std. Error"],
                        mod[["coeftable"]]["anticipated_reminder", "Std. Error"],
                        mod[["coeftable"]]["deadline", "Std. Error"],
                        mod[["coeftable"]]["fee_2.75", "Std. Error"],
                        fitstat(mod, "f")[["f"]][["p"]])
  
  # Remove 2.75 fee if needed (from previous version)
  if (remove_2.75) {
    top_row %<>% select(-fee)
    bottom_row %<>% select(-fee)
  }
  
  # Modify format depending on first column (intercept), which gives variable magnitude
  top_row <- comma_format_toprow(top_row)
  bottom_row <- comma_format_bottomrow(bottom_row, top_row)
  
  # Add significance stars
  top_row <- lapply(colnames(top_row), add_significance_stars, brow = bottom_row, trow = top_row) %>% bind_cols()
  
  # Format bottom row with parentheses for SE and special format for F-stat p-value
  bottom_row %<>%
    mutate_at(vars(-fstat), ~str_c("(", .,  ")")) %>%
    mutate(fstat = str_c("{}[", fstat, "]"))
  
  # Output
  current_longname <- balance_outcomes_varnames %>% filter(shortname == str_replace(outcome, "trust_scale_[0-9]+_", "")) %>% pull(longname)
  varname_tibble <- tribble(~varname, current_longname, "")
  table_row <- cbind(varname_tibble, rbind(top_row, bottom_row))
  return(table_row)
}

# no fastdl
balance_table_treat_proc_no_fastdl_custom <- function(table, remove_2.75 = FALSE) {
  build_latex_from_df <- function(df, has_fee = TRUE) {
    num_cols <- ncol(df)
    align <- paste0("l", paste(rep("c", num_cols - 1), collapse = ""))
    
    latex <- paste0("\\begin{tabular}{", align, "}\n")
    latex <- paste0(latex, "\\toprule\n")
    
    if (has_fee) {
      # with fee
      latex <- paste0(latex, "& Intercept & Unannounced & Announced & Deadline & 2.75\\% Fee & Joint test \\\\\n")
      latex <- paste0(latex, "& & reminder & reminder & & & \\textit{F}-stat \\\\\n")
      latex <- paste0(latex, "& (1) & (2) & (3) & (4) & (5) & (6)\\\\\n")
    } else {
      # without fee
      latex <- paste0(latex, "& Intercept & Unannounced & Announced & Deadline & Joint test \\\\\n")
      latex <- paste0(latex, "& & reminder & reminder & & \\textit{F}-stat \\\\\n")
      latex <- paste0(latex, "& (1) & (2) & (3) & (4) & (5)\\\\\n")
    }
    
    latex <- paste0(latex, "\\midrule\n")
    
    for (i in 1:nrow(df)) {
      row <- df[i, ]
      row_str <- paste(row, collapse = " & ")
      latex <- paste0(latex, row_str, " \\\\\n")
    }
    
    latex <- paste0(latex, "\\bottomrule\n")
    latex <- paste0(latex, "\\end{tabular}")
    
    return(latex)
  }
  
  # if want to use fee (to align for previous version)
  has_fee <- ifelse("fee" %in% colnames(table) || ncol(table) == 7, TRUE, FALSE)
  latex_table <- build_latex_from_df(table, has_fee = has_fee)
  # latex_table <- fix_panel_titles_correct(latex_table)
  
  return(latex_table)
}


# (1.13): Wide balance table processing, presentation version.
balance_table_treat_proc_pres <- function(table, label) {
  table[ncol(table)] <- table[2:nrow(table), ncol(table)] %>% 
    append(NA) %>% 
    str_replace_all(pattern = fixed("["), replacement = "") %>% 
    str_replace_all(pattern = fixed("]"), replacement = "")
  num_coef_cols <- ncol(table) - 1
  # Basic layout and headers
  proc_table <-  kable(table %>% filter(!str_detect(intercept, fixed("("))), 
                       col.names = NULL,
                       format = "latex", align = c("l", rep("c", num_coef_cols)),
                       booktabs = T, line = F, escape = F) %>%
    # Add table titles.
    str_replace(pattern = fixed("\\toprule"), 
                replacement = paste("\\toprule \n", 
                                    "& Intercept & Unannounced",
                                    "& Announced & Deadline & 2.75\\% Fee & \\textit{F}-stat \\\\ \n",
                                    "& & reminder & reminder & & & p-value \\\\ \n",
                                    "\\midrule \n")) %>% 
    # Fix final issues.
    str_replace_all(pattern = fixed("\nFALSE\n"), replacement = "\n") %>%
    str_replace_all(pattern = fixed("\nTRUE\n"), replacement = "") %>% 
    # Add panel titles.
    str_replace(pattern = fixed("\\toprule"),
                replacement = str_c("\\label{tab:", label, "} \\\\ \n \\toprule")) %>% 
    fix_panel_titles(pres = TRUE) %>% 
    adjust_col_spacing(3)
  if (num_coef_cols == 5) {
    proc_table %<>% 
      str_replace(pattern = fixed("& 2.75\\% Fee "), replacement = "") %>% 
      str_replace(pattern = "& & reminder & reminder & & &", replacement = "& & reminder & reminder & &")
  }
  return(proc_table)
}

# (1.14) for Table C.3
add_row_nobs <- function(table, data, var1, var2) {
  nobs_var1 <- data %>% filter(eval(as.name(var1)) == 1) %>% count() %>% as.numeric() %>% format(big.mark = ",")
  nobs_var2 <- data %>% filter(eval(as.name(var2)) == 1) %>% count() %>% as.numeric() %>% format(big.mark = ",")
  nobs_total <- data %>% count() %>% as.numeric() %>% format(big.mark = ",")
  table %>% 
    str_replace(pattern = fixed("\\bottomrule"),
                replacement = paste("Number of observations &",
                                    nobs_var1, "&", nobs_var2, "&",
                                    nobs_total, "&", nobs_total, "\\\\ \n\\bottomrule"))
}

# (1.15): Add F-test of joint significance to balance table.  #
add_f_test <- function(table, data, variable) {
  balance_outcomes_omit <- balance_outcomes %>% .[!str_detect(., "new_buss_type_other")]
  model <- reg_basic(var = variable,
                     model = paste("~", paste(balance_outcomes_omit, collapse = " + ")),
                     data = data)
  f_stat <- fitstat(model, "f")[["f"]][["stat"]]
  p_val <- fitstat(model, "f")[["f"]][["p"]]
  table %>% 
    str_replace(pattern = fixed("\\bottomrule"),
                replacement = paste("\\midrule \n F-stat of joint test & & &",
                                    round(f_stat, 3), "&", 
                                    str_c('$', "[", round(p_val, 3), "]", '$'), " \\\\\n",
                                    "\\bottomrule"))
}

# (1.16): Kable processing for previous function.
balance_table_format <- function(table, var1_title, var2_title, diff_title) {
  kable(table, 
        format = "latex", 
        align = c("l", "c", "c", "c", "c"),
        col.names = c("", "(1)", "(2)", "(3)", "(4)"), 
        booktabs = TRUE, 
        line = FALSE, 
        escape = FALSE) %>%
        str_replace(pattern = fixed("\\toprule"), 
                replacement = paste("\\toprule\n", 
                                    "&", var1_title, "&", var2_title, "&", diff_title, "& P-value \\\\")) %>% 
    # Fix final issues (kable inserting FALSE between rows).
    str_replace_all(pattern = fixed("\nFALSE\n"), replacement = "\n")
}

# (1.17): Generate dataframe rows with variable name, grouped means, difference and p-value of difference.
balance_table_row <- function(data, outcome, model, var1, var2) {
  # Run regression
  mod <- reg_basic(data, outcome, model)
  # Top row: means.
  top_row <- tribble(~unant, ~ant, ~diff, ~pval,
                     data %>% filter(eval(as.name(var1)) == 1) %>%  pull(outcome) %>% mean(na.rm = TRUE),
                     data %>% filter(eval(as.name(var2)) == 1) %>%  pull(outcome) %>% mean(na.rm = TRUE),
                     mod[["coefficients"]][[var2]],
                     mod[["coeftable"]][var2, "Pr(>|t|)"])
  # Bottom row: std. dev. and SE
  bottom_row <- tribble(~unant, ~ant, ~diff, ~pval,
                        data %>% filter(eval(as.name(var1)) == 1) %>%  pull(outcome) %>% sd(na.rm = TRUE),
                        data %>% filter(eval(as.name(var2)) == 1) %>%  pull(outcome) %>% sd(na.rm = TRUE),
                        mod[["se"]][[var2]],
                        0)
  # Modify format depending on first column (intercept), which gives variable magnitude
  top_row <- comma_format_toprow(top_row)
  bottom_row <- comma_format_bottomrow(bottom_row, top_row)
  # Add significance stars
  numeric_pval <- as.numeric(top_row$pval)
  # top_row %<>%
  #   mutate(pval = ifelse(numeric_pval <= 0.01, str_c('$', '[', pval, ']', '^{***}', '$'),
  #                        ifelse(numeric_pval <= 0.05, str_c('$', '[', pval, ']', '^{**}', '$'),
  #                               ifelse(numeric_pval <= 0.1,  str_c('$', '[', pval, ']', '^{*}', '$'),  pval)))) 
  
  top_row %<>%
    mutate(pval = ifelse(numeric_pval <= 0.01, str_c('$', '[', pval, ']', '^{***}', '$'),
                         ifelse(numeric_pval <= 0.05, str_c('$', '[', pval, ']', '^{**}', '$'),
                                ifelse(numeric_pval <= 0.1,  str_c('$', '[', pval, ']', '^{*}', '$'),  
                                       ifelse(numeric_pval > 0.1, str_c('$','[', pval, ']', '$'), pval))))) 
  
  # Delete bottom row pval.
  bottom_row$pval <- ""
  # Brackets for SDs.
  bottom_row %<>% 
    mutate_at(vars(!c(pval, diff)), ~str_c("(", .,  ")")) %>%
    # mutate_at(vars(diff), ~str_c("[", ., "]")) Use the code below instead of this to have parenthesis instead of square bracket (Change made by Mohammad)
    mutate_at(vars(diff), ~str_c("(", ., ")"))
  # Output
  current_longname <- balance_outcomes_varnames %>% filter(shortname == str_replace(outcome, "trust_scale_[0-9]+_", "")) %>% pull(longname)
  varname_tibble <- tribble(~varname, current_longname, "")
  return(cbind(varname_tibble, rbind(top_row, bottom_row)))
}

