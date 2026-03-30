#--------------------------------------------------------------------------------------------
# File name: 		      11b_average_takeup_quintiles.R
# Creation date:      2022-01-03
# Author:          		César Landín
# Files used:
# 	- here("proc", "balance_table_data.csv")
#   - here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("results", "figures", "avg_takeup_baseline_sales_nodlnorem.eps")
#   - here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_ontime.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q1.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q5.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q1_ontime.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q5_ontime.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_test_q5_q1.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_test_q5_q1_ontime.tex")
#   - here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_test_q5_mean.tex")
#   - here("results", "numbers", "stat_n_no_dead_no_rem.tex")
# Purpose:
#   - Figure C.2: Take-Up by Baseline Sales Quintiles for No-Deadline, No-Reminder Group
# 	- Calculate and graph take-up by quintile of baseline sales. Test whether take-up is 
#     significantly different between firms of different baseline sales quintiles.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(scales)
library(fixest)
library(modelsummary)
library(fastDummies)
library(multcomp)
library(conflicted)
library(qs)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # Times.ttf to times.ttf for running on the server
showtext_auto()
options(readr.show_col_types = FALSE)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("last", "dplyr")
source(here("scripts", "programs", "05c_barplot_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "05b_graph_colors.R"))
#########################################################

##############################################
##    (1): Initial survey data processing.  ##
##############################################
# (1.1): Import baseline sales data. 
balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
  select(organization_uuid, valid_volume_w5, median_baseline_sales, above_median_baseline_sales)
balance_data %>% tab(median_baseline_sales)

# (1.2): Import take-up by March 31.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, treat_description, accepted_offer_late, accepted_offer_ontime)
fintech_fee %>% tab(treat_description)

# (1.3): Merge take-up and baseline sales data.
balance_data %<>%
  left_join(fintech_fee, by = "organization_uuid") %>%
  arrange(valid_volume_w5) %>%
  filter(!(treat_description %in% c("Control", "24-hour deadline, no reminder")))


rm(fintech_fee)

####################################################
##    (2): Graph take-up by number of employees.  ##
####################################################
# (2.1): Quintile regression function.
quintile_reg <- function(df) {
  df %>%
    mutate(quintile = ntile(valid_volume_w5, 5)) %>% 
    dummy_cols(c("quintile")) %>% 
    feols(data = .,
          accepted_offer_late ~ quintile_2 + quintile_3 + quintile_4 + quintile_5,
          se = "hetero")
}
quintile_reg_ontime <- function(df) {
  df %>%
    mutate(quintile = ntile(valid_volume_w5, 5)) %>% 
    dummy_cols(c("quintile")) %>% 
    feols(data = .,
          accepted_offer_ontime ~ quintile_2 + quintile_3 + quintile_4 + quintile_5,
          se = "hetero")
}

# (2.2): Function to extract coefficients from regression.
get_coefs <- function(df) {
  m1 <- quintile_reg(df)
  tibble(term = c("intercept", "2", "3", "4", "5"),
         coef = m1$coefficients,
         se = m1$se) %>% 
    mutate(coef = case_when(term != "intercept" ~ coef + nth(coef, 1),
                            term == "intercept" ~coef),
           lci = coef - 1.96 * se,
           lci = ifelse(term == "intercept", NA, lci),
           uci = coef + 1.96 * se,
           uci = ifelse(term == "intercept", NA, uci),
           term = ifelse(term == "intercept", "1", term))
}

get_coefs_ontime <- function(df) {
  m1 <- quintile_reg_ontime(df)
  tibble(term = c("intercept", "2", "3", "4", "5"),
         coef = m1$coefficients,
         se = m1$se) %>% 
    mutate(coef = case_when(term != "intercept" ~ coef + nth(coef, 1),
                            term == "intercept" ~coef),
           lci = coef - 1.96 * se,
           lci = ifelse(term == "intercept", NA, lci),
           uci = coef + 1.96 * se,
           uci = ifelse(term == "intercept", NA, uci),
           term = ifelse(term == "intercept", "1", term))
}
# (2.3): Define bar plot colors from standardized palette.
current_col <- lapply(seq(-8, 0, 2), modify_col, col = group_colors$col1[3]) %>% as_vector()
# names(current_col) <- c(1:5)
show_col(current_col)

# (2.4): Define function for processing data and generating graph.
graph_quintiles <- function(df, var) {
  # Generate graph
  df %>% 
    graph_barplot() +
    scale_fill_manual(values = current_col) +
    xlab(ifelse(var == "nb_employees", 
                "Quintile Number of Employees",
                "Quintile Baseline Sales")) +
    ylab("Average Take-Up Rate") +
    theme(text = element_text(family = "Times New Roman"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
}

graph_quintiles_pres <- function(df, var) {
  # Generate graph
  df %>% 
    graph_barplot() +
    scale_fill_manual(values = current_col) +
    xlab(ifelse(var == "nb_employees", 
                "Quintile Number of Employees",
                "Quintile Baseline Sales")) +
    ylab("Average Take-Up Rate") 
}

# (2.5): Generate restricted and full graphs.
balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>% 
  get_coefs() %>% 
  graph_quintiles("valid_volume_w5")
ggsave_(here("results", "figures", "avg_takeup_baseline_sales_nodlnorem.eps"), width = 6, height = 4)

# (2.5): Calculate mean take-up among no deadline, no reminder group (overall).
balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>% 
  pull(accepted_offer_late) %>% 
  mean() %>% 
  print_pct("avg_takeup_baseline_sales_nodlnorem.tex",
            "Average take-up among firms with no deadline and no reminder")

balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>% 
  pull(accepted_offer_ontime) %>% 
  mean() %>% 
  print_pct("avg_takeup_baseline_sales_nodlnorem_ontime.tex",
            "Average take-up among firms with no deadline and no reminder (ontime)")

# (2.6): Calculate mean take-up among no deadline-no reminder group (for q1 and q5).
calc_mean_takeup <- function(quintile) {
  balance_data %>%
    mutate(quintile_baseline_sales = ntile(valid_volume_w5, 5)) %>% 
    filter(treat_description == "No deadline, no reminder" & quintile_baseline_sales == quintile) %>% 
    pull(accepted_offer_late) %>% 
    mean() %>% 
    print_pct(str_c("avg_takeup_baseline_sales_dlantrem_q", quintile, ".tex"),
              str_c("Average take-up among firms with deadline and anticipated reminder, Q", quintile))
}
calc_mean_takeup(1)
calc_mean_takeup(5)

calc_mean_takeup_ontime <- function(quintile) {
  balance_data %>%
    mutate(quintile_baseline_sales = ntile(valid_volume_w5, 5)) %>% 
    filter(treat_description == "No deadline, no reminder" & quintile_baseline_sales == quintile) %>% 
    pull(accepted_offer_ontime) %>% 
    mean() %>% 
    print_pct(str_c("avg_takeup_baseline_sales_dlantrem_q", quintile, "_ontime.tex"),
              str_c("Average take-up among firms with deadline and anticipated reminder, Q", quintile))
}
calc_mean_takeup_ontime(1)
calc_mean_takeup_ontime(5)
#######################################
##    (3): Hypothesis tests for Q5.  ##
#######################################
# (3.1): Function for testing coefficient significance.
extract_test_coef <- function(model, coef) {
  res <- model %>% 
    glht(linfct = str_c(coef, "= 0")) %>% 
    confint() %>% 
    summary()
  return(as.numeric(res$test$pvalues))
}
get_test_significance <- function(model, coef) {
  pval <- extract_test_coef(model, coef)
  level <- case_when(pval < 0.01 ~ "99%",
                     pval < 0.05 ~ "95%",
                     pval < 0.1 ~ "90%",
                     pval >= 0.1 ~ "")
  level <- ifelse(level != "", 
                  str_c("Reject null hypothesis at the ", level, " level"),
                  "Cannot reject null hypothesis")
  cat(c(str_c("Test: ", coef, " == 0", "\n"),
          str_c("P-value ", ifelse(pval < 0.001, "< 0.001", str_c("= ", comma_format(pval))), "\n"),
          str_c(level)))
  return(invisible(pval))
}

# (3.2): Run tests and export p-values.
balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>%
  quintile_reg() %>% 
  get_test_significance("quintile_5") %>% 
  print_n("avg_takeup_baseline_sales_nodlnorem_test_q5_q1.tex",
          dig = 3,
          "Test whether H[Q5 = 0] is statistically significant")

balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>%
  quintile_reg_ontime() %>% 
  get_test_significance("quintile_5") %>% 
  print_n("avg_takeup_baseline_sales_nodlnorem_test_q5_q1_ontime.tex",
          dig = 3,
          "Test whether H[Q5 = 0] is statistically significant")

# (3.3): Function for testing difference between Int + Q5 and mean.
test_q5_mean <- function(df) {
  # Get confidence interval and SE for Intercept + Q5
  q5_coef_se <- df %>%
    quintile_reg() %>%
    glht(linfct = str_c("(Intercept) + quintile_5 = 0")) %>%
    confint() %>%
    summary() %>%
    .[["test"]] %>%
    .[c("coefficients", "sigma")] %>%
    unlist()
  
  # Regress take-up on constant and get coef and SE of intercept
  mean_coef_se <- df %>%
    feols(data = .,
          accepted_offer_late ~ 1,
          se = "hetero") %>%
    .[c("coefficients", "se")] %>%
    unlist()
  
  # Get Z-score
  cat(str_c("Q5 coef = ", comma_format(q5_coef_se[1]), ", SE = ", comma_format(q5_coef_se[2]), "\n",
            "Mean coef = ", comma_format(mean_coef_se[1]), ", SE = ", comma_format(mean_coef_se[2]),"\n"))
  z <- (q5_coef_se[1] - mean_coef_se[1]) / sqrt(q5_coef_se[2]^2 + mean_coef_se[2]^2)
  cat(paste("Z-score of test of equality between betas is", comma_format(z), "\n"))
  
  # One-tailed test (right tail, Q5 larger than mean)
  z_p <- pnorm(z, lower.tail = FALSE)
  cat(paste("P-value of one-sided test is", comma_format(z_p)))
  return(invisible(z_p))
}

# (3.4): Run tests and export p-values.
balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>% 
  test_q5_mean() %>% 
  print_n("avg_takeup_baseline_sales_nodlnorem_test_q5_mean.tex",
          dig = 3,
          "Test whether H[Int + Q5 - mean = 0] is statistically significant")


# (2.6): Generate and save footnotes.
num_obs_group <- balance_data %>%
  filter(treat_description == "No deadline, no reminder") %>% 
  nrow()
num_obs_group %>% 
  print_n("stat_n_no_dead_no_rem.tex",
          "Number of firms in the no deadline, no reminder group")
