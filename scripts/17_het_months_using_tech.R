#--------------------------------------------------------------------------------------------
# File name: 		      17_het_months_using_tech.R
# Creation date:      2022-04-26
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "colors.csv")
# 	- here("proc", "balance_table_data.csv")
# 	- here("proc", "fintech_fee.csv")
# 	- here("proc", "fintech_fee_light.csv")
# Files created:
# 	- here("results", "figures", "usage_het_trim5_nonres_late.pdf")
# Purpose:
# 	- Figure 11: Effect of Announced Reminders by Length of Business Relationship: Estimate heterogeneity by number of months in Fintech.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(lubridate)
library(magrittr)
library(fixest)
library(modelsummary)
library(ggplot2)
library(scales)
library(cowplot)
library(tabulator)
library(fastDummies)
library(multcomp)
library(conflicted)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # change to times
showtext_auto()
pacman::p_load_current_gh("ChihYuChiang/dlsr")
library(dlsr) # added on Sep 20, 2025 for running on the server
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("last", "dplyr")
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
group_colors <- read_csv(here("proc", "colors.csv"))
#########################################################

#########################################
##    (1): Process Fintech usage data.  ##
#########################################
# (1.1): Import balance table data.
balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
      mutate(antrem = ifelse(anticipated_reminder, "Announced reminder", "Unannounced reminder"))
fintech_fee <- read_csv(here("proc", "fintech_fee.csv"))
# (1.2): Fix observations without owner age.
balance_data %<>% mutate(owner_age = ifelse(is.na(owner_age), 0, owner_age))
balance_data %<>% left_join(fintech_fee|>
                              select(organization_uuid, no_deadline), by = "organization_uuid")
# (1.3): Restrict data to announced / unannounced reminder.
balance_data_antrem <- balance_data %>% 
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1)
# (1.4): Define list with outcomes.
balance_outcomes <- c("owner_sex_female",  "owner_age", "owner_age_unknown",
                      "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals", 
                      "new_buss_type_restaurants", "new_buss_type_small_retailers", 
                      "make_sale", "log_valid_volume_w5", "log_nr_valid_payments_w5")

# (1.5): Define regression function.
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

# (1.6): Import treatment and acceptance status, merge data.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
  select(organization_uuid, takeup_date, starts_with("accepted_offer"))
balance_data_antrem %<>% left_join(fintech_fee, by = "organization_uuid")
rm(fintech_fee)

# (1.7): Select baseline covariates to use for residualization using double lasso selection.
# Generate all interactions between variables
interactions <- combn(balance_outcomes, 2, FUN = paste, collapse = ":")
# Select variables for take-up and months using technology
var_selection <- doubleLassoSelect(balance_data_antrem, 
                                     outcome = "accepted_offer_late",
                                     treatment = "months_since_ft",
                                     test = balance_outcomes) %>% 
  select(-accepted_offer_late, -months_since_ft) %>% 
  colnames()
var_selection
# Append owner age unknown.
var_selection %<>% append("owner_age_unknown")

var_selection_firstday <- doubleLassoSelect(balance_data_antrem, 
                                   outcome = "accepted_offer_firstday",
                                   treatment = "months_since_ft",
                                   test = balance_outcomes) %>% 
  select(-accepted_offer_firstday, -months_since_ft) %>% 
  colnames()
var_selection_firstday
# Append owner age unknown.
var_selection_firstday %<>% append("owner_age_unknown")



var_selection_ontime <- doubleLassoSelect(balance_data_antrem, 
                                            outcome = "accepted_offer_ontime",
                                            treatment = "months_since_ft",
                                            test = balance_outcomes) %>% 
  select(-accepted_offer_ontime, -months_since_ft) %>% 
  colnames()
var_selection_ontime
# Append owner age unknown.
var_selection_ontime %<>% append("owner_age_unknown")


# (1.9): Residualize take-up and usage using baseline firm characteristics.
# Experience can be correlated with other factors: residualize take-up and months using technology
# Extract these residuals
# Loop over take-up and months since first transaction
for (ind_var in c("accepted_offer_late", "months_since_ft")) {
    # Regress usage or takeup on selected variables from double lasso.
    current_reg <- reg_basic(balance_data_antrem, 
                             model = paste(ind_var, " ~", str_c(var_selection, collapse = " + ")))
    # Add residuals to dataset
    balance_data_antrem %<>% mutate(!!str_c("res_", ind_var) := current_reg %>% extract2("residuals"))
}

for (ind_var in c("accepted_offer_firstday", "months_since_ft")) {
  # Regress usage or takeup on selected variables from double lasso.
  current_reg_firstday <- reg_basic(balance_data_antrem, 
                           model = paste(ind_var, " ~", str_c(var_selection_firstday, collapse = " + ")))
  # Add residuals to dataset
  balance_data_antrem_firstday <- balance_data_antrem %>% mutate(!!str_c("res_", ind_var) := current_reg_firstday %>% extract2("residuals"))
}

for (ind_var in c("accepted_offer_ontime", "months_since_ft")) {
  # Regress usage or takeup on selected variables from double lasso.
  current_reg_ontime <- reg_basic(balance_data_antrem, 
                                    model = paste(ind_var, " ~", str_c(var_selection_ontime, collapse = " + ")))
  # Add residuals to dataset
  balance_data_antrem_ontime <- balance_data_antrem %>% mutate(!!str_c("res_", ind_var) := current_reg_ontime %>% extract2("residuals"))
}

#### CALCULATE PERCENTILES OF RESIDUALIZED MONTHS ####
balance_data_antrem %<>%
  mutate(pct_res_months_since_ft = ntile(res_months_since_ft, 100))
######################################################

############################################
##    (2): Generate residualized graphs.  ##
############################################
# (2.1): Define takeup graph function.
graph_takeup <- function(usage_var, takeup_var, ytitle, df = balance_data_antrem, xtitle = "Usage rank") {
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = antrem, fill = antrem)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = group_colors %>% filter(str_detect(group, "anticipated")) %>% pull(col1)) +
    scale_fill_manual(values = group_colors %>% filter(str_detect(group, "anticipated")) %>% pull(col2)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "bottom", size = 17, legend_text_size = 17) + 
    theme(text = element_text(size = 14),
          axis.text.x = element_text(hjust = 1, size = 16),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.text = element_text(margin = margin(t = 5, unit = "pt")),
          legend.box.margin = margin(-10, 0, 0, 0),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(1.5,"line"))
}

graph_takeup_firstday <- function(usage_var, takeup_var, ytitle, df = balance_data_antrem_firstday, xtitle = "Usage rank") {
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = antrem, fill = antrem)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = group_colors %>% filter(str_detect(group, "anticipated")) %>% pull(col1)) +
    scale_fill_manual(values = group_colors %>% filter(str_detect(group, "anticipated")) %>% pull(col2)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "bottom", size = 17, legend_text_size = 17) + 
    theme(text = element_text(size = 14),
          axis.text.x = element_text(hjust = 1, size = 16),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.text = element_text(margin = margin(t = 5, unit = "pt")),
          legend.box.margin = margin(-10, 0, 0, 0),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(1.5,"line"))
}

graph_takeup_ontime <- function(usage_var, takeup_var, ytitle, df = balance_data_antrem_ontime, xtitle = "Usage rank") {
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = antrem, fill = antrem)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = group_colors %>% filter(str_detect(group, "anticipated")) %>% pull(col1)) +
    scale_fill_manual(values = group_colors %>% filter(str_detect(group, "anticipated")) %>% pull(col2)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "bottom", size = 17, legend_text_size = 17) + 
    theme(text = element_text(size = 14),
          axis.text.x = element_text(hjust = 1, size = 16),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.text = element_text(margin = margin(t = 5, unit = "pt")),
          legend.box.margin = margin(-10, 0, 0, 0),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(1.5,"line"))
}


graph_takeup_deadline <- function(usage_var, takeup_var, ytitle, df = balance_data_dl |>
                                    mutate(dl = recode(deadline, `0` = "No deadline", `1` = "Deadline")), xtitle = "Usage rank") {
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = dl, fill = dl)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = group_colors %>% filter(str_detect(group, "deadline")) %>% pull(col1)) +
    scale_fill_manual(values = group_colors %>% filter(str_detect(group, "deadline")) %>% pull(col2)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "bottom", size = 17, legend_text_size = 17) + 
    theme(text = element_text(size = 14),
          axis.text.x = element_text(hjust = 1, size = 16),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.text = element_text(margin = margin(t = 5, unit = "pt")),
          legend.box.margin = margin(-10, 0, 0, 0),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(1.5,"line"))
}

graph_takeup_deadline_firstday <- function(usage_var, takeup_var, ytitle, df = balance_data_dl_firstday |>
                                    mutate(dl = recode(deadline, `0` = "No deadline", `1` = "Deadline")), xtitle = "Usage rank") {
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = dl, fill = dl)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = group_colors %>% filter(str_detect(group, "deadline")) %>% pull(col1)) +
    scale_fill_manual(values = group_colors %>% filter(str_detect(group, "deadline")) %>% pull(col2)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "bottom", size = 17, legend_text_size = 17) + 
    theme(text = element_text(size = 14),
          axis.text.x = element_text(hjust = 1, size = 16),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.text = element_text(margin = margin(t = 5, unit = "pt")),
          legend.box.margin = margin(-10, 0, 0, 0),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(1.5,"line"))
}


graph_takeup_dl_antrem <- function(usage_var, takeup_var, ytitle, df = balance_data_dl_antrem, xtitle = "Usage rank") {
  my_colors <- c("#88CCEE", "#CC6677", "#DDCC77", "#449944")  # Replace with your actual color values
  
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = dl_antrem, fill = dl_antrem)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "right", size = 17, legend_text_size = 17) + 
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(hjust = 1, size = 16),
      legend.position = c(0, 1),               # Place legend at the top left
      legend.justification = c(0, 1),          # Align legend to top left
      legend.title = element_blank(),
      legend.direction = "vertical",           # Stack legend items vertically
      legend.spacing.x = unit(0.3, "cm"), 
      legend.text = element_text(margin = margin(t = 5, unit = "pt")),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(1.5, "line"),
      plot.margin = margin(10, 10, 10, 10)     # Adjust margins if needed
    )
}

graph_takeup_dl_antrem_firstday <- function(usage_var, takeup_var, ytitle, df = balance_data_dl_antrem_firstday, xtitle = "Usage rank") {
  my_colors <- c("#88CCEE", "#CC6677", "#DDCC77", "#449944")  # Replace with your actual color values
  
  df %>% 
    ggplot(aes(x = eval(as.name(usage_var)), y = eval(as.name(takeup_var)), color = dl_antrem, fill = dl_antrem)) +
    geom_smooth(se = TRUE) +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    xlab(xtitle) + 
    ylab(ytitle) +
    set_theme(legend_position = "right", size = 17, legend_text_size = 17) + 
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(hjust = 1, size = 16),
      legend.position = c(0, 1),               # Place legend at the top left
      legend.justification = c(0, 1),          # Align legend to top left
      legend.title = element_blank(),
      legend.direction = "vertical",           # Stack legend items vertically
      legend.spacing.x = unit(0.3, "cm"), 
      legend.text = element_text(margin = margin(t = 5, unit = "pt")),
      legend.box.margin = margin(0, 0, 0, 0),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(1.5, "line"),
      plot.margin = margin(10, 10, 10, 10)     # Adjust margins if needed
    )
}

# (2.2): Define presentation adjustments function.
adjust_size <- function(graph, legend_size = 20) {
  graph %>%
    set_theme(legend_position = "bottom", size = 20, legend_text_size = legend_size) +
    theme(legend.title = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          legend.box.margin = margin(0, 10, 0, 10),
          legend.key.size = unit(1.5,"line"))
}


# (2.3)(**): Generate dataset with top 5 percent trimmed off for non_residualized.
balance_data_trim_nonres <- balance_data_antrem %>%
  mutate(usage_percentile = ntile(months_since_ft, 100)) %>%
  filter(usage_percentile <= 95)

# Keep limits equal
limits <- balance_data_trim_nonres %>%
  group_by(antrem) %>%
  select(months_since_ft) %>%
  summarise(min_usage = min(months_since_ft),
            max_usage = max(months_since_ft)) %>%
  ungroup() %>%
  summarise(min_usage = max(min_usage),
            max_usage = min(max_usage)) 


balance_data_trim_nonres %<>%
  mutate(months_since_ft = ifelse(months_since_ft < limits$min_usage, limits$min_usage, months_since_ft),
         months_since_ft = ifelse(months_since_ft > limits$max_usage, limits$max_usage, months_since_ft))

g1 <- graph_takeup(usage_var = "months_since_ft",
                   takeup_var = "accepted_offer_late",
                   ytitle = "Take-Up",
                   df = balance_data_trim_nonres,
                   xtitle = "Length of Business Relationship (Months)") +
  theme(legend.text = element_text(family = "Times New Roman", size = 16),
        axis.text.x = element_text(family = "Times New Roman", size = 16),
        axis.text.y = element_text(family = "Times New Roman", size = 16),
        axis.title.y = element_text(family = "Times New Roman", size = 16), 
        axis.title.x = element_text(family = "Times New Roman", size = 16))
g1
ggsave_(here("results", "figures", str_c("usage_het_trim5_nonres_late.pdf")), width = 8, height = 6.5)

