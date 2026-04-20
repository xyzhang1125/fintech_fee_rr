#--------------------------------------------------------------------------------------------
# File name: 		      x02_editor_01_regression_clean.R
# Creation date:      2026-01-28
# Author:          		Xinyu Zhang
# Purpose:            Data preparation + length of business relationship HTE on expected savings
# Note:               This script is cleaned by Claude code and manually checked by Xinyu Zhang
#--------------------------------------------------------------------------------------------

# =============================
# 1. SETUP AND LIBRARIES
# =============================
library(here)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(data.table)
library(fixest)
library(tabulator)
library(multcomp)
library(conflicted)
library(qs)
library(modelsummary)
library(showtext)
library(gridExtra)

font_add(family = "Times New Roman", regular = "Times.ttf")
showtext_auto()

conflict_prefer("last", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(magrittr::set_names)

options(readr.show_col_types = FALSE)

source(here("scripts", "programs", "21a_survey_histogram_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "myfunctions.R"))
source(here("scripts", "programs", "winsorize.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "x00a_editor_01_table_function.R"))
source(here("scripts", "programs", "table_functions.R"))

# =============================
# 2. DATA IMPORT
# =============================
# survey data
survey_all <- read_csv("proc/survey_all.csv")
survey_successful <- survey_all %>%
  filter(successful == 1) %>%
  select(organization_uuid, endtime, accepted_offer, new_rate,
         value_transactions, recall_old_fee, amount_paid_fee, exp_savings)

survey_successful_proc <- read_csv("proc/survey_successful.csv")

# extract successful companies
survey_org_uuids <- unique(survey_successful_proc$organization_uuid)

# admin data (monthly and weekly)
monthly_data <- qs::qread(here("proc", "fintech_fee_monthly.qs"))[, c('organization_uuid', 'timestamp_month', 'valid_volume_w5',
                                                                      'valid_volume', 'commission_model', 'treat_type', 'fee_type', 'accepted_offer_ontime')]
monthly_data_svy_successful <- monthly_data[monthly_data$organization_uuid %in% survey_org_uuids, ]

weekly_data <- qs::qread(here("proc", "fintech_fee_weekly.qs"))[, c('organization_uuid', 'timestamp_week', 'valid_volume_w5',
                                                                    'valid_volume', 'commission_model', 'treat_type', 'fee_type', 'accepted_offer_ontime')]
weekly_data_svy_successful <- weekly_data[weekly_data$organization_uuid %in% survey_org_uuids, ]

# =============================
# 3. DATA CLEANING
# =============================

# 3.1 Calculate historical volatility measures (monthly)
monthly_data_svy_successful$timestamp_month <- as.Date(monthly_data_svy_successful$timestamp_month)
monthly_data_svy_successful <- monthly_data_svy_successful %>%
  mutate(in_analysis_period = timestamp_month >= "2019-09-01" & timestamp_month <= "2020-08-01") %>%
  group_by(organization_uuid) %>%
  mutate(
    past_year_sales_mean = mean(valid_volume[in_analysis_period], na.rm = TRUE),
    past_year_sales_sd = sd(valid_volume[in_analysis_period], na.rm = TRUE),
    past_year_volatility = past_year_sales_sd / past_year_sales_mean
  ) %>%
  ungroup() %>%
  filter(timestamp_month == as.Date("2020-08-01"))

# 3.2 Calculate historical volatility measures (weekly)
weekly_data_svy_successful$timestamp_week <- as.Date(weekly_data_svy_successful$timestamp_week)
weekly_data_svy_successful <- weekly_data_svy_successful %>%
  mutate(in_analysis_period = timestamp_week >= "2019-09-02" & timestamp_week <= "2020-08-31") %>%
  group_by(organization_uuid) %>%
  mutate(
    past_year_sales_mean_weekly = mean(valid_volume[in_analysis_period], na.rm = TRUE),
    past_year_sales_sd_weekly = sd(valid_volume[in_analysis_period], na.rm = TRUE),
    past_year_volatility_weekly = past_year_sales_sd_weekly / past_year_sales_mean_weekly
  ) %>%
  ungroup() %>%
  filter(timestamp_week == as.Date("2020-08-31"))

# 3.3 Add survey variables to survey_successful
survey_successful <- survey_successful %>%
  left_join(
    survey_successful_proc %>%
      select(organization_uuid, reminder, deadline, anticipated_reminder,
             unanticipated_reminder, accepted_offer_survey, trust_scale_1, trust_scale_1_binary),
    by = "organization_uuid"
  )

# Add binary response variables
survey_successful <- survey_successful %>%
  mutate(
    dont_know = case_when(exp_savings == -777 ~ 1, TRUE ~ 0),
    refuse = case_when(exp_savings == -888 ~ 1, TRUE ~ 0),
    dont_answer = case_when(exp_savings == -777 | exp_savings == -888 ~ 1, TRUE ~ 0)
  )

# 3.4 Add survey timestamp
survey_successful$survey_timestamp_last_week <-
  mdy_hms(survey_successful$endtime) %>%
  floor_date(unit = "week", week_start = 1) %>%
  - weeks(1) %>% as.Date()

# 3.5 Clean special values in survey variables
vars_to_clean <- c("value_transactions", "recall_old_fee", "amount_paid_fee", "exp_savings")
survey_successful <- survey_successful %>% mutate(across(all_of(vars_to_clean),
                                                         ~ case_when(. %in% c(-666, -777, -888) ~ NA_real_, TRUE ~ .)))

# 3.6 Create monthly validation dataset
weekly_data$timestamp_week <- as.Date(weekly_data$timestamp_week)
weekly_validation <- survey_successful %>% left_join(weekly_data, by = c("organization_uuid" = "organization_uuid",
                                                                         "survey_timestamp_last_week" = "timestamp_week"))

monthly_validation <- monthly_data %>%
  filter(organization_uuid %in% survey_org_uuids,
         timestamp_month >= as.Date("2020-08-01") & timestamp_month <= as.Date("2021-03-01")) %>%
  select(organization_uuid, timestamp_month, valid_volume, commission_model, treat_type, fee_type, accepted_offer_ontime) %>%
  left_join(survey_successful, by = "organization_uuid")

# 3.7 Generate fee variables
monthly_validation %<>%
  mutate(old_fee = case_when(
    commission_model == "FIXED_RATE" ~ 3.5,
    commission_model == "SMART_RATE" ~ 3.75,
    TRUE ~ NA_real_
  ))

# Average volume for later periods
monthly_validation <- monthly_validation %>%
  group_by(organization_uuid) %>%
  mutate(avg_volume_oct_mar = mean(
    valid_volume[timestamp_month >= as.Date("2020-10-01") & timestamp_month <= as.Date("2021-03-01")],
    na.rm = TRUE
  )) %>%
  ungroup()

# Fill missing new_rate values
monthly_validation <- monthly_validation %>%
  mutate(new_rate = case_when(
    !is.na(new_rate) ~ new_rate,
    is.na(new_rate) ~ as.numeric(str_extract(fee_type, "^[0-9]+\\.?[0-9]*")),
    TRUE ~ NA_real_
  ))

# 3.8 Calculate admin savings
monthly_validation <- monthly_validation %>%
  mutate(admin_savings = case_when(
    timestamp_month == as.Date("2020-08-01") ~ valid_volume * (old_fee - new_rate) / 100,
    timestamp_month == as.Date("2020-10-01") ~ valid_volume * (old_fee - new_rate) / 100,
    timestamp_month == as.Date("2021-03-01") ~ avg_volume_oct_mar * (old_fee - new_rate) / 100,
    TRUE ~ NA_real_
  ))

# Savings difference
monthly_validation <- monthly_validation %>%
  mutate(savings_diff = exp_savings - admin_savings,
         savings_diff_std = case_when(
           exp_savings == 0 & admin_savings == 0 ~ 0,
           exp_savings != 0 & admin_savings == 0 ~ NA_real_,
           TRUE ~ savings_diff / admin_savings
         ))

# =============================
# 4. PREPARE REGRESSION DATA FROM SURVEY
# =============================

# Filter for reminder == 1 with valid exp_savings
monthly_validation_for_reg <- monthly_validation %>%
  filter(reminder == 1,
         !is.na(exp_savings),
         !exp_savings %in% c(-777, -888))

# Create August 2020 regression dataset
august_2020_validation_for_reg <- monthly_validation_for_reg %>%
  filter(timestamp_month == "2020-08-01")

# Merge historical volatility measures
august_2020_validation_for_reg <- august_2020_validation_for_reg %>%
  left_join(
    monthly_data_svy_successful %>%
      select(organization_uuid, past_year_sales_mean, past_year_sales_sd, past_year_volatility) %>%
      distinct(organization_uuid, .keep_all = TRUE),
    by = "organization_uuid"
  ) %>%
  left_join(
    weekly_data_svy_successful %>%
      select(organization_uuid, past_year_sales_mean_weekly, past_year_sales_sd_weekly, past_year_volatility_weekly) %>%
      distinct(organization_uuid, .keep_all = TRUE),
    by = "organization_uuid"
  )

# =============================
# 5. MERGE BUSINESS RELATIONSHIP DATA
# =============================
balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>%
  mutate(antrem = ifelse(anticipated_reminder, "Announced reminder", "Unannounced reminder"))

august_2020_validation_for_reg <- august_2020_validation_for_reg %>%
  left_join(
    balance_data %>%
      select(organization_uuid, months_since_ft) %>%
      distinct(),
    by = "organization_uuid"
  )

# =============================
# 6. CREATE BINARY VARIABLES
# =============================

# Expected savings above/below median
august_2020_validation_for_reg <- august_2020_validation_for_reg %>%
  mutate(
    exp_savings_above_median = case_when(
      exp_savings > median(exp_savings, na.rm = TRUE) ~ 1,
      exp_savings <= median(exp_savings, na.rm = TRUE) ~ 0,
      is.na(exp_savings) ~ NA_real_
    )
  )

# Business relationship above/below median
august_2020_validation_for_reg <- august_2020_validation_for_reg %>%
  mutate(
    biz_relation_above_median = case_when(
      months_since_ft > median(months_since_ft, na.rm = TRUE) ~ 1,
      months_since_ft <= median(months_since_ft, na.rm = TRUE) ~ 0,
      is.na(months_since_ft) ~ NA_real_
    )
  )


# Volatility above/below median
august_2020_validation_for_reg <- august_2020_validation_for_reg %>%
  mutate(
    volatility_weekly_above_median = case_when(
      past_year_volatility_weekly > median(past_year_volatility_weekly, na.rm = TRUE) ~ 1,
      past_year_volatility_weekly <= median(past_year_volatility_weekly, na.rm = TRUE) ~ 0,
      is.na(past_year_volatility_weekly) ~ NA_real_
    )
  )

# admin savings above/below median
august_2020_validation_for_reg <- august_2020_validation_for_reg %>%
  mutate(
    admin_savings_above_median = case_when(
      admin_savings > median(admin_savings, na.rm = TRUE) ~ 1,
      admin_savings <= median(admin_savings, na.rm = TRUE) ~ 0,
      is.na(admin_savings) ~ NA_real_
    )
  )

# =============================
# 7. PREPARE TRUST DATA
# =============================

# Add trust variables to weekly_data_svy_successful
weekly_data_svy_successful <- weekly_data_svy_successful %>%
  left_join(
    survey_successful_proc %>%
      select(organization_uuid, trust_scale_1_binary, trust_scale_1),
    by = "organization_uuid"
  )

# Create trust regression dataset
weekly_data_trust_successful <- weekly_data_svy_successful %>%
  filter(!is.na(trust_scale_1))

# Create volatility z-score for trust data
weekly_data_trust_successful <- weekly_data_trust_successful %>%
  mutate(
    past_year_volatility_weekly_z = scale(past_year_volatility_weekly)[, 1]
  )

# Create volatility binary for trust data
weekly_data_trust_successful <- weekly_data_trust_successful %>%
  mutate(
    volatility_weekly_above_median = case_when(
      past_year_volatility_weekly > median(past_year_volatility_weekly, na.rm = TRUE) ~ 1,
      past_year_volatility_weekly <= median(past_year_volatility_weekly, na.rm = TRUE) ~ 0,
      is.na(past_year_volatility_weekly) ~ NA_real_
    )
  )

#-----------------------------------------------------------------------------------------
# Output - HTE: Business relationship and expected savings
#-----------------------------------------------------------------------------------------
controls_sets <- list(
  NULL,
  c("admin_savings_above_median", "volatility_weekly_above_median")
)

models_list <- list()

for (i in seq_along(controls_sets)) {
  if (is.null(controls_sets[[i]]) || length(controls_sets[[i]]) == 0) {
    formula_str <- "exp_savings_above_median ~ biz_relation_above_median + anticipated_reminder + biz_relation_above_median:anticipated_reminder"
  } else {
    formula_str <- paste0(
      "exp_savings_above_median ~ biz_relation_above_median + anticipated_reminder + biz_relation_above_median:anticipated_reminder + ",
      paste(controls_sets[[i]], collapse = " + ")
    )
  }

  models_list[[i]] <- feols(as.formula(formula_str), data = august_2020_validation_for_reg)
}

# ===============================
# Extract linear combinations
# ===============================
linear_combinations <- list()

for (i in seq_along(models_list)) {
  model <- models_list[[i]]
  coef_vector <- coef(model)
  vcov_matrix <- vcov(model)
  coef_names <- names(coef_vector)
  main_effect_name <- "anticipated_reminder"
  interaction_name <- "biz_relation_above_median:anticipated_reminder"

  if (main_effect_name %in% coef_names && interaction_name %in% coef_names) {
    sum_coef <- coef_vector[main_effect_name] + coef_vector[interaction_name]
    var_sum <- vcov_matrix[main_effect_name, main_effect_name] +
      vcov_matrix[interaction_name, interaction_name] +
      2 * vcov_matrix[main_effect_name, interaction_name]
    se_sum <- sqrt(var_sum)
    t_stat <- sum_coef / se_sum
    model_summary <- summary(model)
    df <- model_summary$df_residual

    if (is.null(df) || length(df) == 0) {
      df <- nobs(model) - length(coef_vector)
    }

    p_value <- 2 * (1 - pt(abs(t_stat), df = df))
    linear_combinations[[i]] <- list(sum_coef = sum_coef, p_value = p_value)
  } else {
    linear_combinations[[i]] <- NULL
  }
}

# ===============================
# Extract coefficients and SEs
# ===============================
extract_coef_se <- function(model, coef_name) {
  coef_vector <- coef(model)
  se_vector <- sqrt(diag(vcov(model)))

  if (coef_name %in% names(coef_vector)) {
    coef_val <- coef_vector[coef_name]
    se_val <- se_vector[coef_name]
    model_summary <- summary(model)
    df <- model_summary$df_residual
    if (is.null(df) || length(df) == 0) {
      df <- nobs(model) - length(coef_vector)
    }
    t_stat <- coef_val / se_val
    p_val <- 2 * (1 - pt(abs(t_stat), df = df))

    stars <- ""
    if (!is.na(p_val)) {
      if (p_val <= 0.01) stars <- "***"
      else if (p_val <= 0.05) stars <- "**"
      else if (p_val <= 0.1) stars <- "*"
    }

    return(list(coef = coef_val, se = se_val, stars = stars))
  } else {
    return(list(coef = NA, se = NA, stars = ""))
  }
}

# ===============================
# Build LaTeX table
# ===============================
tex_lines <- c(
  "\\begin{tabular}{l*{2}{>{\\centering\\arraybackslash}p{3cm}}}",
  "\\toprule",
  "  & \\multicolumn{2}{c}{Above-median expected savings} \\\\",
  "  \\cmidrule(lr){2-3}",
  "  & (1) & (2) \\\\",
  "\\midrule"
)

# Treatment coefficient
coef_name <- "anticipated_reminder"
label_line1 <- "Announced reminder ($\\beta_1$)"
label_line2 <- ""
coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models_list)) {
  result <- extract_coef_se(models_list[[i]], coef_name)

  if (!is.na(result$coef)) {
    coef_val <- sprintf("$%.3f$%s", result$coef, result$stars)
    se_val <- sprintf("($%.3f$)", result$se)
  } else {
    coef_val <- ""
    se_val <- ""
  }

  if (i == 1) {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(" & ", se_val)
  } else {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(se_line, " & ", se_val)
  }
}

coef_line <- paste0(coef_line, " \\\\")
se_line <- paste0(se_line, " \\\\")
tex_lines <- c(tex_lines, coef_line, se_line)

# Business relationship coefficient
coef_name <- "biz_relation_above_median"
label_line1 <- "Above-median length of business relationship ($\\beta_2$)"
label_line2 <- ""
coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models_list)) {
  result <- extract_coef_se(models_list[[i]], coef_name)

  if (!is.na(result$coef)) {
    coef_val <- sprintf("$%.3f$%s", result$coef, result$stars)
    se_val <- sprintf("($%.3f$)", result$se)
  } else {
    coef_val <- ""
    se_val <- ""
  }

  if (i == 1) {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(" & ", se_val)
  } else {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(se_line, " & ", se_val)
  }
}

coef_line <- paste0(coef_line, " \\\\")
se_line <- paste0(se_line, " \\\\")
tex_lines <- c(tex_lines, coef_line, se_line)

# Interaction coefficient
coef_name <- "biz_relation_above_median:anticipated_reminder"
label_line1 <- "Above-median length of business relationship"
label_line2 <- "\\hspace{1em} $\\times$ Announced reminder ($\\beta_3$)"
coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models_list)) {
  result <- extract_coef_se(models_list[[i]], coef_name)

  if (!is.na(result$coef)) {
    coef_val <- sprintf("$%.3f$%s", result$coef, result$stars)
    se_val <- sprintf("($%.3f$)", result$se)
  } else {
    coef_val <- ""
    se_val <- ""
  }

  if (i == 1) {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(se_line, " & ", se_val)
  } else {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(se_line, " & ", se_val)
  }
}

coef_line <- paste0(coef_line, " \\\\")
se_line <- paste0(se_line, " \\\\")
tex_lines <- c(tex_lines, coef_line, se_line)

# Controls
if (length(controls_sets[[2]]) > 0) {
  # Above-median actual savings
  coef_name <- "admin_savings_above_median"
  coef_line <- "Above-median actual savings"
  se_line <- ""

  for (i in 1:length(models_list)) {
    result <- extract_coef_se(models_list[[i]], coef_name)

    if (!is.na(result$coef)) {
      coef_val <- sprintf("$%.3f$%s", result$coef, result$stars)
      se_val <- sprintf("($%.3f$)", result$se)
    } else {
      coef_val <- ""
      se_val <- ""
    }

    if (i == 1) {
      coef_line <- paste0(coef_line, " & ", coef_val)
      se_line <- paste0(" & ", se_val)
    } else {
      coef_line <- paste0(coef_line, " & ", coef_val)
      se_line <- paste0(se_line, " & ", se_val)
    }
  }

  coef_line <- paste0(coef_line, " \\\\")
  se_line <- paste0(se_line, " \\\\")
  tex_lines <- c(tex_lines, coef_line, se_line)

  # Above-median volatility
  coef_name <- "volatility_weekly_above_median"
  coef_line <- "Above-median volatility"
  se_line <- ""

  for (i in 1:length(models_list)) {
    result <- extract_coef_se(models_list[[i]], coef_name)

    if (!is.na(result$coef)) {
      coef_val <- sprintf("$%.3f$%s", result$coef, result$stars)
      se_val <- sprintf("($%.3f$)", result$se)
    } else {
      coef_val <- ""
      se_val <- ""
    }

    if (i == 1) {
      coef_line <- paste0(coef_line, " & ", coef_val)
      se_line <- paste0(" & ", se_val)
    } else {
      coef_line <- paste0(coef_line, " & ", coef_val)
      se_line <- paste0(se_line, " & ", se_val)
    }
  }

  coef_line <- paste0(coef_line, " \\\\")
  se_line <- paste0(se_line, " \\\\")
  tex_lines <- c(tex_lines, coef_line, se_line)
}

# Add extra lines
tex_lines <- c(tex_lines, "\\midrule")

# Linear combinations
sum_coef_line <- "$\\beta_1+\\beta_3$"
p_value_line <- "$\\beta_1+\\beta_3$ joint test $p$-value"

for (i in seq_along(linear_combinations)) {
  if (!is.null(linear_combinations[[i]])) {
    sum_coef_val <- sprintf("$%.3f$", linear_combinations[[i]]$sum_coef)
    p_val <- linear_combinations[[i]]$p_value
    p_val_formatted <- sprintf("$[%.3f]$", p_val)
  } else {
    sum_coef_val <- ""
    p_val_formatted <- ""
  }

  if (i == 1) {
    sum_coef_line <- paste0(sum_coef_line, " & ", sum_coef_val)
    p_value_line <- paste0(p_value_line, " & ", p_val_formatted)
  } else {
    sum_coef_line <- paste0(sum_coef_line, " & ", sum_coef_val)
    p_value_line <- paste0(p_value_line, " & ", p_val_formatted)
  }
}

sum_coef_line <- paste0(sum_coef_line, " \\\\")
p_value_line <- paste0(p_value_line, " \\\\")
tex_lines <- c(tex_lines, sum_coef_line, p_value_line)

# Number of firms
nobs_line <- "Number of firms"
for (i in 1:length(models_list)) {
  nobs_val <- nobs(models_list[[i]])
  nobs_formatted <- format(nobs_val, big.mark = ",", scientific = FALSE)

  if (i == 1) {
    nobs_line <- paste0(nobs_line, " & ", nobs_formatted)
  } else {
    nobs_line <- paste0(nobs_line, " & ", nobs_formatted)
  }
}
nobs_line <- paste0(nobs_line, " \\\\")
tex_lines <- c(tex_lines, nobs_line, "\\bottomrule", "\\end{tabular}")

tex_lines <- tex_lines[tex_lines != ""]
tex_tabular <- paste(tex_lines, collapse = "\n")

output_path <- here::here("temp_results", "tables", "het_exp_median_biz_median_aug_updated.tex")
if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}
writeLines(tex_tabular, output_path)

# Output sum coefficients
if (!is.null(linear_combinations[[1]])) {
  sum_coef_model1 <- linear_combinations[[1]]$sum_coef
  sum_coef_model1_formatted <- sprintf("$%.3f$%%", sum_coef_model1)
  writeLines(sum_coef_model1_formatted, here::here("temp_results", "numbers", "het_exp_median_biz_median_b1+b3.tex"))
}

if (!is.null(linear_combinations[[2]])) {
  sum_coef_model2 <- linear_combinations[[2]]$sum_coef
  sum_coef_model2_formatted <- sprintf("$%.3f$%%", sum_coef_model2)
  writeLines(sum_coef_model2_formatted, here::here("temp_results", "numbers", "het_exp_median_biz_median_b1+b3_ctrl.tex"))
}

nobs_val <- nobs(models_list[[1]])
nobs_formatted <- sprintf("$%s$%%", format(nobs_val, big.mark = ",", scientific = FALSE))
writeLines(nobs_formatted, here::here("temp_results", "numbers", "survey_n_reminder_exp_savings.tex"))

#-----------------------------------------------------------------------------------------
# Output - Trust dummy and volatility measures
#-----------------------------------------------------------------------------------------

model_trust_volatility_median <- lm(
  trust_scale_1_binary ~ volatility_weekly_above_median,
  data = weekly_data_trust_successful
)

model_trust_volatility_z <- lm(
  trust_scale_1_binary ~ past_year_volatility_weekly_z,
  data = weekly_data_trust_successful
)

models_list_trust <- list(
  model_trust_volatility_median,
  model_trust_volatility_z
)

coef_map_ordered_trust <- c(
  "volatility_weekly_above_median" = "Above-median volatility",
  "past_year_volatility_weekly_z" = "Volatility (z-score)",
  "(Intercept)" = "Intercept"
)

tex_tabular_trust <- modelsummary(
  models_list_trust,
  vcov = "HC1",
  stars = c("*" = .1, "**" = .05, "***" = 0.01),
  coef_map = coef_map_ordered_trust,
  gof_map = "nobs",
  fmt = 3,
  escape = FALSE,
  output = "latex_tabular"
)

tex_tabular_trust <- gsub("Num\\.Obs\\.", "Number of firms", tex_tabular_trust)
tex_tabular_trust <- gsub("Num\\.Obs", "Number of firms", tex_tabular_trust)
tex_tabular_trust <- gsub("\\\\vphantom\\{[12]\\}", "", tex_tabular_trust)

tex_tabular_trust <- gsub(
  "(-?[0-9]+\\.[0-9]+)(\\*+)?",
  "$\\1$\\2",
  tex_tabular_trust,
  perl = TRUE
)

tex_tabular_trust <- gsub(
  "\\((-?[0-9]+\\.[0-9]+)\\)",
  "($\\1$)",
  tex_tabular_trust,
  perl = TRUE
)

tex_lines_trust <- strsplit(tex_tabular_trust, "\n")[[1]]
toprule_idx_trust <- grep("\\\\toprule", tex_lines_trust)

if (length(toprule_idx_trust) > 0) {
  tabular_idx <- grep("\\\\begin\\{tabular\\}", tex_lines_trust)
  if (length(tabular_idx) > 0) {
    tex_lines_trust[tabular_idx] <- "\\begin{tabular}[t]{lcc}"
  }

  header_line1 <- "  & \\multicolumn{2}{c}{High trust dummy} \\\\"
  header_line2 <- "  \\cmidrule(lr){2-3}"
  header_line3 <- "  & (1) & (2) \\\\"
  current_header_idx_trust <- toprule_idx_trust + 1

  tex_lines_trust <- c(
    tex_lines_trust[1:toprule_idx_trust],
    header_line1,
    header_line2,
    header_line3,
    tex_lines_trust[(current_header_idx_trust + 1):length(tex_lines_trust)]
  )
}

tex_lines_trust <- tex_lines_trust[tex_lines_trust != ""]
tex_tabular_trust <- paste(tex_lines_trust, collapse = "\n")

output_path_trust <- here::here("temp_results", "tables", "trust_volatility.tex")
if (!dir.exists(dirname(output_path_trust))) {
  dir.create(dirname(output_path_trust), recursive = TRUE)
}
writeLines(tex_tabular_trust, output_path_trust)

#-----------------------------------------------------------------------------------------
# Pending: Balance test in the survey regression subsample
#-----------------------------------------------------------------------------------------

monthly_validation <- monthly_validation %>%
  distinct(organization_uuid, .keep_all = TRUE)

august_uuids <- august_2020_validation_for_reg$organization_uuid

monthly_validation <- monthly_validation %>%
  mutate(answer_q13 = ifelse(organization_uuid %in% august_uuids, 1, 0))

# Regressions on reminder == 1 subsample
reminder_subsample <- monthly_validation[monthly_validation$reminder == 1, ]

dep_vars <- c("answer_q13", "dont_know", "refuse")

models_list_q13 <- list()

for (i in seq_along(dep_vars)) {
  formula_str <- paste0(dep_vars[i], " ~ anticipated_reminder")
  models_list_q13[[i]] <- feols(as.formula(formula_str),
                                data = reminder_subsample,
                                vcov = "hetero")
}

# Build LaTeX table
tex_lines_q13 <- c(
  "\\begin{tabular}{l*{3}{>{\\centering\\arraybackslash}p{3cm}}}",
  "\\toprule",
  "  & \\multicolumn{3}{c}{Outcome variables} \\\\",
  "  \\cmidrule(lr){2-4}",
  paste0("  & ", paste(dep_vars, collapse = " & "), " \\\\"),
  "\\midrule"
)

coef_name <- "anticipated_reminder"
label_line1 <- "Announced reminder"
label_line2 <- ""
coef_line <- label_line1
se_line <- label_line2
p_line <- "p-value"

for (i in 1:length(models_list_q13)) {
  model <- models_list_q13[[i]]
  result <- extract_coef_se(model, coef_name)

  if (!is.na(result$coef)) {
    coef_val <- sprintf("$%.3f$%s", result$coef, result$stars)
    se_val <- sprintf("($%.3f$)", result$se)

    model_summary <- summary(model)
    coef_vector <- coef(model)
    se_vector <- sqrt(diag(vcov(model)))
    if (coef_name %in% names(coef_vector)) {
      t_stat <- coef_vector[coef_name] / se_vector[coef_name]
      df <- model_summary$df_residual
      if (is.null(df) || length(df) == 0) {
        df <- nobs(model) - length(coef_vector)
      }
      p_val <- 2 * (1 - pt(abs(t_stat), df = df))
      p_val_formatted <- sprintf("$[%.3f]$", p_val)
    } else {
      p_val_formatted <- ""
    }
  } else {
    coef_val <- ""
    se_val <- ""
    p_val_formatted <- ""
  }

  if (i == 1) {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(" & ", se_val)
    p_line <- paste0(p_line, " & ", p_val_formatted)
  } else {
    coef_line <- paste0(coef_line, " & ", coef_val)
    se_line <- paste0(se_line, " & ", se_val)
    p_line <- paste0(p_line, " & ", p_val_formatted)
  }
}

coef_line <- paste0(coef_line, " \\\\")
se_line <- paste0(se_line, " \\\\")
p_line <- paste0(p_line, " \\\\")
tex_lines_q13 <- c(tex_lines_q13, coef_line, se_line, p_line)

tex_lines_q13 <- c(tex_lines_q13, "\\midrule")

# Add unannounced reminder means
means_line <- "Unannounced reminder mean"

for (i in 1:length(dep_vars)) {
  mean_val <- mean(reminder_subsample[[dep_vars[i]]][reminder_subsample$anticipated_reminder == 0], na.rm = TRUE)
  mean_formatted <- sprintf("$%.3f$", mean_val)

  if (i == 1) {
    means_line <- paste0(means_line, " & ", mean_formatted)
  } else {
    means_line <- paste0(means_line, " & ", mean_formatted)
  }
}
means_line <- paste0(means_line, " \\\\")
tex_lines_q13 <- c(tex_lines_q13, means_line)

# Number of observations
nobs_line <- "Number of firms"
for (i in 1:length(models_list_q13)) {
  nobs_val <- nobs(models_list_q13[[i]])
  nobs_formatted <- format(nobs_val, big.mark = ",", scientific = FALSE)

  if (i == 1) {
    nobs_line <- paste0(nobs_line, " & ", nobs_formatted)
  } else {
    nobs_line <- paste0(nobs_line, " & ", nobs_formatted)
  }
}
nobs_line <- paste0(nobs_line, " \\\\")
tex_lines_q13 <- c(tex_lines_q13, nobs_line)

tex_lines_q13 <- c(tex_lines_q13, "\\bottomrule", "\\end{tabular}")
tex_lines_q13 <- tex_lines_q13[tex_lines_q13 != ""]
tex_tabular_q13 <- paste(tex_lines_q13, collapse = "\n")

output_path_q13 <- here::here(
  "temp_results",
  "tables",
  "reminder_response_balance.tex"
)

if (!dir.exists(dirname(output_path_q13))) {
  dir.create(dirname(output_path_q13), recursive = TRUE)
}

writeLines(tex_tabular_q13, output_path_q13)

#-----------------------------------------------------------------------------------------
