#--------------------------------------------------------------------------------------------
# File name: 		      x03_editor_01_admin_data_cleaned.R
# Creation date:      2026-02-16
# Author:          		Xinyu Zhang
# Purpose:            Data preparation + volatility HTE on adoption and related tests
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

font_add(family = "Times New Roman", regular = "times.ttf")
showtext_auto()

conflict_prefer("last", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(magrittr::set_names)

options(readr.show_col_types = FALSE)

source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "22a_admin_graphs_functions.R"))
source(here("scripts", "programs", "22b_admin_panels_functions.R"))
source(here("scripts", "x00a_editor_01_table_function.R"))
source(here("scripts", "programs", "table_functions.R"))

# =============================
# 2. DATA IMPORT
# =============================

# Administrative data
fintech_fee <- fread(
  here("proc", "fintech_fee_light.csv"),
  select = c("accepted_offer_ontime", "accepted_offer_late", "organization_uuid", "treat_type",
             "takeup_date", "open_email_date", "strata_fe", "control",
             "reminder", "no_reminder", "anticipated_reminder",
             "unanticipated_reminder", "sales", "valid_volume_aug2020")
)

# Create reminder groups
fintech_fee %<>%
  mutate(antrem_group = case_when(anticipated_reminder == 1 ~ "announced_reminder",
                                  unanticipated_reminder == 1 ~ "unannounced_reminder"))

# Balance test data for business relationship
balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>%
  mutate(antrem = ifelse(anticipated_reminder, "Announced reminder", "Unannounced reminder"))

# Weekly sales data
weekly_data <- qs::qread(here("proc", "fintech_fee_weekly.qs"))[, c('organization_uuid', 'timestamp_week', 'valid_volume_w5',
                                                                    'valid_volume', 'commission_model', 'treat_type', 'fee_type', 'accepted_offer_ontime',
                                                                    'anticipated_reminder', 'unanticipated_reminder')]
weekly_data %<>%
  mutate(antrem_group = case_when(anticipated_reminder == 1 ~ "announced_reminder",
                                  unanticipated_reminder == 1 ~ "unannounced_reminder"))

# Filter for reminder sample
weekly_data_antrem <- weekly_data %>%
  filter(!is.na(antrem_group))

# =============================
# 3. CALCULATE VOLATILITY MEASURES
# =============================

# Calculate volatility for reminder sample
weekly_data_antrem <- weekly_data_antrem %>%
  mutate(in_analysis_period = timestamp_week >= "2019-09-02" & timestamp_week <= "2020-08-31") %>%
  group_by(organization_uuid) %>%
  mutate(
    past_year_sales_mean_weekly = mean(valid_volume[in_analysis_period], na.rm = TRUE),
    past_year_sales_sd_weekly = sd(valid_volume[in_analysis_period], na.rm = TRUE),
    past_year_volatility_weekly = past_year_sales_sd_weekly / past_year_sales_mean_weekly
  ) %>%
  ungroup() %>%
  filter(timestamp_week == as.Date("2020-08-31")) %>%
  # Calculate z-score for volatility
  mutate(
    past_year_volatility_weekly_z = (past_year_volatility_weekly - mean(past_year_volatility_weekly, na.rm = TRUE)) /
      sd(past_year_volatility_weekly, na.rm = TRUE)
  )

# =============================
# 4. CREATE BINARY AND NORMALIZED VOLATILITY VARIABLES
# =============================

# Create binary indicator for above-median volatility
weekly_data_antrem <- weekly_data_antrem %>%
  mutate(
    weekly_volatility_above_median = case_when(
      past_year_volatility_weekly > median(past_year_volatility_weekly, na.rm = TRUE) ~ 1,
      past_year_volatility_weekly <= median(past_year_volatility_weekly, na.rm = TRUE) ~ 0,
      is.na(past_year_volatility_weekly) ~ NA_real_
    )
  )

# Create demeaned volatility
weekly_data_antrem <- weekly_data_antrem %>%
  mutate(past_year_volatility_weekly_demeaned =
           past_year_volatility_weekly - mean(past_year_volatility_weekly, na.rm = TRUE))

# =============================
# 5. MERGE BUSINESS RELATIONSHIP DATA
# =============================

weekly_data_antrem <- weekly_data_antrem %>%
  left_join(
    balance_data %>%
      select(organization_uuid, months_since_ft) %>%
      distinct(),
    by = "organization_uuid"
  )

# Create business relationship variables
weekly_data_antrem <- weekly_data_antrem %>%
  mutate(
    biz_relation_above_median = case_when(
      months_since_ft > median(months_since_ft, na.rm = TRUE) ~ 1,
      months_since_ft <= median(months_since_ft, na.rm = TRUE) ~ 0,
      is.na(months_since_ft) ~ NA_real_
    ),
    months_since_ft_z = (months_since_ft - mean(months_since_ft, na.rm = TRUE)) / sd(months_since_ft, na.rm = TRUE)
  )

# =============================
# 6. PREPARE REGRESSION DATASET
# =============================

# Merge volatility measures to main dataset
fintech_fee_antrem_reg <- fintech_fee %>%
  left_join(
    weekly_data_antrem %>%
      select(organization_uuid,
             past_year_volatility_weekly,
             past_year_sales_mean_weekly,
             past_year_sales_sd_weekly,
             weekly_volatility_above_median,
             past_year_volatility_weekly_z,
             biz_relation_above_median,
             months_since_ft_z) %>%
      distinct(organization_uuid, .keep_all = TRUE),
    by = "organization_uuid") %>%
  filter(!is.na(antrem_group))

# =============================
# 7. CREATE VOLATILITY QUANTILE VARIABLES
# =============================

fintech_fee_antrem_reg <- fintech_fee_antrem_reg %>%
  mutate(
    volatility_quantile = ntile(past_year_volatility_weekly, 4),
    vol_Q1 = ifelse(volatility_quantile == 1, 1, 0),
    vol_Q2 = ifelse(volatility_quantile == 2, 1, 0),
    vol_Q3 = ifelse(volatility_quantile == 3, 1, 0),
    vol_Q4 = ifelse(volatility_quantile == 4, 1, 0)
  )

#-----------------------------------------------------------------------------------------
# FORMAL OUTPUT - Heterogeneity analysis with three model specifications
#-----------------------------------------------------------------------------------------

# ===============================
# 1. Set three models
# ===============================

# Model 1: Above-median volatility specification
model_weekly_median <- feols(
  accepted_offer_late ~ weekly_volatility_above_median + anticipated_reminder +
    weekly_volatility_above_median:anticipated_reminder | strata_fe,
  data = fintech_fee_antrem_reg,
  cluster = ~organization_uuid
)

# Model 2: Volatility quantiles specification (Q1 baseline)
model_vol_quantiles_q1_base <- feols(
  accepted_offer_late ~ anticipated_reminder +
    vol_Q2 + vol_Q3 + vol_Q4 +
    anticipated_reminder:vol_Q2 +
    anticipated_reminder:vol_Q3 +
    anticipated_reminder:vol_Q4 |
    strata_fe,
  data = fintech_fee_antrem_reg,
  cluster = ~organization_uuid
)

# Model 3: Z-score volatility specification
model_weekly_vol_z <- feols(
  accepted_offer_late ~ past_year_volatility_weekly_z + anticipated_reminder +
    past_year_volatility_weekly_z:anticipated_reminder | strata_fe,
  data = fintech_fee_antrem_reg,
  cluster = ~organization_uuid
)

# ===============================
# 2. Model list
# ===============================
models <- list(
  model_weekly_median,
  model_vol_quantiles_q1_base,
  model_weekly_vol_z
)

# ===============================
# 3. Extract coefficients and SEs
# ===============================
extract_coef_se <- function(model, coef_name) {
  coef_vector <- coef(model)
  vcov_matrix <- vcov(model)

  if (coef_name %in% names(coef_vector)) {
    coef_val <- coef_vector[coef_name]
    se_val <- sqrt(vcov_matrix[coef_name, coef_name])

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
# 4. Build LaTeX table
# ===============================
tex_lines <- c(
  "\\begin{tabular}[t]{lccc}",
  "\\toprule",
  "  & \\multicolumn{3}{c}{Firm accepted offer} \\\\",
  "  \\cmidrule(lr){2-4}",
  "  & (1) & (2) & (3) \\\\",
  "\\midrule"
)

# Announced reminder coefficient
coef_name <- "anticipated_reminder"
label_line1 <- "Announced reminder"
label_line2 <- ""

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 6. Above-median volatility effect
# ===============================
coef_name <- "weekly_volatility_above_median"
label_line1 <- "Above-median volatility"
label_line2 <- ""

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 7. Interaction: Above-median volatility × Announced reminder
# ===============================
coef_name <- "weekly_volatility_above_median:anticipated_reminder"
label_line1 <- "Above-median volatility"
label_line2 <- "\\hspace{1em} $\\times$ Announced reminder"

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 8. Quartile effects
# ===============================

# Quartile-2 volatility
coef_name <- "vol_Q2"
label_line1 <- "Quartile-2 volatility"
label_line2 <- ""

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# Quartile-3 volatility
coef_name <- "vol_Q3"
label_line1 <- "Quartile-3 volatility"
label_line2 <- ""

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# Quartile-4 volatility
coef_name <- "vol_Q4"
label_line1 <- "Quartile-4 volatility"
label_line2 <- ""

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 9. Quartile interactions with reminder
# ===============================

# Quartile-2 × Announced reminder
coef_name <- "anticipated_reminder:vol_Q2"
label_line1 <- "Quartile-2 volatility"
label_line2 <- "\\hspace{1em} $\\times$ Announced reminder"

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# Quartile-3 × Announced reminder
coef_name <- "anticipated_reminder:vol_Q3"
label_line1 <- "Quartile-3 volatility"
label_line2 <- "\\hspace{1em} $\\times$ Announced reminder"

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# Quartile-4 × Announced reminder
coef_name <- "anticipated_reminder:vol_Q4"
label_line1 <- "Quartile-4 volatility"
label_line2 <- "\\hspace{1em} $\\times$ Announced reminder"

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 10. Z-score volatility treatment
# ===============================
coef_name <- "past_year_volatility_weekly_z"
label_line1 <- "Volatility (z-score)"
label_line2 <- ""

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 11. Z-score interaction
# ===============================
coef_name <- "past_year_volatility_weekly_z:anticipated_reminder"
label_line1 <- "Volatility (z-score)"
label_line2 <- "\\hspace{1em} $\\times$ Announced reminder"

coef_line <- label_line1
se_line <- label_line2

for (i in 1:length(models)) {
  result <- extract_coef_se(models[[i]], coef_name)

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

# ===============================
# 12. Number of firms and footer
# ===============================
tex_lines <- c(tex_lines, "\\midrule")

nobs_line <- "Number of firms"
for (i in 1:length(models)) {
  nobs_val <- nobs(models[[i]])
  nobs_formatted <- format(nobs_val, big.mark = ",", scientific = FALSE)

  if (i == 1) {
    nobs_line <- paste0(nobs_line, " & ", nobs_formatted)
  } else {
    nobs_line <- paste0(nobs_line, " & ", nobs_formatted)
  }
}
nobs_line <- paste0(nobs_line, " \\\\")
tex_lines <- c(tex_lines, nobs_line)

tex_lines <- c(tex_lines, "\\bottomrule", "\\end{tabular}")

tex_lines <- tex_lines[tex_lines != ""]

tex_output <- paste(tex_lines, collapse = "\n")

# ===============================
# 13. Save and export
# ===============================
output_path <- here::here(
  "temp_results",
  "tables",
  "het_adopt_weekly_volatility_3cols.tex"
)

if (!dir.exists(dirname(output_path))) {
  dir.create(dirname(output_path), recursive = TRUE)
}

writeLines(tex_output, output_path)

#-----------------------------------------------------------------------------------------
