#--------------------------------------------------------------------------------------------
# File name: 		      11a_takeup_pooled_reminder.R
# Creation date:      2022-09-15
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "fintech_fee.csv")
# Files created:
#   - here("results", "tables", "effect_timing_all_treatment.tex")
# Purpose: 
#   - Table C.5: Effects of Reminder, Announced Reminder, and Deadline on Days 1–6 vs. 7–8
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(fixest)
library(data.table)
library(modelsummary)
library(lubridate)
library(multcomp)
library(conflicted)
library(qs)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
group_colors <- read_csv(here("proc", "colors.csv"))
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("last", "dplyr")
conflicts_prefer(magrittr::set_names)
#########################################################

#################################################################
##    (1): Table with pooled regressions of days 1-6 vs. 7-8.  ##
#################################################################
# (1.1): Import treatment assignment and take-up data.
fintech_fee <- fread(here("proc", "fintech_fee.csv"))

# (1.2): Create pre/post datasets.
pre_post <- function(data) {
  data %>% 
    select(colnames(data)[!str_detect(colnames(data), "accept_fee_cum")],
           accept_fee_cum2020_09_29, accept_fee_cum2020_09_30, 
           accept_fee_cum2020_10_01, accept_fee_cum2020_10_02, 
           accept_fee_cum2020_10_03, accept_fee_cum2020_10_04, 
           accept_fee_cum2020_10_05, accept_fee_cum2020_10_06) %>% 
    pivot_longer(cols = contains("accept_fee_cum")) %>%
    mutate(date = ymd(str_extract("2020.*$", string = name)),
           post = ifelse(date >= ymd("2020-10-05"), 1, 0)) %>% 
    rename(accepted_offer = value)
}
fintech_fee_prepost_dl <- fintech_fee %>% 
  filter(deadline == 1 | no_deadline == 1) %>% 
  select(organization_uuid, treat_type, treat_description, 
         starts_with("accept_fee_cum"), deadline, no_deadline, strata_fe) %>% 
  pre_post()
fintech_fee_prepost_rem <- fintech_fee %>% 
  filter(reminder == 1 | no_reminder == 1) %>% 
  select(organization_uuid, treat_type, treat_description, 
         reminder, open_reminder_ontime, starts_with("accept_fee_cum"), strata_fe) %>% 
  pre_post()
fintech_fee_prepost_antrem <- fintech_fee %>% 
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
  select(organization_uuid, treat_type, treat_description, 
         anticipated_reminder, unanticipated_reminder, starts_with("accept_fee_cum"), strata_fe) %>% 
  pre_post()

# (1.3): Run regressions.
# Treatment
rem_reg_1_tr <- feols(fml = accepted_offer ~ Treatment + Treatment:post,
                   fixef = c("date", "strata_fe"),
                   cluster = "organization_uuid",
                   data = fintech_fee_prepost_rem |>
                     rename(Treatment = reminder))
antrem_reg_1_tr <- feols(fml = accepted_offer ~ Treatment + Treatment:post,
                      fixef = c("date", "strata_fe"),
                      cluster = "organization_uuid",
                      data = fintech_fee_prepost_antrem |>
                        rename(Treatment = anticipated_reminder))
dl_reg_1_tr <- feols(fml = accepted_offer ~ Treatment + Treatment:post,
                  fixef = c("date", "strata_fe"),
                  cluster = "organization_uuid",
                  data = fintech_fee_prepost_dl |>
                    rename(Treatment = deadline))

extract_coeffs_se_pvalue_dataset <- function(model, model_name) {
  # Extract coefficients, standard errors, and p-values
  coef_data <- model$coefficients
  se_data <- model$se
  p_values <- model$coeftable[, "Pr(>|t|)"]
  num_firms <- nobs(model)  # Number of observations

  # Add stars based on p-values
  stars <- ifelse(
    p_values < 0.01, "***",
    ifelse(p_values < 0.05, "**",
           ifelse(p_values < 0.1, "*", ""))
  )
  coef_starred <- paste0(sprintf("%.3f", coef_data), stars)

  # Combine into a data frame
  df <- data.frame(
    Variable = names(coef_data),
    Coefficient = coef_data,
    Coefficient_Starred = coef_starred,
    Std_Error = se_data,
    P_Value = p_values,
    Num_Firms = num_firms,
    Model = model_name
  )

  return(df)
}

rename_treatments <- c(
  "Treatment" = "Treatment",
  "Treatment:post" = "Treatment $\\times$ Day 7 or 8"
)


results_list <- list(
  rem_reg_1_tr,
  antrem_reg_1_tr,
  dl_reg_1_tr
)

model_names <- c(
  "(1)", "(2)", "(3)"
)

# Use the function to extract data for all models
all_results <- purrr::map2_dfr(results_list, model_names, extract_coeffs_se_pvalue_dataset)

all_results <- all_results %>%
  mutate(Variable = recode(Variable, !!!rename_treatments),
         Variable = sprintf("%s", Variable))

table_data <- all_results %>%
  select(Model, Variable, Coefficient_Starred, Std_Error) %>%
  mutate(Coefficient_Starred = sprintf("%s", Coefficient_Starred),
         Std_Error = sprintf("(%.3f)", Std_Error)) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(Coefficient_Starred, Std_Error),
    names_glue = "{Model}_{.value}"
  )

# Calculate Number of Firms
datasets_list <- list(
  fintech_fee_prepost_rem,
  fintech_fee_prepost_antrem,
  fintech_fee_prepost_dl
)

num_firms_unique <- purrr::map_chr(datasets_list, ~ comma(length(unique(.x$organization_uuid))))

num_firms <- purrr::map_chr(results_list, ~ comma(nobs(.)))

# Calculate Mean Control Take-Up on Day 6
mean_control_takeup <- c(
  fintech_fee %>%
    filter(no_reminder == 1) %>%
    pull(accept_fee_cum2020_10_04) %>%
    mean(),

  fintech_fee %>%
    filter(unanticipated_reminder == 1) %>%
    pull(accept_fee_cum2020_10_04) %>%
    mean(),

  fintech_fee %>%
    filter(no_deadline == 1) %>%
    pull(accept_fee_cum2020_10_04) %>%
    mean()
)

# Format the means to two decimal places
mean_control_takeup_formatted <- sprintf("%.3f", mean_control_takeup)

#num_firms <- purrr::map_chr(results_list, ~ sprintf("%d", nobs(.)))
latex_code <- paste0(
  "\\begin{tabular}{l", paste(rep("c", 4), collapse = ""), "}\n",
  "\\toprule\n",
  " & \\multicolumn{3}{c}{Firm accepted offer} \\\\\n",
  "\\cmidrule(lr){2-4}\n",
  "\\multicolumn{1}{r}{Treatment:} & \\multicolumn{1}{c}{Reminder} & \\multicolumn{1}{c}{Announced reminder} & \\multicolumn{1}{c}{Deadline} \\\\\n",
  " & ", paste(model_names, collapse = " & "), " \\\\\n",
  "\\midrule\n",
  paste(
    apply(
      table_data %>%
        mutate(across(-Variable, ~replace_na(.x, ""))) %>%
        select(Variable, ends_with("Coefficient_Starred"), ends_with("Std_Error")),
      1,
      function(row) {
        n <- 4
        row_content <- paste(row, collapse = " & ")
        parts <- strsplit(row_content, " & ")
        for (i in seq(n, length(parts[[1]]), by = n + 1)) {
          parts[[1]][i] <- paste(parts[[1]][i], "\\\\")
        }
        paste(parts[[1]], collapse = " & ")
      }
    ),
    collapse = " \\\\\n"
  ),
  " \\\\\n",
  "\\midrule\n",
  "Number of observations & ", paste(num_firms, collapse = " & "), " \\\\\n",
  "Number of firms & ", paste(num_firms_unique, collapse = " & "), " \\\\\n",
  "Cumulative take-up by day 6 & ", paste(mean_control_takeup_formatted, collapse = " & "), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}"
)
cat(latex_code)

writeLines(latex_code, here("results", "tables", "effect_timing_all_treatment.tex"))



