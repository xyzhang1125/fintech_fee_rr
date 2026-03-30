#--------------------------------------------------------------------------------------------
# File name: 		      07d_survey_balance_tables.R
# Creation date:      2021-10-10
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "survey_all.csv")
# 	- here("proc", "survey_successful.csv")
# 	- here("proc", "balance_table_data.csv")
# 	- here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("results", "tables", "survey_baseline_treatment_balance.tex")
#   - here("results", "tables", "survey_balance_sample.tex")

# Purpose:
#   - Table C.3: Survey Baseline Treatment Balance.
#   - Table C.4: Balance Between Survey Sample and Non-survey Sample.
#   - Generate balance tables testing differences in baseline covariates between general and
#     survey samples, survey respondents and non-respondents, announced and unannounced 
#     reminder, unanticiated and no reminder groups, and deadline and no deadline groups.
#     between the general and survey sample.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(fixest)
library(modelsummary)
library(kableExtra)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "07b_balance_table_functions.R"))
#########################################################

########################################################################
##    (1): Process survey data and generate balance table datasets.   ##
########################################################################
# (1.1): Import survey data.
survey_all <- read_csv(here("proc", "survey_all.csv"))
survey_successful <- read_csv(here("proc", "survey_successful.csv"))

# (1.2): Import balance table data.
balance_data <- read_csv(here("proc", "balance_table_data.csv"))
survey_successful <- survey_successful %>%
  select(organization_uuid) %>%
  mutate(sample_survey = 1)
balance_data_sample <- balance_data %>%
  left_join(survey_successful, by = "organization_uuid") %>%
  mutate(sample_survey = ifelse(is.na(sample_survey), 0, sample_survey),
         sample_full = ifelse(sample_survey == 1, 0, 1))
balance_data_sample %>% tab(sample_full, sample_survey)

# (1.4): Import no deadline / no reminder variables.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, accepted_offer_ontime, accepted_offer_late, treat_type, treat_description,
         no_deadline, no_reminder)

# (1.5): Generate new dataset with variables for subgroup balance tables.
balance_data_survey <- balance_data_sample %>% 
  left_join(fintech_fee %>% 
              select(organization_uuid, accepted_offer_ontime, accepted_offer_late, no_deadline, no_reminder), 
            by = "organization_uuid") %>% 
  filter(sample_survey == 1)
balance_data_survey_unantrem <- balance_data_survey %>% filter(unanticipated_reminder == 1 | no_reminder == 1)
balance_data_survey_antrem <- balance_data_survey %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1)

# (1.6): Generate new dataset with response/non-response comparison.
balance_data_survey_response <- balance_data_sample %>% 
  select(-sample_full, -sample_survey) %>% 
  unique() %>% 
  inner_join(survey_all %>% 
               select(-ends_with(c("deadline", "reminder", "fee_2.75"))), 
             by = "organization_uuid") %>% 
  left_join(survey_successful %>% 
              mutate(survey_response = 1) %>% 
              select(organization_uuid, survey_response),
            by = "organization_uuid") %>% 
  mutate(survey_response = ifelse(!is.na(survey_response), 1, 0),
         survey_noresponse = ifelse(survey_response == 1, 0, 1)) 
balance_data_survey_response %>% tab(survey_response, survey_noresponse)

rm(survey_successful, balance_data)

########################################################################
##    (2): Treatment balance tables (baseline var ~ all treatments).  ##
########################################################################
# (2.1): Generate treatment balance tables.
# Full version
table_content_full <- lapply(balance_outcomes, balance_table_treat_row_no_fastdl, balance_data_survey, remove_2.75 = FALSE) %>% 
  bind_rows() %>% 
  balance_table_treat_proc_no_fastdl_custom(remove_2.75 = FALSE) %>%
  fix_panel_titles_correct()

writeLines(table_content_full, con = here("results", "tables", "survey_baseline_treatment_balance.tex"))

#####################################################################################################
##    (3): Two-group balance tables (announced vs. unannounced, full vs. survey sample, etc.)  ##
#####################################################################################################
# (3.1): Define balance table parameters.
balance_table_parameters <- tribble(
  ~data, ~var1, ~var2, ~model, ~var1_title, ~var2_title, ~diff_title, ~caption, ~label,
  "balance_data_sample", "sample_full", "sample_survey", "~ sample_survey", "Non-survey sample", "Survey sample", "Difference",
  "Balance Between Survey Sample and Non-survey Sample", "survey_balance_sample") %>% 
  as.data.frame()

# (3.2): Generate paper survey balance tables.
for (i in 1:nrow(balance_table_parameters)) {
  print(paste("Working on table", i, "/", nrow(balance_table_parameters)))
  
  # Get dataframe with variable names, means, difference and p-values.
  table_content <- lapply(balance_outcomes, balance_table_row, 
                          data = eval(as.name(balance_table_parameters$data[i])) %>% 
                            select(all_of(c(balance_outcomes, balance_table_parameters$var1[i], balance_table_parameters$var2[i]))),
                          var1 = balance_table_parameters$var1[i],
                          var2 = balance_table_parameters$var2[i],
                          model = balance_table_parameters$model[i]) %>% 
    bind_rows() %>% 
    # Format balance table
    balance_table_format(var1_title = balance_table_parameters$var1_title[i],
                         var2_title = balance_table_parameters$var2_title[i],
                         diff_title = balance_table_parameters$diff_title[i]) %>% 
    # Add caption and label
    add_caption_label_center(caption = balance_table_parameters$caption[i],
                             label = balance_table_parameters$label[i]) %>% 
    # Add panel titles
    fix_panel_titles_correct() %>% 
    # Add omnibus F-test
    add_f_test(data = eval(as.name(balance_table_parameters$data[i])) %>% 
                 select(all_of(c(balance_outcomes, balance_table_parameters$var1[i], balance_table_parameters$var2[i]))), 
               variable = balance_table_parameters$var2[i]) %>% 
    # Add row with number of observations
    add_row_nobs(data = eval(as.name(balance_table_parameters$data[i])),
                 var1 = balance_table_parameters$var1[i],
                 var2 = balance_table_parameters$var2[i]) %>% 
    # Adjust width
    adjust_box() %>% 
    # Adjust height
    adjust_height(0.75)
  
  table_content <- gsub("P-value", "$p$-value", table_content)
  table_content <- gsub("F-stat", "$F$-stat", table_content)
  
  # export
  writeLines(table_content, con = here("results", "tables", str_c(balance_table_parameters$label[i], ".tex")))
}