#--------------------------------------------------------------------------------------------
# File name: 		      15a_survey_measure_tables.R
# Creation date:      2023-03-28
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
#   - here("proc", "survey_successful.csv")
#   - here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("proc", "survey_successful_heterogeneity.dta")
# Purpose:
#   - Creating .dta dataset for romanowolf analysis in .do file. 
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(here)
library(tidyverse)
library(magrittr)
library(haven)
library(conflicted)

options(modelsummary_format_numeric_latex = "plain",
        modelsummary_stars_note        = FALSE)
options(readr.show_col_types        = FALSE)

source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))

conflict_prefer("select",    "dplyr")
conflict_prefer("filter",    "dplyr")
conflict_prefer("last",      "dplyr")
conflict_prefer("set_names", "magrittr")
#########################################################


######################################################################################
##  (1): Regression tables: test correlation between take-up and survey measures.   ##
######################################################################################
# (1.1): Import survey and take-up data.
survey_successful <- read_csv(here("proc", "survey_successful.csv"))
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, accepted_offer_ontime, accepted_offer_firstday, accepted_offer_late, treat_type, treat_description,
         reminder, deadline, anticipated_reminder, unanticipated_reminder, no_deadline, no_reminder)

# (1.2): Compare survey vs. full sample takeup.
survey_successful %>% summarise_at(vars(contains("accepted")), mean)
fintech_fee %>%
  filter(!str_detect(treat_description, "Control|24")) %>%
  summarise_at(vars(contains("accepted")), mean)

# (1.3): Merge offer acceptance variable.
survey_successful %<>%
  select(organization_uuid, contains("binary"), req_ans_q12.11, ipw_takeup) %>% 
  left_join(fintech_fee, by = "organization_uuid")

# (1.4): Drop missing values.
survey_successful %<>%
  filter(!is.na(trust_scale_1_binary)) %>%
  filter(!is.na(trust_scale_2_binary)) %>%
  filter(!is.na(reciprocity_scale_binary)) %>%
  filter(!is.na(procrastination_scale_binary)) %>%
  filter(!is.na(memory_scale_1_binary)) %>%
  filter(!is.na(memory_scale_2_binary)) %>%
  filter(!is.na(attention_scale_binary))  

write_dta(survey_successful |> 
            select(-req_ans_q12.11), here("proc", "survey_successful_heterogeneity.dta"))

