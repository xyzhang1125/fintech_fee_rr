#--------------------------------------------------------------------------------------------
# File name: 		      10_account_logins.R
# Creation date:      2021-12-08
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "fintech_fee_light.csv")
#   - here("data", "Sent_20210108", "daily_backoffice_data.csv")
#   - here("proc", "balance_table_data.csv")
#   - here("proc", "survey_successful.csv")
# Files created:
#   - here("results", "tables", "account_logins.tex")
# Purpose:
# 	- Table C.14: Account Log ins by Treatment: Analyze effect of treatment on how often firms logged into their account and checked
#     their deposits. 
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(readxl)
library(fixest)
library(modelsummary)
library(data.table)
library(lubridate)
library(kableExtra)
library(conflicted) # added on Sep 20, 2025
conflicts_prefer(lubridate::week)
conflicts_prefer(magrittr::set_names)
conflicts_prefer(dplyr::filter)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
#########################################################

##############################################
##    (1): Import and process logins data.  ##
##############################################
# (1.1): Import and filter logins data.
fintech_fee <- fread(here("proc", "fintech_fee_light.csv"))
access_data <- read_csv(here("data", "Sent_20210108", "daily_backoffice_data.csv")) %>% 
  filter(organization_uuid %in% fintech_fee$organization_uuid) %>% 
  mutate(day = dmy(Day), 
         week = week(day)) %>% 
  select(-Day)

# (1.2): Merge in treatment status and collapse data at the business level.
login_data <- fintech_fee %>% 
  select(organization_uuid, anticipated_reminder, unanticipated_reminder, fast_deadline, deadline, fee_2.75) %>% 
  # Merge with dates
  cross_join(access_data %>% select(day) %>% unique()) %>% 
  # Merge with login data
  left_join(access_data, by = c("organization_uuid", "day")) %>% 
  # Filter dates
  filter(day >= "2020-09-29" & day <= "2020-10-06") %>% 
  select(-day, -week) %>% 
  group_by(organization_uuid, anticipated_reminder, unanticipated_reminder, fast_deadline, deadline, fee_2.75) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  # Generate dummies
  mutate(log_in = if_else(nr_log_ins > 0, 1, 0, 0), 
         view_deposit_section = if_else(nr_times_deposit_section > 0, 1, 0, 0))
   

# (1.5): Calculate above / below median baseline sales.
baseline_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
  select(organization_uuid, valid_volume_w5, valid_volume) %>% 
  mutate(median_sales = median(valid_volume_w5),
         above_median_sales = ifelse(valid_volume_w5 >= median_sales, 1, 0))
baseline_data %>% tab(above_median_sales)
baseline_data %>% select(-organization_uuid) %>% group_by(above_median_sales) %>% summarise_all(mean)
login_data %<>% left_join(baseline_data %>% select(-median_sales), by = "organization_uuid")

# (1.7): Calculate above / below median number of employees.
survey_data <- read_csv(here("proc", "survey_successful.csv")) %>%
  select(organization_uuid, nb_employees)
survey_data %<>%
  filter(nb_employees != -888) %>%
  mutate(above_median_employees = ifelse(nb_employees >= median(nb_employees, na.rm = TRUE), 1, 0),
         more_one_employee = ifelse(nb_employees > 1, 1, 0))
login_data_survey_sample <- survey_data %>%
  left_join(login_data %>%
              select(organization_uuid, nr_log_ins, log_in, view_deposit_section),
            by = "organization_uuid")


# (1.3): Fix missing fee dummies.
login_data %<>%
  replace_na(list(fee_2.75 = 0))

##### Case for between Day-1 and Day-6 #####
# Merge in treatment status and collapse data at the business level.
login_data_d6 <- fintech_fee %>% 
  select(organization_uuid, anticipated_reminder, unanticipated_reminder, fast_deadline, deadline, fee_2.75, strata_fe) %>% 
  # Merge with dates
  cross_join(access_data %>% select(day) %>% unique()) %>% 
  # Merge with login data
  left_join(access_data, by = c("organization_uuid", "day")) %>% 
  # Filter dates
  filter(day >= "2020-09-29" & day <= "2020-10-04") %>% 
  select(-day, -week) %>% 
  group_by(organization_uuid, anticipated_reminder, unanticipated_reminder, fast_deadline, deadline, fee_2.75, strata_fe) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  # Generate dummies
  mutate(log_in = if_else(nr_log_ins > 0, 1, 0, 0), 
         view_deposit_section = if_else(nr_times_deposit_section > 0, 1, 0, 0))

login_data_d6 %<>% left_join(baseline_data %>% select(-median_sales), by = "organization_uuid")

login_data_survey_sample_d6 <- survey_data %>%
  left_join(login_data_d6 %>%
              select(organization_uuid, nr_log_ins, log_in, view_deposit_section),
            by = "organization_uuid")

login_data_d6 %<>%
  replace_na(list(fee_2.75 = 0))

########################################################
##    (2): Simple (non-panel) regressions and table.  ##
########################################################
# (2.1): Run regressions.
# Run regression 
outcomes = c("log_in", "view_deposit_section")
reg_simple <- lapply(outcomes, function(x) {
  feols(fml = as.formula(paste(x, "~ anticipated_reminder + unanticipated_reminder + fast_deadline + deadline + fee_2.75 ")), 
        se = "hetero",
        data = login_data)
})
names(reg_simple) <- c("Firm logged in", "Firm viewed deposits")
mshow(reg_simple)

reg_simple_d6 <- lapply(outcomes, function(x) {
  feols(fml = as.formula(paste(x, "~ anticipated_reminder + unanticipated_reminder + fast_deadline + deadline + fee_2.75 ")), 
        se = "hetero",
        fixef = "strata_fe", 
        data = login_data_d6)
})
names(reg_simple_d6) <- c("Firm logged in", "Firm viewed deposits")
mshow(reg_simple_d6)

# (2.2): Define model and coefficient names.
coef_names <- c("Intercept", "Unannounced reminder", "Announced reminder", "Deadline", "Same-day deadline", "2.75\\% offer")
names(coef_names) <- c("(Intercept)", "unanticipated_reminder", "anticipated_reminder", "deadline", "fast_deadline", "fee_2.75")

# (2.3): Generate and export table.    
gen_base_table_login(models = reg_simple_d6, 
               caption = "Account Log ins by Treatment",
               label = "account_logins",
               coef_names = coef_names,
               only_tabular = TRUE) %>% 
  write_(here("results", "tables", "account_logins_d6.tex"))

# account_logins_table <- readLines(here("results", "tables", "account_logins.tex"))
# account_logins_table <- gsub("\\\\vphantom\\{1\\}", "" , account_logins_table)
# account_logins_table <- gsub("\\\\vphantom\\{2\\}", "" , account_logins_table)
# account_logins_table <- gsub("\\\\adjustbox\\{max width=\\\\textwidth\\}", "\\\\adjustbox\\{max width=1.2\\\\textwidth, center\\}", account_logins_table) %>%
#   write_(here("results", "tables", "account_logins_d6.tex"))

