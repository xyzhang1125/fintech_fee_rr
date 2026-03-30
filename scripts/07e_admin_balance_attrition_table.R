#--------------------------------------------------------------------------------------------
# File name: 		      07e_admin_balance_attrition_table.R
# Creation date:      
# Author:          		Mohammad Atif Haidry
# Files used:
# 	- here("proc", "08_randomization.csv")
# 	- here("data", "Sent_20201202", "daily", "daily.csv")
# Files created:
# 	- here("results", "numbers", "initial_sample.tex")
# 	- here("results", "tables", "table_attrition_fastdl.tex")
# Purpose:
# 	- Table C.1: Treatment Balance (Attrition Test)
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(here)
library(tidyverse)
library(magrittr)
library(assertthat)
library(readxl)
library(lubridate)
library(purrr)
library(fastDummies)
library(data.table)
library(fixest)
library(modelsummary)
library(tabulator)
library(conflicted)
conflict_prefer("month", "lubridate")
conflict_prefer("filter", "dplyr")
conflict_prefer("magrittr", "purrr")
conflicted::conflicts_prefer(dplyr::last)
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "myfunctions.R"), 
       encoding = "UTF-8")
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
options(scipen = 8888)
#########################################################

#################################################
##    (1): Read in data and define functions.  ##
#################################################

# randomization
randomization <- read_csv(here("proc", "08_randomization.csv"))
nrow(randomization) %>%
  print_n("initial_sample.tex", "Initial sample size we had")
daily_first <- fread(here("data", "Sent_20201202", "daily", "daily.csv"))
fintech_users <- unique(daily_first$organization_uuid)

# Creating an absence variable that would be our dependent variable
randomization <- randomization %>%
  mutate(
    absence = if_else(organization_uuid %in% fintech_users, 0, 1),
    absence = if_else(organization_uuid == "0d7623ca-977f-11e9-8cec-62d790593222", 1, absence)
  )


####################################
##    (2): Process main results.  ##
####################################

# (2.10): Define strata fixed effects.
# Note that here we are using the "sales quartiles" that we stratified on, but these are not actually the sales quartiles due to coding error. 
randomization %<>% mutate(strata_fe = paste(sales_quartiles, new_buss_type, sep = "_"))
assert_that(length(unique(randomization$strata_fe)) == 21) 

# (2.11): Create indicators for group types (includes fee group).
randomization %<>% 
  mutate(group_fac = paste("group", group_id, sep = "_")) %>%
  dummy_cols("group_fac") %>% 
  select(-group_fac) %>% 
  rename_all(~str_remove(., "group_fac_"))

# (2.12): Create indicators for more broad group types (pool together fee group).
randomization %<>%
  mutate(treat_fac = paste("treat", treat_type, sep = "_")) %>%
  dummy_cols("treat_fac") %>%
  select(-treat_fac) %>% 
  rename_all(~str_remove(., "treat_fac_"))

# (2.13): Create indicators for pooled treat types.
randomization %<>% 
  mutate(treat_id = as.numeric(gsub("T", "", treat_type)),
         control = as.numeric(treat_id == 1),
         reminder = as.numeric(treat_id == 3 | treat_id == 4 | treat_id == 6 | treat_id == 7),
         no_reminder = as.numeric(treat_id == 2 | treat_id == 5),
         deadline = as.numeric(treat_id == 5 | treat_id == 6 | treat_id == 7),
         no_deadline = as.numeric(treat_id == 2 | treat_id == 3 | treat_id == 4),
         anticipated_reminder = as.numeric(treat_id == 3 | treat_id == 6),
         unanticipated_reminder = as.numeric(treat_id == 4 | treat_id == 7),
         fast_deadline = as.numeric(treat_id == 8))

# # (2.18): Create variables for fee.
 randomization %<>% 
   mutate(fee_2.75 = as.numeric(fee_type == "2.75% offer"))

randomization <- randomization %>%
  mutate(fee_2.75 = if_else(is.na(fee_2.75), 0, fee_2.75))

# Load the data

fintech_attrition_fee <- randomization
run_regression <- function(dependent_variable, independent_variables, data) {
  formula <- as.formula(paste(dependent_variable, "~", paste(independent_variables, collapse = " + ")))
  regression <- feols(fml = formula, data = data, 
                      #fixef = "strata_fe", 
                      se = "hetero")
  return(regression)
}


regression1 <- run_regression(dependent_variable = "absence", 
                              independent_variables = c("anticipated_reminder", "unanticipated_reminder", "deadline", "fast_deadline", "fee_2.75"),
                              data = fintech_attrition_fee)
f_stat <- fitstat(regression1, "f")
print(f_stat)
f_stat_value <- f_stat[["f"]][["stat"]]
f_stat_p_value <- f_stat[["f"]][["p"]]
f_stat_row <- paste0("$F$-statistic & ", format(f_stat_value, digits = 4), "\\\\")
f_stat_p_row <- paste0("& ", "[", format(f_stat_p_value, digits = 3), "]", "\\\\")

model_list<-list(Attrit = regression1)
options("modelsummary_stars_note" = FALSE)
coef_names <- c("Intercept", "Unannounced reminder", "Announced reminder", "Deadline", "Same-day deadline", paste("2.75% Fee"))%>% 
  purrr::set_names(c("(Intercept)", "unanticipated_reminder", "anticipated_reminder", "deadline", "fast_deadline", "fee_2.75"))
current_table <- model_list %>% 
  purrr::set_names(c("Excluded")) %>%
  mshow(coef_map = coef_names,
        output = "latex", fmt = 4)

current_table %>% 
  float_here() %>%
  remove_names() %>% 
  insert_col_numbers(col_numbers = TRUE) %>% 
  add_caption_label_center(caption = "Balance in Probability of Exclusion",
                           label = "table_attrition") %>%
  replace_numobs("Number of observations") %>%
  write(here("results", "tables", "table_attrition_fastdl.tex"))

table_attrition_table <- readLines(here("results", "tables", "table_attrition_fastdl.tex"))
table_attrition_table <- gsub("\\\\vphantom\\{1\\}", "", table_attrition_table)
table_attrition_table <- gsub("\\\\vphantom\\{2\\}", "", table_attrition_table)
table_attrition_table <- gsub("34010", "34,010", table_attrition_table)
# remove begin table
table_attrition_table <- table_attrition_table[!grepl("\\\\begin\\{table\\}", table_attrition_table)]
table_attrition_table <- table_attrition_table[!grepl("\\\\end\\{table\\}", table_attrition_table)]
# remove centering
table_attrition_table <- table_attrition_table[!grepl("\\\\centering", table_attrition_table)]

num_obs_index <- grep("Number of observations", table_attrition_table)
table_attrition_table <- append(table_attrition_table, f_stat_p_row, after = num_obs_index - 1)
table_attrition_table <- append(table_attrition_table, f_stat_row, after = num_obs_index - 1)
writeLines(table_attrition_table, con = here("results", "tables", "table_attrition_fastdl.tex"))

