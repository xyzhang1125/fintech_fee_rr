#--------------------------------------------------------------------------------------------
# File name: 		      07c_admin_balance_tables.R
# Creation date:      2021-12-28
# Author:          		César Landín, Mohammad Atif Haidry, and Xinyu Zhang
# Files used:
# 	- here("proc", "balance_table_data.csv")
# Files created:
#   - here("results", "tables", "baseline_treatment_balance_fastdl.tex")
# 	- here("numbers", "balance_valid_volume_usd_w5.tex")
# 	- here("numbers", "balance_nr_valid_payments_w5.tex")
# Purpose:
# 	- Table 1: Baseline Treatment Balance (Full Sample): Generate balance tables comparing baseline covariates and business characteristics.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(fixest)
library(modelsummary)
library(kableExtra)
source(here("scripts", "programs", "07b_balance_table_functions.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

######################################
##    (1): Baseline balance table.  ##
######################################
# (1.1): Import balance table data.
balance_data <- read_csv(here("proc", "balance_table_data.csv"))

# # (1.2): Generate tables.
table_content <- lapply(balance_outcomes, balance_table_treat_row_fastdl, balance_data) %>% 
  bind_rows() %>% 
  balance_table_treat_proc_fastdl() %>%
  fix_panel_titles_correct()
# export
writeLines(table_content, con = here("results", "tables", "baseline_treatment_balance_fastdl.tex"))


# (1.3): Get valid volume and payments means.
get_means <- function(var) {
  balance_data %>% pull(var) %>% mean() %>% comma_format()
}

# (1.4): Define numbers for footnotes.
get_means("valid_volume_usd_w5") %>% 
  print_n("balance_valid_volume_usd_w5.tex",
          "Average baseline winsorized monthly sales volume in USD")
get_means("nr_valid_payments_w5") %>% 
  print_n("balance_nr_valid_payments_w5.tex",
          "Average baseline winsorized number of transactions")
