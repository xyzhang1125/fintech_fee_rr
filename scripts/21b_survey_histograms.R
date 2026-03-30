#--------------------------------------------------------------------------------------------
# File name: 		      21b_survey_histograms.R
# Creation date:      2021-12-08
# Author:          		César Landín
# Files used:
# 	- here("proc", "survey_successful.csv")
# Files created:
# 	- here("results", "figures", "survey_percent_sales.eps")
# 	- here("results", "numbers", "survey_num_data_percent_sales.tex")
# 	- here("results", "numbers", "survey_num_data_percent_sales_response_sample.tex")
# 	- here("results", "figures", "survey_fee_diff.eps")
# 	- here("results", "numbers", "fee_prior_receive_offer.tex")
# Purpose:
#   - Figure C.5: Percent of Sales Made Through FinTech Provider in Prior Week
#   - Figure C.17: Difference in Pre-Treatment Actual Fee and Perceived Fee
# 	- Generate survey histograms: percent transactions with Zettle, 
#     differences in actual and perceived fees
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(readxl)
library(scales)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # changed to times
showtext_auto()
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "21a_survey_histogram_functions.R"))
source(here("scripts", "programs", "05a_survey_functions.R"))
# source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################



############################################################################################
##  (2): Section 1 (general characteristics): Question 1.9 (percentage sales) histogram.  ##
############################################################################################
# 1.9: What share of your total pesos of sales did you make through iZettle in the past week? #
# Filter for question 1.9: percent_sales > 0

# (2.1): Import survey data.
data_percent_sales <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, percent_sales, req_ans_q1.9) %>% 
  filter(req_ans_q1.9 == 1)
data_percent_sales %>% 
  keep_valid_ans("percent_sales") %>% 
  tab(percent_sales)

# (3.2): Generate and export graph.
data_percent_sales %>% 
  keep_valid_ans("percent_sales") %>% 
  mutate(percent_sales = percent_sales / 100) %>% 
  graph_hist_simple(variable = "percent_sales", 
                    xlab = "Percentage Sales Made Through FinTech Provider",
                    breaks = c(0, seq(0.05, 1, 0.05))) +
  scale_x_continuous(labels = percent, expand = expansion(mult = 0.01)) +
  theme(axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        axis.title.x = element_text(family = "Times New Roman"))
ggsave_(here("results", "figures", "survey_percent_sales.eps"), width = 6, height = 4)

data_percent_sales %>% 
  nrow() %>%
  print_n("survey_num_data_percent_sales.tex", "Number of firms asked the question - What share of your total pesos of sales did you make through (provider) in the past week?", dig = 1)

data_percent_sales %>%
  filter(percent_sales != "-777" & percent_sales != "-888") %>% 
  nrow() %>%
  print_n("survey_num_data_percent_sales_response_sample.tex", "Number of firms asked the question and answered - What share of your total pesos of sales did you make through (provider) in the past week?", dig = 1)


############################################################################################
##  (4): Section 2 (general characteristics): Question 1.10 (fee differences) histogram.  ##
############################################################################################
# 1.10. What was your commission with iZettle the week before you received the offer?
# Filter for question 1.10: None, everyone asked this question.

# (4.1): Import survey data.
data_fee_diff <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, fee_diff, recall_old_fee)
data_fee_diff %>% 
  keep_valid_ans("fee_diff") %>% 
  keep_valid_ans("recall_old_fee") %>% 
  tab(fee_diff)

# (4.2): Generate and export graph.
data_fee_diff %>% 
  keep_valid_ans("fee_diff") %>% 
  keep_valid_ans("recall_old_fee") %>% 
  graph_hist_simple(variable = "fee_diff", 
                    xlab = "Difference in Perceived Fee - Pre-Treatment Actual Fee (pp)") +
  expand_limits(x = c(-4, 4)) + 
  theme(axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        axis.title.x = element_text(family = "Times New Roman"))
ggsave_(here("results", "figures", "survey_fee_diff.eps"), width = 6, height = 4)


nrow(data_fee_diff %>% 
       keep_valid_ans("fee_diff") %>% 
       keep_valid_ans("recall_old_fee")) %>% 
  print_n("fee_prior_receive_offer.tex", "Firms that answered the question -- “What was your fee with (provider) the week before you received the offer?” with a response")


rm(data_fee_diff)
