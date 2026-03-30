#--------------------------------------------------------------------------------------------
# File name: 		      25a_number_calculations.R
# Creation date:      2021-12-29
# Author:          		César Landín
# Files used:
# 	- here("proc", "fintech_fee_light.csv")
# 	- here("proc", "fintech_fee.csv")
# 	- here("proc", "users.rds")
# 	- here("proc", "balance_table_data.csv")
# 	- here("proc", "regs_for_figs.qs")
# 	- here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx")
# Files created:
# 	- here("results", "numbers", "stat_sample_size.tex")
# 	- here("results", "numbers", "stat_sample_size_abstract.tex")
# 	- here("results", "numbers", "stat_number_control.tex")
# 	- here("results", "numbers", "stat_pct_fem.tex")
# 	- here("results", "numbers", "stat_pct_fem_rem.tex")
# 	- here("results", "numbers", "stat_pct_fem_antrem.tex")
# 	- here("results", "numbers", "stat_pct_fem_dl.tex")
# 	- here("results", "numbers", "stat_age_median_rem.tex")
# 	- here("results", "numbers", "stat_age_median_antrem.tex")
# 	- here("results", "numbers", "stat_age_median_dl.tex")
# 	- here("results", "numbers", "stat_pct_small_retailer.tex")
# 	- here("results", "numbers", "stat_pct_prof.tex")
# 	- here("results", "numbers", "avg_takeup_open_email_norem.tex")
# 	- here("results", "numbers", "avg_takeup_open_email_rem.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_3_d1.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_2.75_d1.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_3_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_2.75_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_2.75_d8_pct.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_long.tex")
# 	- here("results", "numbers", "mean_accepted_offer_2.75.tex")
# 	- here("results", "numbers", "mean_accepted_offer_3.00.tex")
# 	- here("results", "numbers", "coef_accepted_offer_rem_d7.tex")
# 	- here("results", "numbers", "coef_accepted_offer_rem_d8.tex")
# 	- here("results", "numbers", "pres_coef_accepted_offer_rem_d8.tex")
# 	- here("results", "numbers", "mean_accepted_offer_norem.tex")
# 	- here("results", "numbers", "pres_mean_accepted_offer_norem.tex")
# 	- here("results", "numbers", "pct_inc_accepted_offer_rem_d8.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_rem_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_antrem_d7.tex")
# 	- here("results", "numbers", "pct_inc_accepted_offer_antrem_d7.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d7.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d7_abstract.tex")
# 	- here("results", "numbers", "pct_inc_accepted_offer_antrem_d8.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d8.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d8_abstract.tex")
# 	- here("results", "numbers", "coef_accepted_offer_antrem_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_unantrem_d8.tex")
# 	- here("results", "numbers", "pct_inc_accepted_offer_unantrem_d8.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_unantrem_d8.tex")
# 	- here("results", "numbers", "pres_pct_inc_accepted_offer_unantrem_d8_abstract.tex")
# 	- here("results", "numbers", "coef_accepted_offer_dl_d1.tex")
# 	- here("results", "numbers", "coef_accepted_offer_dl_d1_abs.tex")
# 	- here("results", "numbers", "p_value_dl_d1.tex")
# 	- here("results", "numbers", "coef_accepted_offer_fee_late_dl.tex")
# 	- here("results", "numbers", "coef_effect_dl_small.tex")
# 	- here("results", "numbers", "coef_effect_dl_small_abstract.tex")
# 	- here("results", "numbers", "coef_effect_dl_small_pp.tex")
# 	- here("results", "numbers", "coef_effect_dl_lower_fee.tex")
# 	- here("results", "numbers", "coef_effect_dl_lower_fee_pp.tex")
# 	- here("results", "numbers", "pct_dl_lower_fee.tex")
# 	- here("results", "numbers", "p_value_dl_fee_interaction.tex")
# 	- here("results", "numbers", "p_value_dl_fsize_interaction.tex")
# 	- here("results", "numbers", "p_value_dl.tex")
# 	- here("results", "numbers", "coef_accepted_offer_t2_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_t3_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_t4_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_t5_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_t6_d8.tex")
# 	- here("results", "numbers", "coef_accepted_offer_t7_d8.tex")
# 	- here("results", "numbers", "coef_takeup_dl.tex")
# 	- here("results", "numbers", "catch_up_nodl_days.tex")
# 	- here("results", "numbers", "catch_up_nodl_fee2.75_days.tex")
# 	- here("results", "numbers", "catch_up_nodl_fee3_days.tex")
# 	- here("results", "numbers", "catch_up_nodl_fee3_weeks.tex")
# 	- here("results", "numbers", "catch_up_nodl_abovemed_days.tex")
# 	- here("results", "numbers", "catch_up_nodl_belowmed_days.tex")
# 	- here("results", "numbers", "catch_up_nodl_belowmed_weeks.tex") 
# 	- here("results", "numbers", "fintech_marketshare_total.tex")
# 	- here("results", "numbers", "fintech_marketshare_mpos.tex")
# 	- here("results", "numbers", "stat_pct_open_terms.tex")

# Purpose:
# 	- Calculate numbers and statistics for admin data and export to tex files.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(readxl)
library(fixest)
library(kableExtra)
library(qs)
library(lubridate)
library(hablar)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
#########################################################

##########################################
##    (1): Summary statistics numbers.  ##
##########################################
# (1.1): Import administrative dataset with treatment variables.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv"), show_col_types = FALSE)
fintech_fee_large <- read_csv(here("proc", "fintech_fee.csv"), show_col_types = FALSE) |>
  select(organization_uuid, starts_with("accept_fee_cum") & !matches("accept_fee_cumweek[1-3]"))
fintech_fee <- fintech_fee |>
  left_join(fintech_fee_large, by = "organization_uuid")
# (1.2): Save total sample size.
nrow(fintech_fee) %>% 
  print_n("stat_sample_size.tex", "Total sample size")

nrow(fintech_fee) %>% 
  print_n("stat_sample_size_abstract.tex", "Total sample size calculated for abstract")

# (1.3): Calculate number of firms in control group.
fintech_fee %>% 
  filter(treat_description == "Control") %>% 
  count() %>% 
  print_n("stat_number_control.tex", "Number of firms in control group")
  
# (1.4): Import firm-level dataset with detailed business type data.
users <- read_rds(here("proc", "users.rds")) %>% 
  filter(organization_uuid %in% fintech_fee$organization_uuid) %>% 
  select(organization_uuid, business_type_description)

# (1.5): Merge detailed business description with dataset with general business types.
fintech_fee %<>% left_join(users, by = "organization_uuid")

# (1.6): Tab largest 3 business types by category.
buss_types <- fintech_fee %>% 
  mutate(total_n = n()) %>% 
  group_by(new_buss_type) %>%
  mutate(group_n = n()) %>%
  group_by(new_buss_type, business_type_description) %>% 
  summarise(prop = n() / group_n,
            group_prop = mean(group_n) / total_n) %>% 
  ungroup() %>% 
  unique() %>% 
  group_by(new_buss_type) %>% 
  arrange(new_buss_type, desc(prop)) %>% 
  slice_head(n = 3) %>% 
  arrange(desc(group_prop))
  
# (1.7): Import balance data and get percentage of female-owned businesses.
balance_data <- read_csv(here("proc", "balance_table_data.csv"), show_col_types = FALSE)
balance_data %>% 
  summarise(pct_fem = sum(owner_sex_female) / n()) %>% 
  print_pct("stat_pct_fem.tex", "Percent of female-owned businesses")
balance_data %>% 
  left_join(fintech_fee %>%
              select(organization_uuid, reminder, no_reminder), by = "organization_uuid") %>%
  filter(reminder == 1 | no_reminder == 1) %>% 
  summarise(pct_fem = sum(owner_sex_female) / n()) %>% 
  print_pct("stat_pct_fem_rem.tex", "Percent of female-owned businesses (between reminder and no reminder groups)")
balance_data %>% 
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
  summarise(pct_fem = sum(owner_sex_female) / n()) %>% 
  print_pct("stat_pct_fem_antrem.tex", "Percent of female-owned businesses (between anticipated reminder and unanticipated reminder groups)")
balance_data %>% 
  left_join(fintech_fee %>%
              select(organization_uuid, no_deadline), by = "organization_uuid") %>%
  filter(deadline == 1 | no_deadline == 1) %>% 
  summarise(pct_fem = sum(owner_sex_female) / n()) %>% 
  print_pct("stat_pct_fem_dl.tex", "Percent of female-owned businesses (between deadline and no deadline groups)")

balance_data %>% 
  left_join(fintech_fee %>%
              select(organization_uuid, reminder, no_reminder), by = "organization_uuid") %>%
  filter(reminder == 1 | no_reminder == 1) %>%
  summarise(median_owner_age = median(owner_age, na.rm = TRUE)) %>% 
  print_n("stat_age_median_rem.tex", " (Median manager age between reminder and no reminder groups)")
balance_data %>% 
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>%
  summarise(median_owner_age = median(owner_age, na.rm = TRUE)) %>% 
  print_n("stat_age_median_antrem.tex", " (Median manager age between anticipated reminder and unanticipated reminder groups)")
balance_data %>% 
  left_join(fintech_fee %>%
              select(organization_uuid, no_deadline), by = "organization_uuid") %>%
  filter(deadline == 1 | no_deadline == 1) %>% 
  summarise(median_owner_age = median(owner_age, na.rm = TRUE)) %>% 
  print_n("stat_age_median_dl.tex", " (Median manager age between deadline and no deadline groups)")


# (1.8): Get percentage of small retailers.
balance_data %>% 
  filter(new_buss_type == "Small_retailers") %>% 
  summarise(pct_small_retailer = n() / nrow(balance_data)) %>% 
  print_pct("stat_pct_small_retailer.tex", "Percent business type = small retailers")

# (1.9): Get percentage of professionals.
balance_data %>% 
  filter(new_buss_type == "Professionals") %>% 
  summarise(pct_prof = n() / nrow(balance_data)) %>% 
  print_pct("stat_pct_prof.tex", "Percent business type = professional")

# (1.10): Get difference of take-up conditional on opening the email by the deadline.
takeup_email <- fintech_fee %>% 
  filter(reminder == 1 | no_reminder == 1) %>% 
  filter(open_email_ontime == 1) %>% 
  group_by(reminder) %>% 
  summarise(takeup = mean(accepted_offer_ontime))
takeup_email %>% 
  filter(reminder == 0) %>% 
  pull(takeup) %>% 
  print_pct("avg_takeup_open_email_norem.tex", "Percent businesses without the reminder accepting the offer, conditional on opening the email by the deadline")
takeup_email %>% 
  filter(reminder == 1) %>%
  pull(takeup) %>% 
  print_pct("avg_takeup_open_email_rem.tex", "Percent businesses with the reminder accepting the offer, conditional on opening the email by the deadline")
# takeup_email %>% mutate(diff = lead(takeup) - takeup) %>% filter(!is.na(diff)) %>% pull(diff)

###############################################################
##  (2): Regression coefficient numbers: lower fee effects.  ##
###############################################################
# (2.1): Import regression data.
current_regs <- c("reg_cum_daily_pooled_accepted_offer_rem",
                  "reg_cum_daily_pooled_accepted_offer_antrem",
                  "reg_cum_daily_pooled_accepted_offer_fee",
                  "reg_cum_daily_pooled_accepted_offer_unantrem",
                  # "reg_cum_daily_fee",
                  "reg_cum_daily",
                  "reg_cum_daily_het_accepted_offer_dl_fee",
                  "reg_cum_daily_het_accepted_offer_fsize_dl",
                  "reg_cum_daily_pooled_accepted_offer_dl")
number_regs <- qread(file = here("proc", "regs_for_figs.qs"))
for (i in 1:length(number_regs)) {
  object <- number_regs[[i]]
  assign(names(number_regs)[i], object)
}
regs_remove <- ls() %>% .[! (. %in% current_regs) & str_detect(., "reg_cum")]
rm(list = regs_remove)
rm(number_regs, object, current_regs)

# (2.2): Calculate 3% offer group take-up on day 1.
fintech_fee %>% 
  filter(fee_2.75 == 1 | fee_2.75 == 0) %>% 
  filter(fee_2.75 == 0) %>%
  summarise(mean_acceptance = mean(accepted_offer_firstday)) %>% 
  print_pct("coef_accepted_offer_fee_3_d1.tex", "3% offer group take-up on day 1")
    
# (2.3): Calculate 2.75% effect on day 1 take-up.
coef_d1 <- reg_cum_daily_pooled_accepted_offer_fee$`Sep 29`$coefficients[["fee_2.75"]]
coef_d1 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_fee_2.75_d1.tex", "Cumulative 2.75% effect on offer acceptance on day 1")

# (2.4): Calculate 3% offer group take-up on day 8.
bline_d8 <- fintech_fee %>% 
  filter(fee_2.75 == 1 | fee_2.75 == 0) %>% 
  filter(fee_2.75 == 0) %>%
  summarise(mean_acceptance = mean(accepted_offer_ontime))
bline_d8 %>%   
  print_pct("coef_accepted_offer_fee_3_d8.tex", "3% offer group take-up on day 8")
    
# (2.5): Calculate 2.75% effect on day 8 take-up.
coef_d8 <- reg_cum_daily_pooled_accepted_offer_fee$`Oct 6`$coefficients[["fee_2.75"]]
coef_d8 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_fee_2.75_d8.tex", "Cumulative 2.75% effect on offer acceptance on day 8")
# Effect in % 
(coef_d8/bline_d8) %>%
  print_pct("coef_accepted_offer_fee_2.75_d8_pct.tex", "Cumulative 2.75% effect on offer acceptance on day 8 (%)")

# (2.6): Calculate gap in take-up after the deadline.
fintech_fee %>% 
  filter(fee_2.75 == 1 | fee_2.75 == 0) %>% 
  group_by(fee_2.75) %>% 
  summarise(mean_acceptance = mean(accepted_offer_late)) %>% 
  pull(mean_acceptance) %>% 
  diff() %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_fee_long.tex", "Cumulative 2.75% effect on offer acceptance after six months")
  

  

# (2.7): Calculate mean take-up by day 8.
# 2.75 group mean
fintech_fee %>% 
  filter(fee_2.75 == 1 | fee_2.75 == 0) %>% 
  filter(treat_T1 != 1 & treat_T8 != 1) %>% 
  filter(fee_2.75 == 1) %>% 
  summarise(mean_acceptance = mean(accept_fee_cumweek1)) %>% 
  print_pct("mean_accepted_offer_2.75.tex", "Cumulative offer acceptance on day 8 for firms with 2.75 offer")
# 3.00 group mean
fintech_fee %>% 
  filter(fee_2.75 == 1 | fee_2.75 == 0) %>% 
  filter(treat_T1 != 1 & treat_T8 != 1) %>% 
  filter(fee_2.75 == 0) %>% 
  summarise(mean_acceptance = mean(accept_fee_cumweek1)) %>% 
  print_pct("mean_accepted_offer_3.00.tex", "Cumulative offer acceptance on day 8 for firms with 3.00 offer")

# Deadline group, no reminder 
fintech_fee %>% 
  filter(deadline == 1 & no_reminder == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))
fintech_fee %>% 
  filter(deadline == 1 & no_reminder == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cum2020_09_29))
fintech_fee %>% 
  filter(unanticipated_reminder == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))
fintech_fee %>% 
  filter(unanticipated_reminder == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cum2020_09_29))
fintech_fee %>% 
  filter(unanticipated_reminder == 1 & no_deadline == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))
fintech_fee %>% 
  filter(unanticipated_reminder == 1 & no_deadline == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cum2020_09_29))
fintech_fee %>% 
  filter(anticipated_reminder == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))
fintech_fee %>% 
  filter(anticipated_reminder == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cum2020_09_29))
fintech_fee %>% 
  filter(anticipated_reminder == 1 & no_deadline == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))
fintech_fee %>% 
  filter(anticipated_reminder == 1 & no_deadline == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cum2020_09_29))

# (2.2): Calculate how much reminders increase take-up of lower fee
# Regression coefficients
coef_d7 <- reg_cum_daily_pooled_accepted_offer_rem$`Oct 5`$coefficients
coef_d7 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_rem_d7.tex", "Cumulative reminder effect on offer acceptance on day 7")
coef_d8 <- reg_cum_daily_pooled_accepted_offer_rem$`Oct 6`$coefficients
coef_d8 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_rem_d8.tex", "Cumulative reminder effect on offer acceptance on day 8")
coef_d8 %>% 
  "*"(100 ) %>% 
  print_n("pres_coef_accepted_offer_rem_d8.tex", "Cumulative reminder effect on offer acceptance on day 8", dig = 1)
# Omitted group mean
omitted_group_mean <- fintech_fee %>% 
  filter(reminder == 1 | no_reminder == 1) %>% 
  filter(no_reminder == 1) %>% 
  summarise(mean_acceptance = mean(accept_fee_cumweek1))
omitted_group_mean %>% 
  print_pct("mean_accepted_offer_norem.tex", "Cumulative offer acceptance on day 8 of firms with no reminder")
omitted_group_mean %>% 
  print_pct("pres_mean_accepted_offer_norem.tex", "Cumulative offer acceptance on day 8 of firms with no reminder", dig = 0)
# Percentage increase
pct_inc <- (coef_d8 / omitted_group_mean)
pct_inc %>% 
  print_pct("pct_inc_accepted_offer_rem_d8.tex", "Percentage increase due to reminder effect on offer acceptance on day 8")
pct_inc %>% 
  print_pct("pres_pct_inc_accepted_offer_rem_d8.tex", "Percentage increase due to reminder effect on offer acceptance on day 8", dig = 0)

# (2.3): Calculate how much anticipated reminders increase take-up of lower fee.
# Regression coefficients
coef_d7 <- reg_cum_daily_pooled_accepted_offer_antrem$`Oct 5`$coefficients
coef_d7 %>% 
  "*"(100) %>% 
  print_n("coef_accepted_offer_antrem_d7.tex", "Cumulative anticipated reminder effect on offer acceptance on day 7", dig = 1)
omitted_group_mean_d7 <- fintech_fee %>% 
  filter(reminder == 1 | no_reminder == 1) %>% 
  filter(no_reminder == 1) %>% 
  summarise(mean_acceptance = mean(accept_fee_cum2020_10_05))
pct_inc_d7 <- (coef_d7 / omitted_group_mean_d7)
pct_inc_d7 %>% 
  print_pct("pct_inc_accepted_offer_antrem_d7.tex", "Percentage increase due to anticipated reminder effect on offer acceptance on day 7")
pct_inc_d7 %>% 
  print_pct("pres_pct_inc_accepted_offer_antrem_d7.tex", "Percentage increase due to anticipated reminder effect on offer acceptance on day 7", dig = 0)
pct_inc_d7 %>% 
  print_pct("pres_pct_inc_accepted_offer_antrem_d7_abstract.tex", "Percentage increase due to anticipated reminder effect on offer acceptance on day 7", dig = 0)

coef_d8 <- reg_cum_daily_pooled_accepted_offer_antrem$`Oct 6`$coefficients

# Omitted group is same as no-reminder
# Percentage increase
pct_inc <- (coef_d8 / omitted_group_mean)
pct_inc %>% 
  print_pct("pct_inc_accepted_offer_antrem_d8.tex", "Percentage increase due to anticipated reminder effect on offer acceptance on day 8")
pct_inc %>% 
  print_pct("pres_pct_inc_accepted_offer_antrem_d8.tex", "Percentage increase due to anticipated reminder effect on offer acceptance on day 8", dig = 0)
pct_inc %>% 
  print_pct("pres_pct_inc_accepted_offer_antrem_d8_abstract.tex", "Percentage increase due to anticipated reminder effect on offer acceptance on day 8", dig = 0)
coef_d8 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_antrem_d8.tex", "Cumulative anticipated reminder effect on offer acceptance on day 8")

# (2.4): Calculate how much anticipated reminders increase take-up of lower fee.
# Regression coefficients
coef_d8 <- reg_cum_daily_pooled_accepted_offer_unantrem$`Oct 6`$coefficients
coef_d8 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_unantrem_d8.tex", "Cumulative unanticipated reminder effect on offer acceptance on day 8")
# Omitted group is same as no-reminder
# Percentage increase
pct_inc <- (coef_d8 / omitted_group_mean)
pct_inc %>% 
  print_pct("pct_inc_accepted_offer_unantrem_d8.tex", "Percentage increase due to unanticipated reminder effect on offer acceptance on day 8")
pct_inc %>% 
  print_pct("pres_pct_inc_accepted_offer_unantrem_d8.tex", "Percentage increase due to unanticipated reminder effect on offer acceptance on day 8", dig = 0)
pct_inc %>% 
  print_pct("pres_pct_inc_accepted_offer_unantrem_d8_abstract.tex", "Percentage increase due to unanticipated reminder effect on offer acceptance on day 8", dig = 0)



coef_d1 <- reg_cum_daily_pooled_accepted_offer_dl$`Sep 29`$coefficients
coef_d1 %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_dl_d1.tex", "Day - 1 negative deadline effect")
abs(coef_d1) %>% 
  "*"(100 ) %>% 
  print_n("coef_accepted_offer_dl_d1_abs.tex", "Day - 1 negative deadline effect (absolute value)")

reg_cum_daily_pooled_accepted_offer_dl$`Sep 29`$coeftable[4] %>%
  print_n("p_value_dl_d1.tex", "P-value for day-1 negative deadline effect", dig = 3)


# Takeup days until no deadline group catches up to deadline group
balance_data_subset <- balance_data %>%
  select(organization_uuid, above_median_baseline_sales)
catch_up_data <- fintech_fee |>
  select(organization_uuid, fee_2.75, deadline, no_deadline, starts_with("accept_fee_cum") & !matches("accept_fee_cumweek[1-3]")) |>
  left_join(balance_data_subset, by = "organization_uuid")
long_data <- catch_up_data %>%
  filter(deadline == 1 | no_deadline == 1) %>%
  pivot_longer(
    cols = starts_with("accept_fee_cum"),
    names_to = "date",
    names_prefix = "accept_fee_cum",
    values_to = "value"
  )

long_data <- long_data %>%
  mutate(date = as.Date(date, format = "%Y_%m_%d"))

result_dl <- long_data %>%
  group_by(date, deadline) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = deadline,
    values_from = avg_value,
    names_prefix = "group_"
  ) %>%
  rename(
    deadline = group_1,
    no_deadline = group_0
  )

result_dl_fee <- long_data %>%
  group_by(date, deadline, fee_2.75) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    group = case_when(
      deadline == 1 & fee_2.75 == 1 ~ "deadline_fee_2.75",
      deadline == 1 & fee_2.75 == 0 ~ "deadline_fee_3.00",
      deadline == 0 & fee_2.75 == 1 ~ "no_deadline_fee_2.75",
      deadline == 0 & fee_2.75 == 0 ~ "no_deadline_fee_3.00"
    )
  ) %>%
  select(date, group, avg_value) %>%
  pivot_wider(
    names_from = group,
    values_from = avg_value
  )

result_dl_above_median_baseline_sales <- long_data %>%
  group_by(date, deadline, above_median_baseline_sales) %>%
  summarize(avg_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    group = case_when(
      deadline == 1 & above_median_baseline_sales == 1 ~ "deadline_above_median_baseline_sales",
      deadline == 1 & above_median_baseline_sales == 0 ~ "deadline_below_median_baseline_sales",
      deadline == 0 & above_median_baseline_sales == 1 ~ "no_deadline_above_median_baseline_sales",
      deadline == 0 & above_median_baseline_sales == 0 ~ "no_deadline_below_median_baseline_sales"
    )
  ) %>%
  select(date, group, avg_value) %>%
  pivot_wider(
    names_from = group,
    values_from = avg_value
  )

# days_to_exceed_nodl <- result_dl %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline >= deadline) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodl = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodl) %>%
#   print_n("catch_up_nodl_days.tex", "Number of days it took for no_deadline group to equal or exceed deadline group post Day-8 deadline", dig = 0)


days_to_exceed_nodl <- result_dl %>%
  # Check the condition for 2020-10-05
  mutate(initial_condition = ifelse(date == as.Date("2020-10-05") & no_deadline >= deadline, TRUE, FALSE)) %>%
  summarize(initial_condition_met = any(initial_condition)) %>%
  pull(initial_condition_met) %>%
  # Compute days_to_exceed_nodl based on the condition
  ifelse(.,
         0,  # If condition met on 2020-10-05, set to 0
         result_dl %>%
           filter(date > as.Date("2020-10-06")) %>%
           mutate(no_deadline_exceeds = no_deadline >= deadline) %>%
           filter(no_deadline_exceeds) %>%
           slice_min(date) %>%
           summarize(days_to_exceed_nodl = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
           pull(days_to_exceed_nodl)) %>%
  print_n("catch_up_nodl_days.tex", "Number of days it took for no_deadline group to equal or exceed deadline group post Day-8 deadline", dig = 0)


# days_to_exceed_nodlfee2.75 <- result_dl_fee %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_fee_2.75 >= deadline_fee_2.75) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlfee2.75 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlfee2.75) %>%
#   print_n("catch_up_nodl_fee2.75_days.tex", "Number of days it took for no_deadline with fee_2.75 group to equal or exceed deadline with fee_2.75 group post Day-8 deadline", dig = 0)

days_to_exceed_nodlfee2.75 <- result_dl_fee %>%
  # Check the condition for 2020-10-05
  mutate(initial_condition = ifelse(date == as.Date("2020-10-05") & no_deadline_fee_2.75 >= deadline_fee_2.75, TRUE, FALSE)) %>%
  summarize(initial_condition_met = any(initial_condition)) %>%
  pull(initial_condition_met) %>%
  # Compute days_to_exceed_nodlfee2.75 based on the condition
  ifelse(.,
         0,  # If condition met on 2020-10-05, set to 0
         result_dl_fee %>%
           filter(date > as.Date("2020-10-06")) %>%
           mutate(no_deadline_exceeds = no_deadline_fee_2.75 >= deadline_fee_2.75) %>%
           filter(no_deadline_exceeds) %>%
           slice_min(date) %>%
           summarize(days_to_exceed_nodlfee2.75 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
           pull(days_to_exceed_nodlfee2.75)) %>%
  print_n("catch_up_nodl_fee2.75_days.tex", "Number of days it took for no_deadline with fee_2.75 group to equal or exceed deadline with fee_2.75 group post Day-8 deadline", dig = 0)


# days_to_exceed_nodlfee3 <- result_dl_fee %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_fee_3.00 >= deadline_fee_3.00) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlfee3 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlfee3) %>%
#   print_n("catch_up_nodl_fee3_days.tex", "Number of days it took for no_deadline with fee_3 group to equal or exceed deadline with fee_3 group post Day-8 deadline", dig = 0)

days_to_exceed_nodlfee3 <- result_dl_fee %>%
  # Check the condition for 2020-10-05
  mutate(initial_condition = ifelse(date == as.Date("2020-10-05") & no_deadline_fee_3.00 >= deadline_fee_3.00, TRUE, FALSE)) %>%
  summarize(initial_condition_met = any(initial_condition)) %>%
  pull(initial_condition_met) %>%
  # Compute days_to_exceed_nodlfee3 based on the condition
  ifelse(.,
         0,  # If condition met on 2020-10-05, set to 0
         result_dl_fee %>%
           filter(date > as.Date("2020-10-06")) %>%
           mutate(no_deadline_exceeds = no_deadline_fee_3.00 >= deadline_fee_3.00) %>%
           filter(no_deadline_exceeds) %>%
           slice_min(date) %>%
           summarize(days_to_exceed_nodlfee3 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
           pull(days_to_exceed_nodlfee3)) %>%
  print_n("catch_up_nodl_fee3_days.tex", "Number of days it took for no_deadline with fee_3 group to equal or exceed deadline with fee_3 group post Day-8 deadline", dig = 0)



weeks_to_exceed_nodlfee3 <- result_dl_fee %>%
  filter(date > as.Date("2020-10-06")) %>%
  mutate(no_deadline_exceeds = no_deadline_fee_3.00 >= deadline_fee_3.00) %>%
  filter(no_deadline_exceeds) %>%
  slice_min(date) %>%
  summarize(weeks_to_exceed_nodlfee3 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
  mutate(weeks_to_exceed_nodlfee3 = weeks_to_exceed_nodlfee3/7) %>%
  pull(weeks_to_exceed_nodlfee3) %>%
  print_n("catch_up_nodl_fee3_weeks.tex", "Number of weeks it took for no_deadline with fee_3 group to equal or exceed deadline with fee_3 group post Day-8 deadline", dig = 1)


# days_to_exceed_nodlabovemed <- result_dl_above_median_baseline_sales %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_above_median_baseline_sales >= deadline_above_median_baseline_sales) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlabovemed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlabovemed) %>%
#   print_n("catch_up_nodl_abovemed_days.tex", "Number of days it took for no_deadline with above median baseline sales group to equate or exceed deadline with above median baseline sales group post Day-8 deadline", dig = 0)


days_to_exceed_nodlabovemed <- result_dl_above_median_baseline_sales %>%
  # Check the condition for 2020-10-05
  mutate(initial_condition = ifelse(date == as.Date("2020-10-05") & 
                                      no_deadline_above_median_baseline_sales >= deadline_above_median_baseline_sales, 
                                    TRUE, 
                                    FALSE)) %>%
  summarize(initial_condition_met = any(initial_condition)) %>%
  pull(initial_condition_met) %>%
  # Compute days_to_exceed_nodlabovemed based on the condition
  ifelse(.,
         0,  # If condition met on 2020-10-05, set to 0
         result_dl_above_median_baseline_sales %>%
           filter(date > as.Date("2020-10-06")) %>%
           mutate(no_deadline_exceeds = no_deadline_above_median_baseline_sales >= deadline_above_median_baseline_sales) %>%
           filter(no_deadline_exceeds) %>%
           slice_min(date) %>%
           summarize(days_to_exceed_nodlabovemed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
           pull(days_to_exceed_nodlabovemed)) %>%
  print_n("catch_up_nodl_abovemed_days.tex", "Number of days it took for no_deadline with above median baseline sales group to equate or exceed deadline with above median baseline sales group post Day-8 deadline", dig = 0)

# days_to_exceed_nodlbelowmed <- result_dl_above_median_baseline_sales %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_below_median_baseline_sales >= deadline_below_median_baseline_sales) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlbelowmed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlbelowmed) %>%
#   print_n("catch_up_nodl_belowmed_days.tex", "Number of days it took for no_deadline with below median baseline sales group to equate or exceed deadline with below median baseline sales group post Day-8 deadline", dig = 0)

days_to_exceed_nodlbelowmed <- result_dl_above_median_baseline_sales %>%
  # Check the condition for 2020-10-05
  mutate(initial_condition = ifelse(date == as.Date("2020-10-05") & 
                                      no_deadline_below_median_baseline_sales >= deadline_below_median_baseline_sales, 
                                    TRUE, 
                                    FALSE)) %>%
  summarize(initial_condition_met = any(initial_condition)) %>%
  pull(initial_condition_met) %>%
  # Compute days_to_exceed_nodlbelowmed based on the condition
  ifelse(.,
         0,  # If condition met on 2020-10-05, set to 0
         result_dl_above_median_baseline_sales %>%
           filter(date > as.Date("2020-10-06")) %>%
           mutate(no_deadline_exceeds = no_deadline_below_median_baseline_sales >= deadline_below_median_baseline_sales) %>%
           filter(no_deadline_exceeds) %>%
           slice_min(date) %>%
           summarize(days_to_exceed_nodlbelowmed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
           pull(days_to_exceed_nodlbelowmed)) %>%
  print_n("catch_up_nodl_belowmed_days.tex", "Number of days it took for no_deadline with below median baseline sales group to equate or exceed deadline with below median baseline sales group post Day-8 deadline", dig = 0)



weeks_to_exceed_nodlbelowmed <- result_dl_above_median_baseline_sales %>%
  filter(date > as.Date("2020-10-06")) %>%
  mutate(no_deadline_exceeds = no_deadline_below_median_baseline_sales >= deadline_below_median_baseline_sales) %>%
  filter(no_deadline_exceeds) %>%
  slice_min(date) %>%
  summarize(weeks_to_exceed_nodlbelowmed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
  mutate(weeks_to_exceed_nodlbelowmed = weeks_to_exceed_nodlbelowmed/7) %>%
  pull(weeks_to_exceed_nodlbelowmed) %>%
  print_n("catch_up_nodl_belowmed_weeks.tex", "Number of weeks it took for no_deadline with below median baseline sales group to equate or exceed deadline with below median baseline sales group post Day-8 deadline", dig = 1)


# # Define a function to handle the logic for saving in days or weeks
# save_days_or_weeks <- function(days, file_prefix, description) {
#   if (days >= 7) {
#     weeks <- days / 7
#     file_name <- paste0(file_prefix, "_weeks.tex")
#     #print_n(file_name, paste0(description, " (in weeks: ", weeks, ")"), dig = 2)
#   } else {
#     file_name <- paste0(file_prefix, "_days.tex")
#     #print_n(file_name, paste0(description, " (in days: ", days, ")"), dig = 0)
#   }
# }
# 
# 
# # Apply the function for each case
# days_to_exceed_nodl <- result_dl %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline >= deadline) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodl = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodl)
# 
# save_days_or_weeks(
#   days_to_exceed_nodl,
#   "catch_up_nodl",
#   "Number of days it took for no_deadline group to equal or exceed deadline group post Day-8 deadline"
# )
# 
# days_to_exceed_nodlfee2.75 <- result_dl_fee %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_fee_2.75 >= deadline_fee_2.75) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlfee2.75 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlfee2.75)
# 
# save_days_or_weeks(
#   days_to_exceed_nodlfee2.75,
#   "catch_up_nodl_fee2.75",
#   "Number of days it took for no_deadline with fee_2.75 group to equal or exceed deadline with fee_2.75 group post Day-8 deadline"
# )
# 
# days_to_exceed_nodlfee3 <- result_dl_fee %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_fee_3.00 >= deadline_fee_3.00) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlfee3 = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlfee3)
# 
# save_days_or_weeks(
#   days_to_exceed_nodlfee3,
#   "catch_up_nodl_fee3",
#   "Number of days it took for no_deadline with fee_3 group to equal or exceed deadline with fee_3 group post Day-8 deadline"
# )
# 
# days_to_exceed_nodlabovemed <- result_dl_above_median_baseline_sales %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_above_median_baseline_sales >= deadline_above_median_baseline_sales) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlabovemed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlabovemed)
# 
# save_days_or_weeks(
#   days_to_exceed_nodlabovemed,
#   "catch_up_nodl_abovemed",
#   "Number of days it took for no_deadline with above median baseline sales group to equate or exceed deadline with above median baseline sales group post Day-8 deadline"
# )
# 
# days_to_exceed_nodlbelowmed <- result_dl_above_median_baseline_sales %>%
#   filter(date > as.Date("2020-10-06")) %>%
#   mutate(no_deadline_exceeds = no_deadline_below_median_baseline_sales >= deadline_below_median_baseline_sales) %>%
#   filter(no_deadline_exceeds) %>%
#   slice_min(date) %>%
#   summarize(days_to_exceed_nodlbelowmed = as.numeric(min(date) - as.Date("2020-10-06"))) %>%
#   pull(days_to_exceed_nodlbelowmed)
# 
# save_days_or_weeks(
#   days_to_exceed_nodlbelowmed,
#   "catch_up_nodl_belowmed",
#   "Number of days it took for no_deadline with below median baseline sales group to equate or exceed deadline with below median baseline sales group post Day-8 deadline"
# )

# (2.4): Calculate deadline effects.
# dead_effect <- fintech_fee %>% 
#   filter(deadline == 1 | no_deadline == 1) %>% 
#   group_by(deadline) %>% 
#   summarise(mean_takeup = mean(accepted_offer_late)) %>% 
#   ungroup()
# dead_effect %>% 
#   pull(mean_takeup) %>% 
#   rev() %>% 
#   diff() %>% 
#   "*"(100) %>% 
#   print_n("coef_accepted_offer_fee_late_dl.tex", "Long-term deadline effect", dig = 0)

reg_cum_dl_accepted_offer_late <- feols(accepted_offer_late ~ deadline, se = "hetero", fixef = "strata_fe", 
                                        data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))
num_reg_cum_dl_accepted_offer_late <- abs(reg_cum_dl_accepted_offer_late$coeftable[1]*100) %>% 
  print_n("coef_accepted_offer_fee_late_dl.tex", "Long-term deadline effect", dig = 1)



# dead_effect_check <- fintech_fee %>% 
#   filter(deadline == 1 | no_deadline == 1) %>% 
#   group_by(deadline) %>% 
#   summarise(mean_takeup = mean(accepted_offer_ontime)) %>% 
#   ungroup()



# dead_effect_ <- fintech_fee %>%
#   left_join(balance_data_subset, by = "organization_uuid") %>%
#   filter(deadline == 1 | no_deadline == 1) %>% 
#   filter(fast_deadline == 0) %>%
#   filter(above_median_baseline_sales == 0) %>%
#   group_by(deadline) %>%
#   summarise(mean_takeup = mean(accept_fee_cumweek1)) 
# dead_effect_ %>%
#   mutate(diff_result = diff(mean_takeup) * 100 / (mean_takeup[1]*100)) %>%
#   slice(1) %>%
#   pull(diff_result) %>%
#   print_pct("coef_effect_dl_small.tex", "Effect of deadline on take-up for small firms as percent") 

# dead_effect_ %>%
#   mutate(diff_result = diff(mean_takeup)) %>%
#   mutate(diff_result = diff_result*100) %>%
#   slice(1) %>%
#   pull(diff_result) %>%
#   print_n("coef_effect_dl_small_pp.tex", "Effect of deadline on take-up for small firms as pp", dig = 1) 

# dead_effect_ %>%
#   mutate(diff_result = diff(mean_takeup) * 100 / (mean_takeup[1]*100)) %>%
#   slice(1) %>%
#   pull(diff_result)%>%
#   print_pct("coef_effect_dl_small_abstract.tex", "Effect of deadline on take-up for small firms as percent", dig = 0)


# dead_effect_ <- fintech_fee %>%
#   left_join(balance_data_subset, by = "organization_uuid") %>%
#   filter(deadline == 1 | no_deadline == 1) %>% 
#   filter(fast_deadline == 0) %>%
#   filter(fee_type == "3% offer") %>%
#   group_by(deadline) %>%
#   summarise(mean_takeup = mean(accept_fee_cumweek1)) 
# dead_effect_ %>% 
#   pull(mean_takeup) %>% 
#   diff() %>% 
#   "*"(100) %>% 
#   print_n("pct_dl_lower_fee.tex", "Effect of a deadline on cumulative take-up by day 8 for the less valuable offer (the 3% fee)", dig = 0)

# dead_effect_ <- fintech_fee %>%
#   left_join(balance_data_subset, by = "organization_uuid") %>%
#   filter(deadline == 1 | no_deadline == 1) %>% 
#   filter(fast_deadline == 0) %>%
#   filter(above_median_baseline_sales == 0) %>%
#   group_by(deadline) %>%
#   summarise(mean_takeup = mean(accept_fee_cumweek1)) 
# dead_effect_ %>% 
#   pull(mean_takeup) %>% 
#   diff() %>% 
#   "*"(100) %>% 
#   print_n("pct_dl_small_firm.tex", "Effect of a deadline on cumulative take-up by day 8 for smaller firms", dig = 0)

## Deadline effect but considering regressions
omitted_group_mean_d8 <- fintech_fee %>% 
  left_join(balance_data_subset, by = "organization_uuid") %>%
  filter(deadline == 1 | no_deadline == 1) %>% 
  filter(fast_deadline == 0) %>%
  filter(above_median_baseline_sales == 0) %>%
  filter(no_deadline == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))

coef_d8 <- reg_cum_daily_het_accepted_offer_fsize_dl$`Oct 6`$coeftable[2]
pct_inc <- (coef_d8 / omitted_group_mean_d8)
pct_inc %>%
  print_pct("coef_effect_dl_small.tex", "Effect of deadline on take-up for small firms as percent") 
pct_inc %>%
  print_pct("coef_effect_dl_small_abstract.tex", "Effect of deadline on take-up for small firms as percent", dig = 0)
adjusted_value <- reg_cum_daily_het_accepted_offer_fsize_dl$`Oct 6`$coeftable[2] * 100
adjusted_value %>%
  print_n("coef_effect_dl_small_pp.tex", "Effect of deadline on take-up for small firms as pp", dig = 1) 

coef_d8 <- reg_cum_daily_het_accepted_offer_fsize_dl$`Oct 6`$coeftable[2]
adjusted_value <- coef_d8 * 100
adjusted_value %>% 
  print_n("pct_dl_small_firm.tex", "Effect of a deadline on cumulative take-up by day 8 for smaller firms", dig = 1)

omitted_group_mean_d8_fee <- fintech_fee %>% 
  left_join(balance_data_subset, by = "organization_uuid") %>%
  filter(deadline == 1 | no_deadline == 1) %>% 
  filter(fast_deadline == 0) %>%
  filter(fee_2.75 == 0) %>%
  filter(no_deadline == 1) %>%
  summarise(mean_acceptance = mean(accept_fee_cumweek1))

coef_d8 <- reg_cum_daily_het_accepted_offer_dl_fee$`Oct 6`$coeftable[1]
pct_inc <- (coef_d8 / omitted_group_mean_d8_fee)
pct_inc %>%
  print_pct("coef_effect_dl_lower_fee.tex", "Effect of deadline on lower fee take-up as percent") 
adjusted_value <- reg_cum_daily_het_accepted_offer_dl_fee$`Oct 6`$coeftable[1] * 100
adjusted_value %>%
  print_n("coef_effect_dl_lower_fee_pp.tex", "Effect of deadline on lower fee take-up as pp", dig = 1) 


adjusted_value <- coef_d8 * 100
adjusted_value %>% 
  print_n("pct_dl_lower_fee.tex", "Effect of a deadline on cumulative take-up by day 8 for the less valuable offer (the 3% fee)", dig = 1)



## Printing p-values for the interaction effect of deadline and fee_type of 2.75% and above_median_sales
reg_cum_daily_het_accepted_offer_dl_fee$`Oct 6`$coeftable[12] %>%
  print_n("p_value_dl_fee_interaction.tex", "P-value of the interaction term deadline*fee_2.75", dig = 3)

reg_cum_daily_het_accepted_offer_fsize_dl$`Oct 6`$coeftable[12] %>%
  print_n("p_value_dl_fsize_interaction.tex", "P-value of the interaction term deadline*above_median_sales", dig = 3)

reg_cum_daily_pooled_accepted_offer_dl$`Oct 6`$coeftable[4] %>%
  print_n("p_value_dl.tex", "P-value of effect on take-up by the deadline group on day-8", dig = 3)



# (2.6): Calculate take-up rates of each group.
reg_cum_daily$`Oct 6`$coefficients[1] %>% 
  print_pct("coef_accepted_offer_t2_d8.tex", "Take-up of no deadline, no reminder group on day 8")
reg_cum_daily$`Oct 6`$coefficients[2] %>% 
  print_pct("coef_accepted_offer_t3_d8.tex", "Take-up of no deadline, anticipated reminder group on day 8")
reg_cum_daily$`Oct 6`$coefficients[3] %>% 
  print_pct("coef_accepted_offer_t4_d8.tex", "Take-up of no deadline, unanticipated reminder group on day 8")
reg_cum_daily$`Oct 6`$coefficients[4] %>% 
  print_pct("coef_accepted_offer_t5_d8.tex", "Take-up of deadline, no reminder group on day 8")
reg_cum_daily$`Oct 6`$coefficients[5] %>% 
  print_pct("coef_accepted_offer_t6_d8.tex", "Take-up of deadline, anticipated reminder group on day 8")
reg_cum_daily$`Oct 6`$coefficients[6] %>% 
  print_pct("coef_accepted_offer_t7_d8.tex", "Take-up of deadline, unanticipated reminder group on day 8")

(reg_cum_daily_pooled_accepted_offer_dl$`Oct 6`$coefficients*100) %>%
  print_n(file = "coef_takeup_dl.tex", "Take-up of deadline on day 8", dig = 1)



#########################################
##    (3): Other number calculations.  ##
#########################################
# (3.1): fintech market share.
fintech_pos <- 130159
total_pos <- 3264750
mobile_pos <- 1299429 
# fintech POS / total POS
(fintech_pos / total_pos) %>% 
  print_pct("fintech_marketshare_total.tex")
# fintech POS / mPOS
(fintech_pos / mobile_pos) %>% 
  print_pct("fintech_marketshare_mpos.tex")

#######################################################################
##    (4): Calculate percentage firms opening terms and conditions.  ##
#######################################################################
# (4.1): Import main results.
results <- here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx") %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx"))

# (4.2): Split up excel workbook.
open_email <- results$`open - 1er envío`
rm(results)

# (4.3): Keep necessary variables in data.
open_email %<>%
  transmute(organization_uuid,
            open_terms_date = `T&C`,
            open_terms = as.numeric(!is.na(open_terms_date)))
open_email %>% tab(open_terms)

# (4.4): Merge in offer acceptance and email opening.
open_email %<>%
  inner_join(fintech_fee %>% 
              select(organization_uuid, accepted_offer_late, open_email_late, treat_description),
            by = "organization_uuid")
open_email %>% 
  filter(open_email_late == 1) %>% 
  group_by(accepted_offer_late) %>% 
  summarise(open_terms = mean(open_terms)) %>% 
  ungroup()

# (4.5): Print percentage firms that opened email that read terms and conditions.
open_email %>% 
  filter(open_email_late == 1) %>% 
  tab(open_terms, round = 4) %>% 
  filter(open_terms == 1) %>% 
  pull(prop) %>% 
  print_pct("stat_pct_open_terms.tex",
            "Percentage firms that opened email that read terms and conditions")

