#--------------------------------------------------------------------------------------------
# File name: 		      24b_elasticity_tables_shortterm.R
# Creation date:      2021-12-20
# Author:          		César Landín
# Files used:
#   - here("proc", "07b_panel_regressions.qs")
#   - here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
#   - here("proc", "fintech_fee_light.csv")
# Files created:
#   - Numbers in here("results", "numbers")
#   - here("results", "tables", "monthly_itt_tot.tex")
# Purpose:
# 	- Table 3: Monthly Sales Elasticity: Intent to Treat and Treatment on the Treated: This script generates short-term monthly card sales elasticity tables (ITT and TOT)
#     and prints coefficients to /results/numbers.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(lubridate)
library(fixest)
library(modelsummary)
library(qs)
library(scales)
library(tabulator)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "24a_elasticity_tables_functions.R"))
#########################################################

##################################################
##    (1): Import and process regression data.  ##
##################################################
# (1.1): Read in panel regression data.
panel_regressions <- qread(here("proc", "20_panel_regressions.qs"))
for (i in 1:length(panel_regressions)) {
  object <- panel_regressions[[i]]
  assign(names(panel_regressions)[i], object)
}
rm(panel_regressions)

# (1.2): Load monthly sales data for notes.
fintech_fee_dtm_adjusteddates <- qread(here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")) %>%
  filter(timestamp_month <= "2021-03-01") %>% 
  select(organization_uuid, timestamp_month, treat_description,
         log_valid_volume, arcsinh_valid_volume, log_nr_payments, 
         asinh_nr_payments, make_sale, treat_T1, valid_volume_w5, 
         nr_valid_payments, nr_valid_payments_w5, valid_volume, valid_volume_per_transaction, 
         valid_volume_w5_per_transaction, log_valid_volume_per_transaction, 
         asinh_valid_volume_per_transaction)
fintech_fee_dtm_adjusteddates %>% pull(timestamp_month) %>% min()

# (1.3): Generate dummy: firm made any number of transactions on or after current month.
fintech_fee_dtm_adjusteddates %<>%
  arrange(organization_uuid, desc(timestamp_month)) %>% 
  group_by(organization_uuid) %>% 
  mutate(made_sale_after = cumsum(make_sale)) %>% 
  arrange(organization_uuid, timestamp_month) %>% 
  mutate(made_sale_after = ifelse(made_sale_after > 0, 1, 0)) %>% 
  ungroup()

# # (1.4): Read in daily to monthly cash data with adjusted dates.
# cash_sample <- read_csv(here("proc", "cash_sample.csv"), show_col_types = FALSE)
# fintech_fee_dtm_adjusteddates %<>% left_join(cash_sample, by = c("organization_uuid"))
# rm(cash_sample)

# (1.6): Subset regression lists.
names(reg_monthly_itt_dtm_adjusteddates)
get_coef_names <- function(name, reg) {
  reg[[name]][["fml"]][[2]]
}
wp_outcomes = c("Log(sales + 1)", "Log(\\# transactions + 1)", "Continued using technology")
lapply(wp_outcomes, get_coef_names, reg_monthly_itt_dtm_adjusteddates)
reg_monthly_itt_dtm_adjusteddates %<>% .[wp_outcomes]
reg_monthly_tot_dtm_adjusteddates %<>% .[wp_outcomes]

# (1.7): Calculate percentage change in fee.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  mutate(old_fee = ifelse(commission_model == 'FIXED_RATE', 3.5, 3.75),
         new_fee = ifelse(fee_type == "2.75% offer", 2.75, 3.00),
         fee_reduction = (old_fee - new_fee)/old_fee) %>% 
  select(organization_uuid, contains("fee")) %>% 
  filter(!is.na(new_fee))
fee_reduction <- fintech_fee %>% pull(fee_reduction) %>% mean()

# (1.7): Convert and save coefficients.
# ITT coefs
get_coef("itt", "logsales") %>%
  print_pct("elasticity_itt_logsales.tex", "ITT effect on log(sales + 1)")
get_coef("itt", "logsales") %>%
  print_pct("pres_elasticity_itt_logsales.tex", "ITT effect on log(sales + 1)", dig = 0)
get_coef("itt", "logtransactions") %>%
  print_pct("elasticity_itt_logtransactions.tex", "ITT effect on log(# transactions + 1)")
round(get_coef("itt", "churn")*100, 1) %>%
  print_n("elasticity_itt_churn.tex", "ITT effect on making at least 1 sale")
round(get_coef("itt", "churn")*100, 1) %>%
  print_n("pres_elasticity_itt_churn.tex", "ITT effect on making at least 1 sale", dig = 0)
# TOT coefs
get_coef("tot", "logsales") %>%
  print_pct("elasticity_tot_logsales.tex", "TOT effect on log(sales + 1)")
get_coef("tot", "logtransactions") %>%
  print_pct("elasticity_tot_logtransactions.tex", "TOT effect on log(# transactions + 1)")
get_coef("tot", "logtransactions") %>%
  print_pct("pres_elasticity_tot_logtransactions.tex", "TOT effect on log(# transactions + 1)", dig = 0)
round(get_coef("tot", "churn")*100, 1) %>%
  print_n("elasticity_tot_churn.tex", "TOT effect on making at least 1 sale")
round(get_coef("tot", "churn")*100, 1) %>%
  print_n("pres_elasticity_tot_churn.tex", "TOT effect on making at least 1 sale", dig = 0)

# Control mean probability of making at least 1 sale.
get_control_mean(fintech_fee_dtm_adjusteddates, "made_sale_after") %>% 
  print_pct("elasticity_churn_control.tex", "Control mean probability of continuing to make sales.")
get_control_mean(fintech_fee_dtm_adjusteddates, "made_sale_after") %>%
  print_n("elasticity_churn_control_num.tex", "Control mean probability of continuing to make sales.")
# Treatment increase in probability of using technology
((get_coef("tot", "churn")*100) / (100 * get_control_mean(fintech_fee_dtm_adjusteddates, "made_sale_after"))) %>%
  print_pct("elasticity_churn_treatment_increase.tex", "Treatment increase in probability of continuing to use the payments technology")
# Average change in fee and sales elasticity
fee_reduction %>% 
  print_pct("pres_elasticity_change_fee.tex", "Average percentual change in fee", dig = 0)
(get_coef("tot", "logsales") / fee_reduction) %>% 
  print_n("pres_sales_elasticity.tex", "Sales elasticity", dig = 0)

#####################################################
##   (2): Generate individual ITT and TOT tables.  ##
#####################################################
# (2.1): Define additional rows.
additional_rows <- get_add_rows(fintech_fee_dtm_adjusteddates)

# (2.2): Generate ITT table.
gen_save_table(models = reg_monthly_itt_dtm_adjusteddates,
               sname = "monthly_itt_shortterm_card",
               lname = "Intent to Treat",
               pres = FALSE)

# (2.4): Generate TOT table.
gen_save_table(models = reg_monthly_tot_dtm_adjusteddates,
               sname = "monthly_tot_shortterm_card",
               lname = "Treatment on the Treated",
               pres = FALSE)

##############################################
##   (3): Generate combined ITT/TOT table.  ##
##############################################
# (3.1): Generate combined table.
gen_full_table("monthly_itt_shortterm_card") %>%
  write_(here("results", "tables", "monthly_itt_tot.tex"))
