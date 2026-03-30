#--------------------------------------------------------------------------------------------
# File name: 		      08a_regressions.R
# Creation date:      2021-10-21
# Author:          		Noah Forougi & César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "fintech_fee.csv")
# 	- here("proc", "survey_successful.csv")
#   - here("proc", "balance_table_data.csv"))
# Files created:
# 	- here("proc", "regressions.qs")
# 	- here("proc", "regs_for_figs.qs")
# Purpose:
# 	- Create the regressions laid out in the PAP.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(here)
library(tidyverse)
library(magrittr)
library(lubridate)
library(fixest)
library(modelsummary)
library(qs)
library(data.table)
library(tabulator)
library(conflicted)
library(multcomp)
conflict_prefer("month", "lubridate")
conflict_prefer("filter", "dplyr")
conflicts_prefer(dplyr::select)
#########################################################

###########################################################
##    (1): Import data and define regression functions.  ##
###########################################################
# (1.1): Import admin data.
fintech_fee <- read_csv(here("proc", "fintech_fee.csv"), show_col_types = FALSE)

# (1.2): Create objects for formatting regressions.
outcomes <- c("open_email_firstday", "open_email_ontime", "accepted_offer_ontime")
outcomes_postdl <- c( "open_email_late", "accepted_offer_late")
clean_outcomes <- c("Opened email first day", "Opened email by day of deadline", "Accepted offer")

# (1.3): Function for visualizing quickly regression outcomes.
mshow <- function(model) { 
  modelsummary(model[[1]], stars = c("*" = .1, "**" = .05, "***" = 0.01),
               gof_omit = "AIC|BIC|R2 Pseudo|R2 Within|Log.Lik.")
}

# (1.4): Create pre/post dataset.
fintech_fee_prepost <- fintech_fee %>% 
  select(organization_uuid, treat_type, treat_description, anticipated_reminder, unanticipated_reminder, reminder, no_reminder, deadline, no_deadline, 
         fee_2.75, high_gain,
         accept_fee_cum2020_09_29, accept_fee_cum2020_09_30, 
         accept_fee_cum2020_10_01, accept_fee_cum2020_10_02, 
         accept_fee_cum2020_10_03, accept_fee_cum2020_10_04, 
         accept_fee_cum2020_10_05, accept_fee_cum2020_10_06) %>% 
  pivot_longer(cols = 12:length(.)) %>%
  mutate(date = ymd(str_extract("2020.*$", string = name)),
         post = ifelse(date >= ymd("2020-10-05"), 1, 0))

# (1.5): Define daily outcomes.
day_outcomes <- names(fintech_fee)[names(fintech_fee) %>% str_detect("accept_fee_2020")][1:8]
day_cum_outcomes <-   names(fintech_fee)[names(fintech_fee) %>% str_detect("accept_fee_cum2020")][1:8]
day_outcomes_openemail <- names(fintech_fee)[names(fintech_fee) %>% str_detect("open_email_2020")][1:8]
day_cum_outcomes_openemail <-   names(fintech_fee)[names(fintech_fee) %>% str_detect("open_email_cum2020")][1:8]
day_cum_outcomes_openemailreminder <- names(fintech_fee)[names(fintech_fee) %>% str_detect("open_email_reminder_cum2020")][1:8]
clean_days <- paste(month(seq(ymd("2020-09-29"), ymd("2020-10-06"), by = "day"), label = T),
                    day(seq(ymd("2020-09-29"), ymd("2020-10-06"), by = "day")))

# (1.6): Define regression function.
reg_fe <- function(var, interaction_var = "", fe = "strata_fe", special_model = "", data = fintech_fee, wt = 1) {
  if (special_model != "") {
    model <- paste(var, "~", special_model)
  } else {
    model <- paste(var, "~", paste0("treat_T", c(2:8), interaction_var, collapse = " + "))
  }
  if (!is.na(fe)) {
    reg <- feols(fml = as.formula(model), # Not clustered since randomization unit is individual firm.
                 fixef = fe,
                 data = data, 
                 se = "hetero",
                 weights = wt,
                 lean = FALSE)
  } else {
    reg <- feols(fml = as.formula(model), # Not clustered since randomization unit is individual firm.
                 data = data, 
                 se = "hetero",
                 weights = wt,
                 lean = FALSE)
  }
  reg <- reg[!str_detect(names(reg), "residuals|sumFE|fitted.values|scores|fml|call|cov.iid|cov.unscaled|summary_flags|fixef_id")]
  class(reg) <- "fixest"
  return(reg)
}

# (1.7): Apply reg_fe to a list of outcomes.
reg_fe_all <- function(current_outcomes = outcomes, 
                       interaction_var = "", 
                       fe = "strata_fe", 
                       special_model = "", 
                       data = fintech_fee, 
                       wt = 1, 
                       model_names = clean_outcomes) {
  current_regs <- lapply(current_outcomes, 
                         reg_fe, 
                         interaction_var = interaction_var,
                         fe = fe,
                         special_model = special_model,
                         data = data,
                         wt = wt)
  names(current_regs) <- model_names
  return(current_regs)
}

# (1.8): Import survey data and generate above median number of employees variable.
survey_data <- read_csv(here("proc", "survey_successful.csv"))
survey_data %<>% 
  mutate(above_median_employees = ifelse(nb_employees >= median(nb_employees, na.rm = TRUE), 1, 0),
         more_one_employee = ifelse(nb_employees > 1, 1, 0),
         in_survey = 1) %>% 
  select(organization_uuid, above_median_employees, more_one_employee, nb_employees, in_survey, ipw_takeup)
survey_data %>% tab(more_one_employee)

# (1.9): Merge variable with master dataset.
fintech_fee %<>% 
  left_join(survey_data, by = "organization_uuid") %>% 
  replace_na(list(in_survey = 0))
fintech_fee %>% tab(in_survey)

##################################
##    (2): Main specification.  ##
##################################
# (2.1): Main regression specification.
reg_main_notext_fe <- lapply(outcomes, reg_fe)

# (2.2): Main regression specification with text fixed effects.
fintech_fee %<>%
  mutate(text_fe_10am = as.factor(messages_10am),
         text_fe_1005am = as.factor(messages_1005am))
reg_main_textfe <- reg_fe_all(special_model = "treat_T2 + treat_T3 + treat_T4 + treat_T5 + treat_T6 + treat_T7 + treat_T8 + as.factor(text_fe_10am) + as.factor(text_fe_1005am)")
reg_main <- c(reg_main_notext_fe, reg_main_textfe)
names(reg_main) <- rep(clean_outcomes, 2)

################################
##    (3): Robustness tests.  ##
################################
# (3.1): Robustness test 1: extending dates.
extend_dates_outcomes <- c("open_email_firstday", "open_email_late", "accepted_offer_late")
reg_rob_dates <- reg_fe_all(current_outcomes = extend_dates_outcomes)

# (3.2): Robustness test 2: daily regressions.  #
# Daily effects for completing application
reg_rob_daily <- reg_fe_all(current_outcomes = day_outcomes, model_names = clean_days)

# Daily cumulative effects for completing application
reg_cum_daily <- reg_fe_all(current_outcomes = day_cum_outcomes, model_names = clean_days)

# Daily effects for opening email
reg_rob_daily_openemail <- reg_fe_all(current_outcomes = day_outcomes_openemail, model_names = clean_days)

# Daily cumulative effects for opening email
reg_cum_daily_pooled_accepted_offer_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, model_names = clean_days)

# Daily effects for lower fee
reg_cum_daily_fee <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fee_2.75",
                                data = fintech_fee %>% filter(treat_T1 != 1 & treat_T8 != 1), fe = NA,
                                model_names = clean_days)

# Daily effects for lower fee (including same-day (fast) deadline)

# reg_cum_daily_fee_fast_deadline <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fee_2.75",
#                                               data = fintech_fee %>% filter(treat_T1 != 1), fe = NA,
#                                               model_names = clean_days)
reg_cum_daily_fee_fast_deadline <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fee_2.75 + control",
                                data = fintech_fee %>%
                                  mutate(fee_2.75 = ifelse(is.na(fee_2.75), 0, fee_2.75)), fe = NA,
                                model_names = clean_days)
reg_cum_daily_fee_independent_fast_deadline <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fee_2.75_control + fee_3_control",
                                              data = fintech_fee %>%
                                                mutate(fee_2.75_control = ifelse(fee_2.75 == 1, 1, 0)) %>% 
                                                mutate(fee_2.75_control = ifelse(control == 1, 0, fee_2.75_control)) %>%
                                                mutate(fee_3_control = ifelse(fee_2.75 == 0, 1, 0)) %>%
                                                mutate(fee_3_control = ifelse(control == 1, 0, fee_3_control)),
                                              model_names = clean_days)

# (3.3): Robustness test 3: weekly regressions.
# PAP: (iv) weekly effects by estimating separate regressions for each week since the beginning of the experiment.
# PAP: For items iii and iv we will use two specifications: one with cumulative take-up by that day or week and 
#      another with take-up just in that day or week as the outcome.
weekly_outcomes <- names(fintech_fee)[names(fintech_fee) %>% str_detect("accept_fee_week")]
weekly_cum_outcomes <- names(fintech_fee)[names(fintech_fee) %>% str_detect("accept_fee_cumweek")]
weekly_outcomes_openemail <- names(fintech_fee)[names(fintech_fee) %>% str_detect("open_email_week")]
weekly_cum_outcomes_openemail <- names(fintech_fee)[names(fintech_fee) %>% str_detect("open_email_cumweek")]
clean_weeks <- c("Week 1", "Week 2", "Week 3")

# Weekly effect for opening email 
reg_rob_week_openemail <- reg_fe_all(current_outcomes = weekly_outcomes_openemail, model_names = clean_weeks)

# Weekly effect for accepting fee
reg_rob_week <- reg_fe_all(current_outcomes = weekly_outcomes, model_names = clean_weeks)

# Weekly cumulative effect for opening email
reg_rob_cum_week_openemail <- reg_fe_all(current_outcomes = weekly_cum_outcomes_openemail, model_names = clean_weeks)

# Weekly cumulative effect for accepting fee
reg_rob_cum_week <- reg_fe_all(current_outcomes = weekly_cum_outcomes, model_names = clean_weeks)

# (3.4): Robustness test 4: pooled results.
# Effect of an anticipated reminder
reg_pooled_antreminder <- reg_fe_all(special_model = "anticipated_reminder")

# Effect of an  reminder
reg_pooled_reminder <- reg_fe_all(special_model = "reminder")

# Effect of a deadline
reg_pooled_deadline <- reg_fe_all(special_model = "deadline")

# (3.5): Robustness test 5: conditional on opening first email.
reg_rob_openedfirstday <- reg_fe_all(current_outcomes = "accepted_offer_ontime", 
                                     special_model = "treat_T3 + treat_T4 + treat_T5 + treat_T6 + treat_T7 + treat_T8",
                                     data = fintech_fee %>% filter(open_email_firstday == 1),
                                     model_names = "Accepted offer")

# (3.6): Robustness test 6: only business fixed effects.
reg_rob_onlybusinessfe <- reg_fe_all(fe = "new_buss_type")
reg_rob_fe <- c(reg_main_notext_fe, reg_rob_onlybusinessfe)
names(reg_rob_fe) <- rep(clean_outcomes, 2)

# (3.7): Robustness test 7: use actual sales quartiles * business type as strata FE.
reg_rob_actualsalesq <- reg_fe_all(fe = "strata_fe_actualsalesq")
reg_rob_actualsalesq <- c(reg_main_notext_fe, reg_rob_actualsalesq)
names(reg_rob_actualsalesq) <- rep(clean_outcomes, 2)

###############################
##    (4): Heterogeneities.  ##
###############################
# (4.1): Heterogeneity test 1: baseline sales.
fintech_fee %<>% mutate(salesq_fac = case_when(sales_quartiles == 1 ~ "quartile1", 
                                               sales_quartiles == 2 ~ "quartile2", 
                                               sales_quartiles == 3 ~ "quartile3", 
                                               sales_quartiles == 4 ~ "quartile4")) %>%
  mutate(salesq_fac = as.factor(salesq_fac))
reg_het_sales <- reg_fe_all(interaction_var = "*salesq_fac")

# (4.2): Heterogeneity test 2: business type.
fintech_fee %<>% mutate(new_buss_type = as.factor(new_buss_type))

reg_het_buss <- reg_fe_all(interaction_var = "*new_buss_type")
# Variables removed because of collinearity: factor variables.

# (4.3): Heterogeneity test 3: gender.
reg_het_gender <- reg_fe_all(interaction_var = "*gender")

# (4.3): Heterogeneity test 4: length of time using technology.
reg_het_length <- reg_fe_all(interaction_var = "*length")

# (4.4): Heterogeneity test 5: new fee.
# Note: use colons to specify the interaction ONLY. If we run this with treat_TX*lower_fee, R forces all the interactions AND the 
# simple dummy for the lower fee indicator. The way we regress it below shows that the lower fee indicator gets absorbed by all 
# the interaction terms. 
reg_het_newfee <- reg_fe_all(special_model = "treat_T2 + treat_T2:lower_fee + treat_T3 + treat_T3:lower_fee + treat_T4 + treat_T4:lower_fee + treat_T5 + treat_T5:lower_fee + treat_T6 + treat_T6:lower_fee + treat_T7 + treat_T7:lower_fee + treat_T8 + treat_T8:lower_fee")

# (4.5): Heterogeneity test 6: tax registration status.
fintech_fee %<>% mutate(customer_type = as.factor(customer_type))

reg_het_taxreg <- reg_fe_all(interaction_var = "*customer_type")
# Variables removed because of collinearity: factor variables.

# (4.6): Heterogeneity test 7: above/below median sales.
reg_het_hilowsales <- reg_fe_all(interaction_var = "*hilow_sales")

# (4.7): Heterogeneity test 8: expected benefit.
fintech_fee %<>% mutate(gain_quartiles = as.factor(gain_quartiles))

reg_het_gain <- reg_fe_all(interaction_var = "*gain_quartiles")

# (4.8): Heterogeneity test 9: expected benefit by above.
fintech_fee %<>% mutate(gain_hilow = as.factor(gain_hilow))

reg_het_gainhilow <- reg_fe_all(interaction_var = "*gain_hilow")

# (4.9): Heterogeneity test 10: covid-19 shock.
fintech_fee$covid_shock <- as.factor(fintech_fee$covid_shock)

reg_het_covidshock <- reg_fe_all(interaction_var = "*above_median_covid_shock")

#################################
##    (5): Text regressions.  ##
#################################
# (5.1): 10 am texts.
reg_text_10am <- reg_fe_all(special_model = "received_4_10am + received_3_10am + received_2_10am + received_1_10am")

# (5.1): 10:05 am texts.
reg_text_1005am <- reg_fe_all(special_model = "received_2_1005am")

####################################
##    (6): Reminder regressions.  ##
####################################
# (6.1): Split up.
reg_opened_reminder <- reg_fe_all(current_outcomes = "open_reminder_ontime", special_model = "treat_T4 + treat_T6 + treat_T7",
                                  data = fintech_fee %>% filter(treat_type %in% c("T3", "T4", "T6", "T7")),
                                  model_names = "Open Reminder")

# (6.2): Pooled.
reg_opened_reminder_pooled <- reg_fe_all(current_outcomes = "open_reminder_ontime", special_model = "anticipated_reminder",
                                         data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1),
                                         model_names = "Open Reminder")

##################################
##    (7): Pooled regressions.  ##
##################################
# (7.1): Pooled group 1: Anticipated reminder vs. unanticipated reminder.
# Heterogeneity by high/low baseline sales
reg_pooled_het_antrem_hilowsales <- reg_fe_all(special_model = "treat_T2:high_sales + anticipated_reminder*high_sales + unanticipated_reminder*high_sales",
                                               data = fintech_fee %>% filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1))

fintech_fee %>% filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
  tab(anticipated_reminder, unanticipated_reminder, high_sales) %>% 
  arrange(anticipated_reminder, unanticipated_reminder, high_sales)

# Heterogeneity by new/old users
reg_pooled_het_antrem_length <- reg_fe_all(special_model = "treat_T2:old_user +  anticipated_reminder*old_user + unanticipated_reminder*old_user",
                                           data = fintech_fee %>% filter(treat_T2 == 1 |anticipated_reminder == 1 | unanticipated_reminder == 1))

# Heterogeneity by new fee type 
reg_pooled_het_antrem_feetype <- reg_fe_all(special_model = "treat_T2:fee_2.75 + anticipated_reminder*fee_2.75 + unanticipated_reminder*fee_2.75",
                                            data = fintech_fee %>% filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1))
fintech_fee %>% filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
  tab(treat_T2, anticipated_reminder, unanticipated_reminder, fee_2.75) %>% 
  arrange(anticipated_reminder, unanticipated_reminder, fee_2.75)

# Heterogeneity by high/low expected gain
reg_pooled_het_antrem_gain <- reg_fe_all(special_model = "treat_T2:high_gain + anticipated_reminder*high_gain + unanticipated_reminder*high_gain",
                                         data = fintech_fee %>% filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1))
fintech_fee %>% filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
  tab(treat_T2, anticipated_reminder, unanticipated_reminder, high_gain) %>% 
  arrange(anticipated_reminder, unanticipated_reminder, high_gain)

# Heterogeneity by commission model
reg_pooled_het_commission <- reg_fe_all(special_model = "treat_T2:fixed_rate + anticipated_reminder*fixed_rate + unanticipated_reminder*fixed_rate",
                                        data = fintech_fee %>% 
                                          filter(treat_T2 == 1 | anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                          filter(sales <= 20000))

# (7.2): Pooled Group 2: Reminder vs. no reminder.
# Heterogeneity by high/low baseline sales
reg_pooled_het_commission <- reg_fe_all(special_model = "reminder*high_sales",
                                        data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Heterogeneity by new/old users
reg_pooled_het_rem_length <- reg_fe_all(special_model = "reminder*old_user",
                                        data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Heterogeneity by new fee type 
reg_pooled_het_rem_feetype <- reg_fe_all(special_model = "reminder*fee_2.75",
                                         data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Heterogeneity by high/low expected gain
reg_pooled_het_rem_gain <- reg_fe_all(special_model = "reminder*high_gain",
                                      data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Heterogeneity by commission model
reg_pooled_het_rem_commission <- reg_fe_all(special_model = "reminder*fixed_rate",
                                            data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# (7.3): Pooled Group 3: Deadline vs. no deadline.
# Heterogeneity by high/low baseline sales
reg_pooled_het_DL_hilowsales <- reg_fe_all(special_model = "deadline*high_sales",
                                           data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Heterogeneity by new/old users
reg_pooled_het_DL_length <- reg_fe_all(special_model = "deadline*old_user",
                                       data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Heterogeneity by new fee type 
reg_pooled_het_DL_feetype <- reg_fe_all(special_model = "deadline*fee_2.75",
                                        data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Heterogeneity by high/low expected gain
reg_pooled_het_DL_gain <- reg_fe_all(special_model = "deadline*high_gain",
                                     data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Heterogeneity by high/low expected gain
reg_pooled_het_DL_commission <- reg_fe_all(special_model = "deadline*fixed_rate",
                                           data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))


######################################################
##    (8): Email and reminder opening regressions.  ##
######################################################
# (8.1): Daily regressions, email opening.
# Daily effects for anticipated reminder vs. unanticipated reminder
reg_cum_daily_pooled_open_email_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "anticipated_reminder",
                                                     data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Daily effects for reminder vs. no reminder
reg_cum_daily_pooled_open_email_rem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "reminder",
                                                  data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Daily effects for deadline vs. no deadline
reg_cum_daily_pooled_open_email_dl <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "deadline",
                                                 data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Daily effects for 2.75 vs. 3.00 fee
reg_cum_daily_pooled_open_email_fee <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "fee_2.75",
                                                  data = fintech_fee %>% filter(fee_2.75 == 1 | fee_2.75 == 0))

# Daily effects for unanticipated reminder vs. no reminder
reg_cum_daily_pooled_open_email_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "unanticipated_reminder",
                                                       data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1))

# (8.2): Robustness test: daily regressions, email opening, interacting treatment groups.
# Reminder vs. deadline
reg_cum_daily_het_open_email_dl_rem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "deadline*reminder",
                                                  data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Deadline vs. anticipated reminder
reg_cum_daily_het_open_email_dl_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemail, special_model = "anticipated_reminder*deadline",
                                                     data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# (8.3): Daily regressions, email or reminder opening.
# Daily effects for anticipated reminder vs. unanticipated reminder
reg_cum_daily_pooled_open_erem_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "anticipated_reminder",
                                                    data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Daily effects for reminder vs. no reminder
reg_cum_daily_pooled_open_erem_rem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "reminder",
                                                 data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Daily effects for deadline vs. no deadline
reg_cum_daily_pooled_open_erem_dl <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "deadline",
                                                data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Daily effects for 2.75 vs. 3.00 fee
reg_cum_daily_pooled_open_erem_fee <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "fee_2.75",
                                                 data = fintech_fee %>% filter(fee_2.75 == 1 | fee_2.75 == 0))

# Daily effects for unanticipated reminder vs. no reminder
reg_cum_daily_pooled_open_erem_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "unanticipated_reminder",
                                                      data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1))

# (8.4): Robustness test: daily regressions, email or reminder opening, interacting treatment groups.
# Reminder vs. deadline
reg_cum_daily_het_open_erem_dl_rem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "deadline*reminder",
                                                 data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Deadline vs. anticipated reminder
reg_cum_daily_het_open_erem_dl_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes_openemailreminder, special_model = "anticipated_reminder*deadline",
                                                    data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))


#############################################
##    (8): Pooled regressions robustness.  ##
#############################################
# (8.1): Robustness test 1: daily regressions for pooled groups.
# Daily effects for Anticipated reminder vs. unanticipated reminder
reg_cum_daily_pooled_accepted_offer_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                         data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))
# + conditional on getting a 2.75 fee.
reg_cum_daily_pooled_accepted_offer_antrem_2.75 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                              data = fintech_fee %>% 
                                                                filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                filter(fee_type == "2.75% offer"))
# + conditional on getting a 3.00 fee.
reg_cum_daily_pooled_accepted_offer_antrem_3 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                           data = fintech_fee %>% 
                                                             filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                             filter(fee_type == "3% offer"))
# + conditional on opening email.
reg_cum_daily_pooled_accepted_offer_antrem_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                                   data = fintech_fee %>% 
                                                                     filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                     filter(open_email_before_reminder == 1))
# + conditional on opening email and getting a 2.75 fee.
reg_cum_daily_pooled_accepted_offer_antrem_openemail_2.75 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                                        data = fintech_fee %>% 
                                                                          filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                          filter(open_email_before_reminder == 1) %>% 
                                                                          filter(fee_type == "2.75% offer"))
# + conditional on opening email and getting a 3.00 fee.
reg_cum_daily_pooled_accepted_offer_antrem_openemail_3 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                                     data = fintech_fee %>% 
                                                                       filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                       filter(open_email_before_reminder == 1) %>% 
                                                                       filter(fee_type == "3% offer"))

# Daily effects for Reminder vs. no reminder
reg_cum_daily_pooled_accepted_offer_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
                                                      data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))
# reg_cum_daily_pooled_accepted_offer_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fast_deadline + no_reminder + reminder",
#                                                       data = fintech_fee) Changes by Mohammad
# reg_cum_daily_pooled_accepted_offer_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "unanticipated_reminder",
#                                                       data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1)) Changes by Mohammad

# + conditional on opening email
reg_cum_daily_pooled_accepted_offer_rem_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
                                                                data = fintech_fee %>% 
                                                                  filter(reminder == 1 | no_reminder == 1) %>% 
                                                                  filter(open_email_before_reminder == 1))

# Daily effects for Deadline vs. no deadline
reg_cum_daily_pooled_accepted_offer_dl <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
                                                     data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))
# + conditional on opening email.
reg_cum_daily_pooled_accepted_offer_dl_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
                                                               data = fintech_fee %>% 
                                                                 filter(deadline == 1 | no_deadline == 1) %>% 
                                                                 filter(open_email_before_reminder == 1))

# Daily effects for 2.75 vs. 3.00 fee
reg_cum_daily_pooled_accepted_offer_fee <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fee_2.75",
                                                      data = fintech_fee %>% filter(fee_2.75 == 1 | fee_2.75 == 0))
# + conditional on opening email.
reg_cum_daily_pooled_accepted_offer_fee_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "fee_2.75",
                                                                data = fintech_fee %>% 
                                                                  filter(fee_2.75 == 1 | fee_2.75 == 0) %>% 
                                                                  filter(open_email_before_reminder == 1))


# Daily effects for unanticipated reminder vs. no reminder
reg_cum_daily_pooled_accepted_offer_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "unanticipated_reminder",
                                                           data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1))
# + conditional on opening email
reg_cum_daily_pooled_accepted_offer_unantrem_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "unanticipated_reminder",
                                                                     data = fintech_fee %>% 
                                                                       filter(unanticipated_reminder == 1 | no_reminder == 1) %>% 
                                                                       filter(open_email_before_reminder == 1))

###### Non-heterogeneous effects of each treatment in the survey sample. 
# Daily effects for Reminder vs. no reminder IN SURVEY SAMPLE
reg_cum_daily_pooled_accepted_offer_rem_survey <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
                                                             data = fintech_fee %>% 
                                                               filter(reminder == 1 | no_reminder == 1) %>% 
                                                               filter(in_survey == 1))
# Weighted version
reg_cum_daily_pooled_accepted_offer_rem_survey_wt <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
                                                                data = fintech_fee %>% 
                                                                  filter(reminder == 1 | no_reminder == 1) %>% 
                                                                  filter(in_survey == 1),
                                                                wt = ~ipw_takeup)

# Daily effects for Deadline vs. no deadline IN SURVEY SAMPLE
reg_cum_daily_pooled_accepted_offer_dl_survey <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
                                                            data = fintech_fee %>% 
                                                              filter(deadline == 1 | no_deadline == 1) %>% 
                                                              filter(in_survey == 1))
# Weighted version
reg_cum_daily_pooled_accepted_offer_dl_survey_wt <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
                                                               data = fintech_fee %>% 
                                                                 filter(deadline == 1 | no_deadline == 1) %>% 
                                                                 filter(in_survey == 1),
                                                               wt = ~ipw_takeup)

# Daily effects for Anticipated reminder vs. unanticipated reminder IN SURVEY SAMPLE
reg_cum_daily_pooled_accepted_offer_antrem_survey <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                                data = fintech_fee %>% 
                                                                  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                  filter(in_survey == 1))
# Weighted version
reg_cum_daily_pooled_accepted_offer_antrem_survey_wt <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                                   data = fintech_fee %>% 
                                                                     filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                     filter(in_survey == 1),
                                                                   wt = ~ipw_takeup)

# (8.2): Robustness test 2: daily regressions for pooled groups with heterogeneity on baseline sales.
# Anticipated reminder vs. unanticipated reminder
reg_cum_daily_het_accepted_offer_antrem_hilowsales <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*high_sales",
                                                                 data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Reminder vs. no reminder
reg_cum_daily_het_accepted_offer_rem_hilowsales <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder*high_sales",
                                                              data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Deadline vs.  No Deadline
reg_cum_daily_het_accepted_offer_DL_hilowsales <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline*high_sales",
                                                             data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# (8.3): Robustness test 3: daily regressions for pooled groups with heterogeneity on high/low expected gain.
# Anticipated reminder vs. unanticipated reminder
reg_cum_daily_het_accepted_offer_antrem_hilowgain <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*high_gain",
                                                                data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Reminder vs. no reminder
reg_cum_daily_het_accepted_offer_rem_hilowgain <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder*high_gain",
                                                             data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Deadline vs.  No Deadline
reg_cum_daily_het_accepted_offer_DL_hilowgain <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline*high_gain",
                                                            data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# (8.4): Robustness test 4: daily regressions for pooled groups with heterogeneity on male/female/unknown.
fintech_fee %<>%
  mutate(male = if_else(gender == "Male", 1, 0, 0), 
         female = if_else(gender == "Female", 1, 0, 0), 
         unknown = if_else(gender == "Missing", 1, 0, 0))

# Anticipated reminder vs. unanticipated reminder
reg_cum_daily_het_accepted_offer_antrem_gender <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*male + anticipated_reminder*unknown",
                                                             data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Reminder vs. no reminder
reg_cum_daily_het_accepted_offer_rem_gender <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder*male + reminder*unknown",
                                                          data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Deadline vs. no deadline
reg_cum_daily_het_accepted_offer_DL_gender <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline*male + deadline*unknown",
                                                         data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# (8.5): Robustness test 5: daily regressions for pooled groups with heterogeneity on 3.00/2.75.
# Anticipated reminder vs. unanticipated reminder
reg_cum_daily_het_accepted_offer_antrem_fee <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*fee_2.75",
                                                          data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))
# Conditional on opening email.
reg_cum_daily_het_accepted_offer_antrem_fee_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*fee_2.75",
                                                                    data = fintech_fee %>% 
                                                                      filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
                                                                      filter(open_email_before_reminder == 1))

# Reminder vs. no reminder
reg_cum_daily_het_accepted_offer_rem_fee <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder*fee_2.75",
                                                       data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))
# Conditional on opening email.
reg_cum_daily_het_accepted_offer_rem_fee_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder*fee_2.75",
                                                                 data = fintech_fee %>% 
                                                                   filter(reminder == 1 | no_reminder == 1) %>% 
                                                                   filter(open_email_before_reminder == 1))

# Deadline vs. no deadline
reg_cum_daily_het_accepted_offer_dl_fee <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline*fee_2.75",
                                                      data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))
# Conditional on opening email.
reg_cum_daily_het_accepted_offer_dl_fee_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline*fee_2.75",
                                                                data = fintech_fee %>% 
                                                                  filter(deadline == 1 | no_deadline == 1) %>% 
                                                                  filter(open_email_before_reminder == 1))

# Unanticipated reminder vs. no reminder
reg_cum_daily_het_accepted_offer_unantrem_fee <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "unanticipated_reminder*fee_2.75",
                                                            data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1)) 

# Conditional on opening email.
reg_cum_daily_het_accepted_offer_unantrem_fee_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "unanticipated_reminder*fee_2.75",
                                                                      data = fintech_fee %>% 
                                                                        filter(unanticipated_reminder == 1 | no_reminder == 1) %>% 
                                                                        filter(open_email_before_reminder == 1))

# (8.6): Robustness test 6: daily regressions interacting treatment groups.
# Reminder vs. deadline
reg_cum_daily_het_accepted_offer_dl_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline*reminder",
                                                      data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Deadline vs. anticipated reminder
reg_cum_daily_het_accepted_offer_dl_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*deadline",
                                                         data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))
reg_cum_daily_het_accepted_offer_dl_antrem_no_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*deadline",
                                                         data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

reg_cum_daily_het_accepted_offer_dl_antrem_no_reminder <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*deadline",
                                                                data = fintech_fee %>% filter(anticipated_reminder == 1 | no_reminder == 1))

# Deadline vs. unanticipated reminder
reg_cum_daily_het_accepted_offer_dl_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "unanticipated_reminder*deadline",
                                                           data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1))

reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder*unanticipated_reminder*deadline",
                                                           data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1 | no_reminder == 1))

# Conducting a first day statistical test for cases:
# Announced vs no reminder - p-value of the diff between the two estimates 
# Announced vs unannounced - p-value of the diff between the two estimates
# Announced vs (unannounced and no reminder pooled together) - p-value of the diff between the two estimates
reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday <- feols(accept_fee_cum2020_09_29 ~ anticipated_reminder*unanticipated_reminder*deadline,
                                                                          fixef = "strata_fe",
                                                                          se = "hetero",
                                                                          data = fintech_fee %>% 
                                                                            filter(anticipated_reminder == 1 | unanticipated_reminder == 1 | no_reminder == 1))
reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday_fullsample <- feols(accept_fee_cum2020_09_29 ~ anticipated_reminder*unanticipated_reminder*deadline + control + fast_deadline,
                                                                          fixef = "strata_fe",
                                                                          se = "hetero",
                                                                          data = fintech_fee)

glht_test_1 <- glht(reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday, linfct = "anticipated_reminder:deadline = 0")
summary(glht_test_1)
# p_value <- summary(glht_test_1)$test$pvalues[1]
# formatted_p_value <- sprintf("%.3f", p_value)
# writeLines(formatted_p_value, con = "../results/numbers/.tex")

glht_test_2 <- glht(reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday, linfct = "anticipated_reminder:deadline - unanticipated_reminder:deadline = 0")
summary(glht_test_2)

reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday <- feols(accept_fee_cum2020_09_29 ~ anticipated_reminder*deadline,
                                                                          fixef = "strata_fe",
                                                                          se = "hetero",
                                                                          data = fintech_fee %>% 
                                                                            filter(anticipated_reminder == 1 | unanticipated_reminder == 1 | no_reminder == 1))

reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday_fullsample <- feols(accept_fee_cum2020_09_29 ~ anticipated_reminder*deadline + control + fast_deadline,
                                                                              fixef = "strata_fe",
                                                                              se = "hetero",
                                                                              data = fintech_fee)

glht_test_3 <- glht(reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday_fullsample, linfct = "anticipated_reminder:deadline = 0")
summary(glht_test_3)

# (8.7): Robustness test 7: daily regressions interacting above median sales.
# Calculate above / below median baseline sales.
baseline_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
  select(organization_uuid, valid_volume_w5) %>% 
  mutate(median_sales = median(valid_volume_w5),
         above_median_sales = ifelse(valid_volume_w5 >= median_sales, 1, 0))
fintech_fee %<>% left_join(baseline_data %>% select(-median_sales), by = "organization_uuid")

# Reminder vs. above median sales
reg_cum_daily_het_accepted_offer_fsize_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_sales*reminder",
                                                         data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Anticipated reminder vs. above median sales
reg_cum_daily_het_accepted_offer_fsize_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_sales*anticipated_reminder",
                                                            data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Deadline vs. above median sales
reg_cum_daily_het_accepted_offer_fsize_dl <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_sales*deadline",
                                                        data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Unanticipated reminder vs. above median sales
reg_cum_daily_het_accepted_offer_fsize_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_sales*unanticipated_reminder",
                                                              data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1))

# (8.7): Robustness test 7: daily regressions interacting above median expected gain.
# Calculate above / below median expected gain.
# (1.5): Calculate above / below median baseline sales.
baseline_data <- read_csv(here("proc", "balance_table_data.csv"), show_col_types = FALSE) %>% 
  select(organization_uuid, valid_volume_w5, valid_volume)
fintech_fee %<>% 
  left_join(baseline_data, by = "organization_uuid")

# (1.4): Calculate expected gain
fintech_fee %<>%
  # Multiply pre-experiment sales by fee reduction
  mutate(old_fee = ifelse(commission_model == 'FIXED_RATE', 3.5, 3.75),
         new_fee = ifelse(fee_type == "2.75% offer", 2.75, 3.00),
         fee_reduction = (old_fee - new_fee)/100,
         savings_potential = valid_volume * fee_reduction)

# (1.7): Calculate above / below median expected gain.
fintech_fee %<>% 
  mutate(above_median_expected_gain = ifelse(savings_potential >= median(savings_potential, na.rm = TRUE), 1, 0))

# Reminder vs. above median expected gain
reg_cum_daily_het_accepted_offer_expgain_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_expected_gain*reminder",
                                                           data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Anticipated reminder vs. above median expected gain
reg_cum_daily_het_accepted_offer_expgain_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_expected_gain*anticipated_reminder",
                                                              data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Deadline vs. above median expected gain
reg_cum_daily_het_accepted_offer_expgain_dl <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_expected_gain*deadline",
                                                          data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# Unanticipated reminder vs. above median expected gain
reg_cum_daily_het_accepted_offer_expgain_unantrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_expected_gain*unanticipated_reminder",
                                                                data = fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1))


# (8.8): Robustness test 8: daily regressions interacting with time using Fintech.
# Calculate above / below median baseline sales.
usage_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
  select(organization_uuid, months_since_ft)
fintech_fee %<>% 
  left_join(usage_data, by = "organization_uuid") %>% 
  mutate(above_median_usage = ifelse(months_since_ft > median(months_since_ft), 1, 0))
rm(usage_data)

# Reminder vs. above median sales
reg_cum_daily_het_accepted_offer_usage_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_usage*reminder",
                                                         data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Anticipated reminder vs. above median sales
reg_cum_daily_het_accepted_offer_usage_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_usage*anticipated_reminder",
                                                            data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Deadline vs. above median sales
reg_cum_daily_het_accepted_offer_usage_dl <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "above_median_usage*deadline",
                                                        data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

# (8.9): Robustness test 9: daily regressions for pooled groups, accept that day.
# Daily effects for Anticipated reminder vs. unanticipated reminder
reg_cum_daily_pooled_accepted_offer_accdayof_antrem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
                                                                  data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1))

# Daily effects for Reminder vs. no reminder
reg_cum_daily_pooled_accepted_offer_accdayof_rem <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
                                                               data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1))

# Daily effects for Deadline vs. no deadline
reg_cum_daily_pooled_accepted_offer_accdayof_deadline <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
                                                                    data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1))

#############################################
##    (9): Pooled regressions robustness.  ##
#############################################
# (9.1): Pre/Post 1: Pooled groups.
# Reminder vs. no reminder
reg_pooled_prepost_rem <- lapply("value", reg_fe, special_model = "reminder + reminder:post",
                                 data = fintech_fee_prepost %>% filter(reminder == 1 | no_reminder == 1),
                                 fe = "date")

# Deadline vs. no deadline
reg_pooled_prepost_DL <- lapply("value", reg_fe, special_model = "deadline + deadline:post",
                                data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1), 
                                fe = "date")

# Anticipated reminder vs. unanticipated reminder
reg_pooled_prepost_antrem <- lapply("value", reg_fe, special_model = "anticipated_reminder  + anticipated_reminder:post",
                                    data = fintech_fee_prepost %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1), 
                                    fe = "date")

# (9.2): Pre/Post 2: Pooled groups w. heterogeneity on new fee type.
# Reminder vs. no reminder
groups = c(0,1)
reg_pooled_prepost_rem_het_fee_2.75 <- lapply(groups, function(x) { 
  feols(fml = value ~ reminder + reminder:post, 
        data = fintech_fee_prepost %>% filter(reminder == 1 | no_reminder == 1) %>% filter(fee_2.75 == x), 
        fixef = c("date"), 
        cluster = "organization_uuid"
  )})
names(reg_pooled_prepost_rem_het_fee_2.75) <- c("3.00% Fee", "2.75% Fee")

# Deadline vs. no deadline
reg_pooled_prepost_DL_het_fee_2.75 <- lapply(groups, function(x) { 
  feols(fml = value ~ deadline + deadline:post, 
        data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1) %>% filter(fee_2.75 == x), 
        fixef = c("date"), 
        cluster = "organization_uuid"
  )})
names(reg_pooled_prepost_DL_het_fee_2.75) <- c("3.00% Fee", "2.75% Fee")

# Anticipated reminder vs. unanticipated reminder
reg_pooled_prepost_antrem_het_fee_2.75 <- lapply(groups, function(x) { 
  feols(fml = value ~ anticipated_reminder + anticipated_reminder:post, 
        data = fintech_fee_prepost %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% filter(fee_2.75 == x), 
        fixef = c("date"), 
        cluster = "organization_uuid"
  )})
names(reg_pooled_prepost_antrem_het_fee_2.75) <- c("3.00% Fee", "2.75% Fee")

# (9.3): Pre/Post 3: Pooled groups w. heterogeneity on high low expected gain.
# Reminder vs. no reminder
groups = c(0,1)
reg_pooled_prepost_rem_het_highgain <- lapply(groups, function(x) { 
  feols(fml = value ~ reminder + reminder:post, 
        data = fintech_fee_prepost %>% filter(reminder == 1 | no_reminder == 1) %>% filter(high_gain == x), 
        fixef = c("date"), 
        cluster = "organization_uuid"
  )})
names(reg_pooled_prepost_rem_het_highgain) <- c("Below median expected gain", "Above median expected gain")

# Deadline vs. no deadline
reg_pooled_prepost_DL_het_highgain <- lapply(groups, function(x) { 
  feols(fml = value ~ deadline + deadline:post, 
        data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1) %>% filter(high_gain == x), 
        fixef = c("date"), 
        cluster = "organization_uuid"
  )})
names(reg_pooled_prepost_DL_het_highgain) <- c("Below median expected gain", "Above median expected gain")

# Anticipated reminder vs. unanticipated reminder
reg_pooled_prepost_antrem_het_highgain <- lapply(groups, function(x) { 
  feols(fml = value ~ anticipated_reminder + anticipated_reminder:post, 
        data = fintech_fee_prepost %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% filter(high_gain == x), 
        fixef = c("date"), 
        cluster = "organization_uuid"
  )})
names(reg_pooled_prepost_antrem_het_highgain) <-  c("Below median expected gain", "Above median expected gain")

# # POOLED W/EXTRA SPLIT ------------------------------------------------------------
# #### Extra split 1: Anticipated Reminder w Daily ####
# # Effect of an anticipated reminder (conditional on having a deadline)
# reg_pooled_extrasplit_antreminder_wDL <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
#                                                     data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% filter(deadline == 1))
# 
# # Effect of an anticipated reminder (conditional on not having a deadline)
# reg_pooled_extrasplit_antreminder_noDL <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "anticipated_reminder",
#                                                      data = fintech_fee %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% filter(deadline == 0))
# 
# #### Extra split 2: Reminder w Daily ####
# # Effect of an  reminder (conditional on having a deadline)
# reg_pooled_extrasplit_reminder_wDL <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                  data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1) %>% filter(deadline == 1))
# # + conditional on getting a 2.75 fee.
# reg_pooled_extrasplit_reminder_wDL_2.75 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                       data = fintech_fee %>% 
#                                                         filter(reminder == 1 | no_reminder == 1) %>% 
#                                                         filter(deadline == 1) %>% 
#                                                         filter(fee_type == "2.75% offer"))
# # + conditional on getting a 3.00 fee.
# reg_pooled_extrasplit_reminder_wDL_3 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                    data = fintech_fee %>% 
#                                                      filter(reminder == 1 | no_reminder == 1) %>% 
#                                                      filter(deadline == 1) %>% 
#                                                      filter(fee_type == "3% offer"))
# # + conditional on getting email
# reg_pooled_extrasplit_reminder_wDL_openemail <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                            data = fintech_fee %>% 
#                                                              filter(reminder == 1 | no_reminder == 1) %>% 
#                                                              filter(deadline == 1) %>% 
#                                                              filter(open_email_before_reminder == 1))
# # + conditional on getting email and a 2.75 fee.
# reg_pooled_extrasplit_reminder_wDL_openemail_2.75 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                                 data = fintech_fee %>% 
#                                                                   filter(reminder == 1 | no_reminder == 1) %>% 
#                                                                   filter(deadline == 1) %>% 
#                                                                   filter(open_email_before_reminder == 1) %>% 
#                                                                   filter(fee_type == "2.75% offer"))
# # + conditional on getting email and a 3.00 fee.
# reg_pooled_extrasplit_reminder_wDL_openemail_3 <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                              data = fintech_fee %>% 
#                                                                filter(reminder == 1 | no_reminder == 1) %>% 
#                                                                filter(deadline == 1) %>% 
#                                                                filter(open_email_before_reminder == 1) %>% 
#                                                                filter(fee_type == "3% offer"))
# 
# # Effect of an  reminder (conditional on not having a deadline)
# reg_pooled_extrasplit_reminder_noDL <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "reminder",
#                                                   data = fintech_fee %>% filter(reminder == 1 | no_reminder == 1) %>% filter(deadline == 0))
# 
# #### Extra split 3: Deadline w Daily####
# # Effect of a deadline (conditional on having a reminder)
# reg_pooled_extrasplit_deadline_wREM <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
#                                                   data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1) %>% filter(reminder == 1))
# 
# # Effect of a deadline (conditional on not having a reminder)
# reg_pooled_extrasplit_deadline_noREM <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
#                                                    data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1) %>% filter(reminder == 0))
# 
# # Effect of a deadline (conditional on having an anticiapted reminder)
# reg_pooled_extrasplit_deadline_wANTREM <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
#                                                      data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1) %>% filter(anticipated_reminder == 1))
# 
# # Effect of a deadline (conditional on having an unanticiapted reminder)
# reg_pooled_extrasplit_deadline_wUNANTREM <- reg_fe_all(current_outcomes = day_cum_outcomes, special_model = "deadline",
#                                                        data = fintech_fee %>% filter(deadline == 1 | no_deadline == 1) %>% filter(unanticipated_reminder == 1))
# 
# #### Extra split 4: Anticipated Reminder w Pre/Post ####
# # Effect of an anticipated reminder (conditional on having a deadline)
# reg_pooled_prepost_extrasplit_antreminder_wDL <- lapply("value", reg_fe, special_model = "anticipated_reminder  + anticipated_reminder:post",
#                                                         data = fintech_fee_prepost %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% filter(deadline == 1),
#                                                         fe = "date")
# 
# # Effect of an anticipated reminder (conditional on not having a deadline)
# reg_pooled_prepost_extrasplit_antreminder_noDL <- lapply("value", reg_fe, special_model = "anticipated_reminder  + anticipated_reminder:post",
#                                                          data = fintech_fee_prepost %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% filter(deadline == 0),
#                                                          fe = "date")
# 
# #### Extra split 5: Reminder w Pre/Post ####
# # Effect of an  reminder (conditional on having a deadline)
# reg_pooled_prepost_extrasplit_reminder_wDL <- lapply("value", reg_fe, special_model = "anticipated_reminder  + anticipated_reminder:post",
#                                                      data = fintech_fee_prepost %>% filter(reminder == 1 | no_reminder == 1) %>% filter(deadline == 1),
#                                                      fe = "date")
# 
# # Effect of an  reminder (conditional on not having a deadline)
# reg_pooled_prepost_extrasplit_reminder_noDL <- lapply("value", reg_fe, special_model = "anticipated_reminder  + anticipated_reminder:post",
#                                                       data = fintech_fee_prepost %>% filter(reminder == 1 | no_reminder == 1) %>% filter(deadline == 0),
#                                                       fe = "date")
# 
# #### Extra split 6: Deadline w Pre/Post (reminder yes or no) ####
# # Effect of a deadline (conditional on having a reminder)
# reg_pooled_prepost_extrasplit_DL_wREM <- lapply("value", reg_fe, special_model = "deadline  + deadline:post",
#                                                 data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1) %>% filter(reminder == 1), 
#                                                 fe = "date")
# 
# # Effect of a deadline (conditional on not having a reminder)
# reg_pooled_prepost_extrasplit_DL_wREM <- lapply("value", reg_fe, special_model = "deadline  + deadline:post",
#                                                 data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1) %>% filter(reminder == 0), 
#                                                 fe = "date")
# 
# #### Extra split 7: Deadline w Pre/Post (type of reminder) ####
# # Effect of a deadline (conditional on having an anticipated reminder)
# reg_pooled_prepost_extrasplit_DL_wANTREM <- lapply("value", reg_fe, special_model = "deadline  + deadline:post",
#                                                    data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1) %>% filter(anticipated_reminder == 1), 
#                                                    fe = "date")
# 
# # Effect of a deadline (conditional on having an unaticipated reminder)
# reg_pooled_prepost_extrasplit_DL_wUNANTREM <- lapply("value", reg_fe, special_model = "deadline  + deadline:post",
#                                                      data = fintech_fee_prepost %>% filter(deadline == 1 | no_deadline == 1) %>% filter(anticipated_reminder == 0), 
#                                                      fe = "date")

#################################
##    (10): Save regressions.  ##
#################################
# (10.1): Rename all regressions.
dont_include <- paste(c("reg_pooled_antreminder", "reg_pooled_reminder", "reg_pooled_deadline",
                        "reg_pooled_het_antrem_", "reg_pooled_het_rem_", "reg_pooled_het_DL_",
                        "reg_pooled_het_commission", "reg_pooled_prepost_", 
                        "reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday", 
                        "reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday",
                        "reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday_fullsample",
                        "reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday_fullsample"),
                      collapse = "|")
rename_regs <- ls()[str_detect(ls(), "reg_rob_pooled|reg_cum_daily|reg_rob_daily|reg_pooled") & !str_detect(ls(), dont_include)]
for (reg in rename_regs) {
  print(reg)
  current_list <- eval(as.name(reg))
  names(current_list) <- clean_days
  assign(reg, current_list)
}

# (10.2): Export all regressions.
stuff_to_save <-  ls(.GlobalEnv)[grep("reg", ls())]
save_pres <- c("reg_cum_daily", "reg_cum_daily_fee", "reg_cum_daily_fee_fast_deadline", "reg_cum_daily_fee_independent_fast_deadline",
               "reg_cum_daily_pooled_accepted_offer_rem", "reg_cum_daily_pooled_accepted_offer_rem_openemail", 
               "reg_cum_daily_pooled_accepted_offer_unantrem", "reg_cum_daily_pooled_accepted_offer_unantrem_openemail",
               "reg_cum_daily_pooled_accepted_offer_dl", "reg_cum_daily_pooled_accepted_offer_dl_openemail", 
               "reg_cum_daily_pooled_accepted_offer_antrem", "reg_cum_daily_pooled_accepted_offer_antrem_openemail", 
               "reg_cum_daily_pooled_accepted_offer_fee", "reg_cum_daily_pooled_accepted_offer_fee_openemail", 
               "reg_cum_daily_het_accepted_offer_antrem_fee", "reg_cum_daily_het_accepted_offer_antrem_fee_openemail", 
               "reg_cum_daily_het_accepted_offer_rem_fee", "reg_cum_daily_het_accepted_offer_rem_fee_openemail", 
               "reg_cum_daily_het_accepted_offer_dl_fee", "reg_cum_daily_het_accepted_offer_dl_fee_openemail",
               "reg_cum_daily_het_accepted_offer_unantrem_fee", "reg_cum_daily_het_accepted_offer_unantrem_fee_openemail", 
               "reg_cum_daily_het_accepted_offer_dl_rem", 
               "reg_cum_daily_het_accepted_offer_dl_antrem",
               "reg_cum_daily_het_accepted_offer_dl_unantrem",
               "reg_cum_daily_het_accepted_offer_fsize_rem",
               "reg_cum_daily_het_accepted_offer_fsize_antrem", 
               "reg_cum_daily_het_accepted_offer_fsize_dl",
               "reg_cum_daily_het_accepted_offer_fsize_unantrem", 
               "reg_cum_daily_het_accepted_offer_expgain_rem",
               "reg_cum_daily_het_accepted_offer_expgain_antrem",
               "reg_cum_daily_het_accepted_offer_expgain_dl",
               "reg_cum_daily_het_accepted_offer_expgain_unantrem",
               # "reg_cum_daily_pooled_accepted_offer_rem_survey", 
               # "reg_cum_daily_pooled_accepted_offer_rem_survey_wt",
               # "reg_cum_daily_pooled_accepted_offer_dl_survey", 
               # "reg_cum_daily_pooled_accepted_offer_dl_survey_wt",
               # "reg_cum_daily_pooled_accepted_offer_antrem_survey",
               # "reg_cum_daily_pooled_accepted_offer_antrem_survey_wt",
               "reg_cum_daily_pooled_open_email_antrem",
               "reg_cum_daily_pooled_open_email_rem",
               "reg_cum_daily_pooled_open_email_dl",
               "reg_cum_daily_pooled_open_email_fee",
               "reg_cum_daily_pooled_open_email_unantrem",
               "reg_cum_daily_pooled_open_erem_antrem",
               "reg_cum_daily_pooled_open_erem_rem",
               "reg_cum_daily_pooled_open_erem_dl",
               "reg_cum_daily_pooled_open_erem_fee",
               "reg_cum_daily_pooled_open_erem_unantrem",
               "reg_cum_daily_het_open_email_dl_rem",
               "reg_cum_daily_het_open_email_dl_antrem",
               "reg_cum_daily_het_open_erem_dl_rem",
               "reg_cum_daily_het_open_erem_dl_antrem",
               "reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem",
               "reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday",
               "reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday",
               "reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem_firstday_fullsample",
               "reg_cum_daily_het_accepted_offer_dl_antrem_norem_nounantrem_firstday_fullsample",
               "reg_cum_daily_het_accepted_offer_dl_antrem_no_reminder")

all_regressions <- list()
for (i in 1:length(stuff_to_save)) {
  all_regressions[[i]] <- get(stuff_to_save[i])
}
names(all_regressions) <- stuff_to_save

regs_for_figs <- list()
for (i in 1:length(save_pres)) {
  regs_for_figs[[i]] <- get(save_pres[i])
}
names(regs_for_figs) <- save_pres

qs::qsave(all_regressions, file = here("proc", "regressions.qs"))
qs::qsave(regs_for_figs, file = here("proc", "regs_for_figs.qs"))

