#--------------------------------------------------------------------------------------------
# File name: 		      25b_number_calculations_survey.R
# Creation date:      2023-01-27
# Author:          		César Landín
# Files used:
#   - here("proc", "survey_successful.csv")
#   - here("proc", "fintech_fee_light.csv")
#   - here("results", "tables", "romano_p_antrem_unantrem.tex")
# Files created:
#   - here("results", "numbers", "survey_sample_size.tex")
#   - here("results", "numbers", "trust_unantrem_norem.tex")
#   - here("results", "numbers", "trust_unantrem_antrem.tex")
#   - here("results", "numbers", "trust_antrem_no_rem.tex")
#   - here("results", "numbers", "survey_pct_owner_receives_emails.tex")
#   - here("results", "numbers", "pres_survey_pct_owner_receives_emails.tex")
#   - here("results", "numbers", "survey_nb_employees_mean.tex")
#   - here("results", "numbers", "survey_nb_employees_median.tex")
#   - here("results", "numbers", "survey_nb_employees_sd.tex")
#   - here("results", "numbers", "survey_pct_budget_costs.tex")
#   - here("results", "numbers", "survey_value_transactions_mean.tex")
#   - here("results", "numbers", "survey_value_transactions_median.tex")
#   - here("results", "numbers", "survey_value_transactions_sd.tex")
#   - here("results", "numbers", "survey_percent_sales_mean.tex")
#   - here("results", "numbers", "survey_percent_sales_median.tex")
#   - here("results", "numbers", "survey_percent_sales_sd.tex")
#   - here("results", "numbers", "survey_fee_diff_mean.tex")
#   - here("results", "numbers", "survey_fee_diff_median.tex")
#   - here("results", "numbers", "survey_fee_diff_sd.tex")
#   - here("results", "numbers", "survey_pct_recall_old_fee_4.tex")
#   - here("results", "numbers", "survey_pct_fee_diff_0.tex")
#   - here("results", "numbers", "survey_pct_firstemail_recall.tex")
#   - here("results", "numbers", "survey_n_firstemail_recall.tex")
#   - here("results", "numbers", "survey_pct_recall_sms.tex")
#   - here("results", "numbers", "survey_n_recall_sms.tex")
#   - here("results", "numbers", "survey_pct_read_sms.tex")
#   - here("results", "numbers", "survey_n_read_sms.tex")
#   - here("results", "numbers", "survey_pct_click_recall.tex")
#   - here("results", "numbers", "survey_n_click_recall.tex")
#   - here("results", "numbers", "survey_pct_accept_offer_recall.tex")
#   - here("results", "numbers", "survey_n_accept_offer_recall.tex")
#   - here("results", "numbers", "survey_offer_effect_time_mean.tex")
#   - here("results", "numbers", "survey_offer_effect_time_median.tex")
#   - here("results", "numbers", "survey_offer_effect_time_sd.tex")
#   - here("results", "numbers", "survey_pct_noticed_deadline.tex")
#   - here("results", "numbers", "survey_n_noticed_deadline.tex")
#   - here("results", "numbers", "survey_pct_reminder_recall.tex")
#   - here("results", "numbers", "survey_n_reminder_recall.tex")
#   - here("results", "numbers", "survey_pct_open_reminder_recall.tex")
#   - here("results", "numbers", "survey_n_open_reminder_recall.tex")
#   - here("results", "numbers", "survey_pct_offer_value_change.tex")
#   - here("results", "numbers", "survey_n_offer_value_change.tex")
#   - here("results", "numbers", "survey_n_a_reminder_takeup.tex")
#   - here("results", "numbers", "survey_pct_a_reminder_takeup_0.tex")
#   - here("results", "numbers", "pres_survey_pct_a_reminder_takeup_0.tex")
#   - here("results", "numbers", "survey_pct_a_reminder_takeup_1.tex")
#   - here("results", "numbers", "survey_num_a_reminder_takeup_1.tex")
#   - here("results", "numbers", "survey_num_a_reminder_takeup_calculate.tex")
#   - here("results", "numbers", "p_value_het_trust_antrem_interaction_late.tex")
#   - here("results", "numbers", "p_value_het_overconfidence_antrem_interaction_late.tex")
#   - here("results", "numbers", "romano_p_value_het_trust_antrem_interaction_late.tex")
#   - here("results", "numbers", "romano_p_value_het_overconfidence_antrem_interaction_late.tex")
# Purpose:
# 	- Calculate numbers and statistics for survey data and export to tex files.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(fixest)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
#########################################################

#################################################################
##    (1): Define additional functions for number processing.  ##
#################################################################
# (1.1): Filter firms that were asked question.
keep_req_ans <- function(variable) {
  # Define required answer variable name
  req_ans_var <- var_mapping %>% filter(var_name == variable) %>% pull(var_question) 
  # Filter firms that should have been asked question
  proc_data <- survey_data %>% 
    filter(successful == 1) %>%             # Keep successful surveys
    filter(eval(as.name(req_ans_var)) == 1) # Keep firms that should have answered this question
  return(proc_data)
}

# (1.2): Keep valid answers (drop other, don't know, refuse to answer).
keep_valid_ans <- function(df, var) {
  df %>% 
    filter(!is.na(eval(as.name(var))) &
             !(eval(as.name(var))) %in% c(-666, -777, -888))
}

# (1.3): Define function to calculate and save stats.
calc_stats <- function(df, var_name, var_label = "", dig = 1) {
  # Mean
  df %>%
    num_mean(var_name, dig) %>%
    print_n(str_c("survey_", var_name, "_mean.tex"),
            note = ifelse(var_label == "", "", paste("Mean", var_label)))
  # Median
  df %>%
    num_median(var_name, dig) %>%
    print_n(str_c("survey_", var_name, "_median.tex"),
            note = ifelse(var_label == "", "", paste("Median", var_label)))
  # SD
  df %>%
    num_sd(var_name, dig) %>%
    print_n(str_c("survey_", var_name, "_sd.tex"),
            note = ifelse(var_label == "", "", paste("SD of", var_label)))
}


# (1.4): Get % of respondents answering "yes" to question.
tab_yes <- function(df, ...) {
  df %>% 
    tab(..., round = 3) %>% 
    filter_at(1, all_vars(. == 1)) %>% 
    pull(prop)
}

# (1.5: Filter firms that should've been asked this question, keep valid answers and print percentage answering yes.
print_pct_yes <- function(variable, note) {
  # Filter firms that were asked question
  proc_data <- keep_req_ans(variable)
  # Filter valid answers
  proc_data %<>% keep_valid_ans(variable)
  # Print percentage answering yes
  proc_data %>% 
    tab_yes(eval(as.name(variable))) %>% 
    print_pct(str_c("survey_pct_", variable, ".tex"),
              note)
}

# (1.6): Get number of firms asked a question.
print_n_ask <- function(variable, note) {
  # Filter firms that were asked question
  proc_data <- keep_req_ans(variable)
  # Print number firms that were asked question
  proc_data %>% 
    nrow() %>% 
    print_n(str_c("survey_n_", variable, ".tex"),
            note)
}

####################################################################
##    (2): Calculate section 1 (business characteristics) stats.  ##
####################################################################
# (2.1): Import successful survey data.
survey_data <- read_csv(here("proc", "survey_successful.csv"))
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, no_reminder, accepted_offer_late) 
survey_data <- survey_data %>%
  left_join(fintech_fee, by = "organization_uuid")
# Get survey sample size
nrow(survey_data) %>% 
  print_n("survey_sample_size.tex", "Survey sample size")

# Get sample size for those who answered trust related questions for (anticipated_reminder and unanticipated_reminder groups)
nrow(survey_data %>%
       filter(!is.na(trust_scale_1_binary)) %>%
       filter(unanticipated_reminder == 1 | no_reminder == 1)) %>%
  print_n("trust_unantrem_norem.tex", "Trust related number for unannounced reminder (and no reminder)")
nrow(survey_data %>%
       filter(!is.na(trust_scale_1_binary)) %>%
       filter(unanticipated_reminder == 1 | anticipated_reminder == 1)) %>%
  print_n("trust_unantrem_antrem.tex", "Trust related number for unannounced reminder (and announced reminder)")
# Get sample size for those who answered trust related questions for (unanticipated_reminder and no_reminder groups)
nrow(survey_data %>%
       filter(!is.na(trust_scale_1_binary)) %>%
       filter(anticipated_reminder == 1 | no_reminder == 1)) %>%
  print_n("trust_antrem_no_rem.tex", "Trust related number for announced reminder (and no reminder)")


# (2.2): Calculate percentage of firms in which the owner is the one receiving the email.
survey_data %>% 
  mutate(owner_receives_emails = ifelse(is.na(owner_receives_emails), 0, 1)) %>% 
  tab_yes(owner_receives_emails) %>%
  print_pct("survey_pct_owner_receives_emails.tex",
            "Percentage firms remember receiving an email about the offer")

survey_data %>% 
  mutate(owner_receives_emails = ifelse(is.na(owner_receives_emails), 0, 1)) %>% 
  tab_yes(owner_receives_emails) %>%
  print_pct("pres_survey_pct_owner_receives_emails.tex",
            "Percentage firms remember receiving an email about the offer",
            dig = 0)

# (2.3): Save stats for Question 1.1.	How many employees work in your business, including yourself?
survey_data %>% 
  keep_valid_ans("nb_employees") %>% 
  calc_stats("nb_employees", "number of employees")

# (2.4): Save stats for Question 1.3.	Have you made a budget of what costs facing your business are likely to be for this year that just started?
survey_data %>% 
  keep_valid_ans("budget_costs") %>% 
  tab_yes(budget_costs) %>% 
  print_pct("survey_pct_budget_costs.tex",
            "Percentage of firms that have made a budget of the costs their firms are facing")

# (2.5): Save stats for Question 1.8. What is the approximate value of all transactions you have made through iZettle in the past week?
survey_data %>% 
  keep_valid_ans("value_transactions") %>% 
  calc_stats("value_transactions", "approximate value of transactions through FinTech")

# (2.6): Save stats for Question 1.9: What share of your total pesos of sales did you make through iZettle in the past week? #
survey_data %>% 
  keep_valid_ans("percent_sales") %>% 
  calc_stats("percent_sales", "share of sales made through FinTech")

# (2.7): Save stats for Question 1.10. What was your commission with iZettle the week before you received the offer? 
survey_data %>% 
  keep_valid_ans("fee_diff") %>% 
  keep_valid_ans("recall_old_fee") %>% 
  calc_stats("fee_diff", "difference in perceived fee - pre-treatment actual fee")

# (2.8): Question 1.10. What was your commission with iZettle the week before you received the offer? Calculate percentage of firms report fee is 4%.
keep_req_ans("recall_old_fee") %>% 
  keep_valid_ans("recall_old_fee") %>% 
  mutate(recall_old_fee_4 = (recall_old_fee == 4) %>% as.numeric()) %>% 
  tab_yes(recall_old_fee_4) %>% 
  print_pct("survey_pct_recall_old_fee_4.tex",
            "Percentage of firms report their previous fee is 4%")

# (2.9): Question 1.10. What was your commission with iZettle the week before you received the offer? Calculate percentage of firms get their current fee right.
survey_data %>% 
  keep_valid_ans("fee_diff") %>% 
  keep_valid_ans("recall_old_fee") %>% 
  mutate(fee_diff_0  = (fee_diff == 0) %>% as.numeric()) %>% 
  tab_yes(fee_diff_0) %>% 
  print_pct("survey_pct_fee_diff_0.tex",
            "Percentage of firms get their current fee right")

#######################################################
##    (3): Numbers for section 2 (email questions).  ##
#######################################################
# (3.1): Tab Question 2.1. Do you remember receiving an email about the offer? 
# Question asked to all firms
print_pct_yes("firstemail_recall",
              "Percentage firms remember receiving an email about the offer")
print_n_ask("firstemail_recall",
            "Number of firms asked if they remember receiving an email about the offer")
# Check extent of non response
tabna("firstemail_recall", survey_data)

# (3.2): Tab Question 2.2. You may have also received SMS text messages from iZettle recently. 
#        Do you remember receiving text messages from iZettle with an offer about your merchant fee?
# Question asked to all firms
print_pct_yes("recall_sms",
              "Percentage firms remember receiving SMS messages with the offer")
print_n_ask("recall_sms",
            "Number of firms asked if they remember receiving SMS messages with the offer")
# Check extent of non response
tabna("recall_sms", survey_data)

# (3.3): Tab Question 2.3. Did you read the text messages that you received from iZettle?
# Filter for question 2.3: recall_sms == 1
keep_req_ans("read_sms") %>% 
  tab_yes(read_sms) %>%                  # This includes -777 in denominator
  print_pct("survey_pct_read_sms.tex",
            "Percentage firms report reading SMS messages with the offer")
print_n_ask("read_sms",
            "Number of firms asked if they read the SMS messages with the offer")
# Check extent of non response
tabna("read_sms", survey_data)

# (3.4): Tab Question 2.4. Did you click on the offer you received by email or SMS?
# Filter for question 2.3: (firstemail_recall == 1 | recall_sms == 1)
print_pct_yes("click_recall",
            "Percentage firms report clicked on the offer sent by email or SMS")
print_n_ask("click_recall",
            "Number of firms asked if they clicked on the offer sent by email or SMS")
# Check extent of non response
tabna("click_recall", survey_data)

# (3.5): Tab Question 2.5. Even though you may not recall having clicked on the offer, 
#        we see that you did activate the lower commission. 
#        Do you remember filling out the form to activate the offer? 
# Filter for question 2.5: (click_recall < 1 & accepted_offer == 1)
keep_req_ans("accept_offer_recall") %>% 
  tab_yes(accept_offer_recall) %>%                  # This includes -777 in denominator
  print_pct("survey_pct_accept_offer_recall.tex",
            "Percentage firms that don't recall clicking on offer that remember filling out form to activate offer")
print_n_ask("accept_offer_recall",
            "Number of firms that don't recall clicking on offer asked if they remember filling out form to activate offer")
# Check extent of non response
tabna("accept_offer_recall", survey_data)

#################################################################
##    (4): Numbers for section 6 (time to complete the form).  ##
#################################################################
# (4.1): Save stats for Question 6.3.	When did you expect to see your commission change active?
keep_req_ans("offer_effect_time") %>%
  keep_valid_ans("offer_effect_time") %>% 
  calc_stats("offer_effect_time", "expected days it would change for commission to change active")

################################################
##    (5): Numbers for section 7 (deadline).  ##
################################################
# (5.1): Tab Question 7.1. Did you notice that your offer had a deadline? 
# Filter for sections 3-10: (recall_sms == 1 | firstemail_recall == 1), redundant due to skip patterns
# Filter for section 7: deadline == 1
print_pct_yes("noticed_deadline",
              "Percentage firms noticed offer had a deadline")
print_n_ask("noticed_deadline",
            "Number of firms asked if they noticed offer had a deadline")
# Check extent of non response
tabna("noticed_deadline", survey_data)

##################################################
##    (6): Numbers for section 10 (reminders).  ##
##################################################
# (6.1): Tab Question 10.1.	Our records show that you received a reminder email and reminder SMS text messages. 
#        Do you remember receiving a reminder email or reminder SMS message about the offer?
# Filter for sections 3-10: (recall_sms == 1 | firstemail_recall == 1), redundant due to skip patterns
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.1: (no additional filters)
print_pct_yes("reminder_recall",
              "Percentage firms remember receiving the reminder")
print_n_ask("reminder_recall",
            "Number of firms asked if they remember receiving the reminder")
# Check extent of non response
tabna("reminder_recall", survey_data)

# (6.2): Tab Question 10.2.	Do you remember reading the reminder? 
# Filter for sections 3-10: (recall_sms == 1 | firstemail_recall == 1), redundant due to skip patterns
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.2: reminder_recall == 1
print_pct_yes("open_reminder_recall",
              "Percentage firms remember reading the reminder")
print_n_ask("open_reminder_recall",
            "Number of firms asked if they remember reading the reminder")
# Check extent of non response
tabna("open_reminder_recall", survey_data)

# (6.3): Tab Question 10.5.	Did the reminder change your perception of the offer’s value? 
# Filter for sections 3-10: (recall_sms == 1 | firstemail_recall == 1), redundant due to skip patterns
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.5: reminder_recall == 1
print_pct_yes("offer_value_change",
              "Percentage firms for which the reminder changed their perception of the offer's value")
print_n_ask("offer_value_change",
            "Number of firms asked the reminder changed their perception of the offer's value")
# Check extent of non response
tabna("offer_value_change", survey_data)

# (6.4): Analyze answers to Question 10.6. (do anything between email and reminder).
# 10.6.	We see that you received an anticipated notice in the first email and SMS of the offer, 
#      that you would receive a reminder about the offer a week after if you had not yet activated it. 
#      Did you of anything between receiving the initial email and receiving the reminder so that you 
#      would know whether to take up the offer when you received the reminder?
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.6: anticipated_reminder == 1
print_n_ask("a_reminder_takeup",
            "Number of firms asked whether they did anything between receivint the initial email and reminder")
# Check extent of non response
tabna("a_reminder_takeup", survey_data)

# Import survey data.
data_a_reminder_takeup <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, a_reminder_takeup, a_reminder_takeup_spec, req_ans_q10.6) %>% 
  filter(req_ans_q10.6 == 1)
# Calculate numbers
a_reminder_takeup_pct <- data_a_reminder_takeup %>% 
  filter(!is.na(a_reminder_takeup)) %>% 
  mutate(a_reminder_takeup = ifelse(a_reminder_takeup == -777, 0, a_reminder_takeup)) %>% 
  tab(a_reminder_takeup, round = 3)
a_reminder_takeup_pct %>% 
  filter(a_reminder_takeup == 0) %>% 
  pull(prop) %>% 
  print_pct("survey_pct_a_reminder_takeup_0.tex",
            "Percentage of firms reporting that they have did NOT do anything between receiving the initial email and receiving the anticipated reminder")
a_reminder_takeup_pct %>% 
  filter(a_reminder_takeup == 0) %>% 
  pull(prop) %>% 
  print_pct("pres_survey_pct_a_reminder_takeup_0.tex",
            "Percentage of firms reporting that they have did NOT do anything between receiving the initial email and receiving the anticipated reminder",
            dig = 0)
a_reminder_takeup_pct %>% 
  filter(a_reminder_takeup == 1) %>% 
  pull(prop) %>% 
  print_pct("survey_pct_a_reminder_takeup_1.tex",
            "Percentage of firms reporting that they have DID do anything between receiving the initial email and receiving the anticipated reminder")
a_reminder_takeup_pct %>% 
  filter(a_reminder_takeup == 1) %>% 
  pull(N) %>% 
  print_n("survey_num_a_reminder_takeup_1.tex",
          "Number of firms reporting that they have DID do anything between receiving the initial email and receiving the anticipated reminder")
# Tab responses
data_a_reminder_takeup %>% filter(!is.na(a_reminder_takeup_spec)) %>% pull(a_reminder_takeup_spec)
# TWO firms took action:
# 1. "Marco para saber como seria la comision y hablo con otro personal"
# 2. "Hizo el calculo para saberbsi aprovechar la promo" 
2 %>%
  print_n("survey_num_a_reminder_takeup_calculate.tex",
          "Number of firms reporting that they calculated whether they should accept the offer between receiving the initial email and receiving the anticipated reminder")

############################################
##    (1): Previous number calculations.  ##
############################################
# # (1.5): Calculate percentage of firms that did not adopt because employee did not open email / click link / fill form.
# # Only one respondent.
# pct_no_adopt_employee <- survey %>% 
#   filter(why_no_click_other == "Porque su asistente no lo lleno") %>% 
#   pull(ipw_takeup) /
#   survey %>% 
#   filter(accepted_offer_late == 0) %>% 
#   pull(ipw_takeup) %>% 
#   sum(na.rm = TRUE)
# pct_no_adopt_employee %>% 
#   print_pct("pct_no_adopt_employee.tex")
# # (1.8): Calculate percentage of users that believe deadline exists because of limited opportunities.
# why_deadline_other_recode <- read_csv(here("data", "SurveyRecodes", "why_deadline_other_recode.csv"))
# survey %<>%
#   left_join(why_deadline_other_recode, by = "why_deadline_other")
# survey %>% 
#   filter(why_deadline_other_flag == 1) %>% 
#   tab(why_deadline_other)
# # One respondent answered "because it wasn't going to be for all users, only for those who had a chance to sign up"
# Final sample: users with a deadline who recall receiving the first email and/or the first email message,
# and who gave an answer to the question "7.2.	Why do you think the offer had a deadline?" (omitting "did not know" and "did not answer").
# Using late acceptance survey weights.
# # (1.5): Calculate percentage of firms that did not accept offer because they thought the ad was fake.
# pct_fake <- (survey %>% filter(why_no_open_6 == 1 | why_no_click_6 == 1 | why_no_accept_5 == 1) %>% pull(ipw_takeup) %>% sum()) /
#   (survey %>% filter(accepted_offer_ontime == 0) %>% pull(ipw_takeup) %>% sum())
# pct_fake %>% 
#   print_pct(here("results", "numbers", "pct_fake_ontime.tex"))


## Number calculation for Heterogeneity table interaction and p-value (model and romano) 
reg_het_trust_antrem_late <- feols(accepted_offer_late ~ anticipated_reminder*trust_scale_1_binary, 
                                   data = survey_data |>
                                     filter(anticipated_reminder == 1 | unanticipated_reminder == 1),
                                   se = "hetero")

reg_het_trust_antrem_late$coeftable[16] %>%
  print_n("p_value_het_trust_antrem_interaction_late.tex", "P-value for trust survey measure interacted with announced reminder for late take-up (Heterogeneous Effects of Announced Reminder by GSS Measures)", dig = 3)

reg_het_overconfidence_antrem_late <- feols(accepted_offer_late ~ anticipated_reminder*memory_scale_2_binary, 
                                   data = survey_data |>
                                     filter(anticipated_reminder == 1 | unanticipated_reminder == 1),
                                   se = "hetero")

reg_het_overconfidence_antrem_late$coeftable[16] %>%
  print_n("p_value_het_overconfidence_antrem_interaction_late.tex", "P-value for Overconfidence survey measure interacted with announced reminder for late take-up (Heterogeneous Effects of Announced Reminder by GSS Measures)", dig = 3)



antrem_unantrem_romano <- readLines(here("results", "tables", "romano_p_antrem_unantrem.tex"))
rw_pvalues <- str_trim(antrem_unantrem_romano [6:11]) %>% 
  str_extract_all("\\d*\\.\\d+") %>% 
  sapply(`[`, 3) %>%  # Extract the third match from each row
  as.numeric()

rw_pvalues[c(1)] %>%
  print_n("romano_p_value_het_trust_antrem_interaction_late.tex", "Romano-Wolf P-value for trust survey measure interacted with announced reminder for late take-up (Heterogeneous Effects of Announced Reminder by GSS Measures)", dig = 3)

rw_pvalues[c(5)] %>%
  print_n("romano_p_value_het_overconfidence_antrem_interaction_late.tex", "Romano-Wolf P-value for Overconfidence survey measure interacted with announced reminder for late take-up (Heterogeneous Effects of Announced Reminder by GSS Measures)", dig = 3)
