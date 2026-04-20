#--------------------------------------------------------------------------------------------
# File name: 		      05d_survey_proc.R
# Creation date:      2022-01-03
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
#   - here("data", "SurveyCTO_downloads", "2021_03_15", "Encuesta oferta fintech_WIDE.csv")
#   - here("proc", "fintech_fee_light.csv")
#   - here("data", "SurveyRecodes", "recode_7.2_why_deadline_other.csv")
#   - here("data", "SurveyRecodes", "recode_8.1_why_activate_firstday.csv")
#   - here("data", "SurveyRecodes", "recode_9.1_why_activate_later_other.csv")
#   - here("data", "SurveyRecodes", "recode_10.3_why_reminder.csv")
#   - here("data", "SurveyRecodes", "recode_10.5_reminder_feel_cat_missing.csv")
#   - here("data", "SurveyRecodes", "recode_10.5_reminder_feel_cat.csv")
#   - here("data", "SurveyRecodes", "recode_11.1_offer_impact_cat_missing.csv")
#   - here("data", "SurveyRecodes", "recode_11.1_offer_impact_cat.csv")
# Files created:
#   - here("proc", "survey_all.csv")
#   - here("proc", "survey_other_tables.xlsx")
#   - here("proc", "survey_percent_sales.csv")
#   - here("proc", "survey_successful.csv")
# Purpose:
# 	- Process raw survey data, prepare variables for histograms and regressions.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(data.table)
library(lubridate)
library(assertthat)
library(readxl)
library(writexl)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "programs", "05c_barplot_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#conflicts_prefer(dplyr::select)
#########################################################

#############################################
##    (1): Import and filter survey data.  ##
#############################################
# (1.1): Import raw survey data and recoding dataset.
survey_raw <- fread(here("data", "SurveyCTO_downloads", "2021_03_15", "Encuesta oferta fintech_WIDE.csv")) %>% 
  filter(uuid != "0d7623ca-977f-11e9-8cec-62d790593222") %>% 
  as_tibble()

# (1.2): Keep entries from start of survey.
# Previous ones are tests
survey_proc <- survey_raw %>% 
  mutate(submission_date = str_replace(SubmissionDate, ",.*$", ""),
         submission_date = mdy(submission_date))  %>%  
  filter(submission_date >= ymd("2020-11-23"))

# (1.3): Filter out test surveys.
survey_proc %<>% filter(!(uuid %in% c("1", "2", "3", "4", "5", "6")))

# (1.4): Filter out manual duplicates.
survey_proc %<>% filter(!(KEY == "uuid:a241740a-dbcb-4f5f-b00f-afc28c0f38a6" | 
                            KEY == "uuid:c7e1be4d-5f99-4dca-ac3f-3c20fe751e8f" |
                            KEY == "uuid:2a03e341-15c6-4704-afbb-2ee08cff607a")) 

# (1.5): Remove surveys with missing uuid.
survey_proc %<>% 
  # rename(organization_uuid = uuid) %>% #edited below by Mohammad
  rename(organization_uuid = uuid) %>%
  filter(organization_uuid != "")

# (1.6): Filter out manual organization IDs.
# For this user, he thought it was a fraud call so didn't want to continue answering questions - remove that duplicate submission.
survey_proc %>% filter(organization_uuid == "9aa20444-5340-11ea-ab5b-8a387484c944") %>% select(text_audit, duration)
survey_proc %<>% filter(text_audit != "media\\TA_94d2f740-3fbf-453c-a228-7f49394f6de3.csv")
# One submission has NA for total duration so exclude that submission
survey_proc %>% filter(organization_uuid == "8f0b1bf0-081a-11e5-b9cb-26031b540ade") %>% select(text_audit, phone_call_duration)
survey_proc %<>% filter(text_audit != "media\\TA_301e22ab-0d6d-4c93-be38-b20eec858314.csv")
# For this user, the second submission has the information from both calls, so remove that submission
survey_proc %>% filter(organization_uuid == "bc69fc6c-afa8-11e9-a69f-ff0d072db56b") %>% select(text_audit, phone_call_duration)
survey_proc %<>% filter(text_audit != "media\\TA_145c881f-71cb-4844-9274-5564285030e9.csv")
# For this user, the second submission has the information from both calls, so take that submission
survey_proc %>% filter(organization_uuid == "17709fa0-db9f-11e8-9adb-e516f37847a1") %>% select(text_audit, phone_call_duration, submission_date)
survey_proc %<>% filter(text_audit != "media\\TA_ee420730-ebf8-41d2-9dca-f5f6258f0ba9.csv")
# For this user, the first submission wasn't completed.
survey_proc %>% filter(organization_uuid == "ed54f410-e6bb-11e7-bb31-344b23349644") %>% select(text_audit, phone_call_duration, submission_date)
survey_proc %<>% filter(text_audit != "media\\TA_2e3efd1c-61a3-4901-bc79-2571cb23437b.csv")
# Omit firm with no covariate data.
survey_proc %<>% filter(organization_uuid != "0d7623ca-977f-11e9-8cec-62d790593222")

# (1.7): Review survey completeness status.
survey_proc %>% tab(successful_txt, successful, isavailable, consent) %>% arrange(successful)
survey_proc %<>% mutate(successful_txt = ifelse(successful == 1, "Completa", successful_txt))
survey_proc %>% tab(successful_txt, successful, isavailable, consent) %>% arrange(successful)

# (1.8): Define complete and survey start variables.
survey_proc %<>% 
  mutate(survey_complete = case_when(successful == 1 ~ 1,
                                     successful %in% c(0, 2) ~ 0),
         survey_started = ifelse(!is.na(survey_complete), 1, 0))
survey_proc %>% tab(survey_complete, survey_started)

# (1.9): Drop non-started surveys for firms that started a survey.
survey_proc %<>%
  group_by(organization_uuid) %>% 
  mutate(firm_survey_started = max(survey_started)) %>% 
  ungroup()
survey_proc %>% tab(survey_started)  
survey_proc %<>% filter(survey_started == firm_survey_started)
unique(survey_proc$organization_uuid) %>% length()

# (1.10): Keep last survey for firms with more than 1 survey.
# This only affects 2 firms that started a survey.
# Check number of surveys per firm
survey_proc %>% 
  group_by(organization_uuid) %>% 
  mutate(num_svy = n()) %>% 
  ungroup() %>% 
  tab(num_svy)
# Keep last survey for each
survey_proc %<>%
  mutate(submission_datetime = str_replace(SubmissionDate, ".*,", ""), 
         submission_datetime = str_c(submission_date, submission_datetime)) %>% 
  group_by(organization_uuid) %>% 
  filter(max(submission_datetime) == submission_datetime) %>% 
  ungroup() %>% 
  select(-submission_datetime)
unique(survey_proc$organization_uuid) %>% length()

# (1.11): Re-define successful surveys: firms consenting to survey.
# ¿Tengo su permiso para hacer estas preguntas?   
survey_proc %>% tab(consent, isavailable) %>% arrange(consent, isavailable)
# Unsucessful surveys are those that did not consent to the survey or were not available
survey_proc %<>% mutate(successful = ifelse(consent == 1, 1, 0))
survey_proc %>% tab(successful)

# (1.12): Import administrative data.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, treat_description, 
         deadline, reminder, no_reminder, anticipated_reminder, unanticipated_reminder,
         commission_model, fee_type, fee_2.75, 
         open_email_reminder_date, clicked_email_reminder_link_date, accepted_offer_date)

# (1.13): Merge with fintech_fee to obtain treatment variables (some missing from survey).  
survey_proc %<>%
  select(-c(deadline, reminder, anticipated_reminder, unanticipated_reminder)) %>% 
  left_join(fintech_fee %>% 
              select(organization_uuid, treat_description,
                     deadline, reminder, no_reminder, anticipated_reminder, unanticipated_reminder,
                     commission_model, fee_type, fee_2.75), 
            by = "organization_uuid")

# (1.14): Remove 1 control observation.
survey_proc %>% tab(treat_description)
survey_proc %<>% filter(treat_description != "Control")

# (1.15): Print number of successful, unsuccessful and total surveys.
survey_proc %>% 
  filter(successful == 1) %>% 
  count() %>% 
  print_n("survey_n_success.tex",
          "Number of successful surveys")
survey_proc %>% 
  filter(successful != 1 | is.na(successful)) %>% 
  count() %>% 
  print_n("survey_n_no_success.tex",
          "Number of unsuccessful surveys")
survey_proc %>% 
  count() %>% 
  print_n("survey_n_total.tex",
          "Number of total firms called to take part in survey")

# (1.16): Save full (all firms we tried to survey) dataset.
write_csv_(survey_proc, here("proc", "survey_all.csv"))

# (1.17): Keep successful surveys.
survey_all <- survey_proc
survey_proc %<>% filter(successful == 1)


why_reminder_other_table <- survey_proc |>
  select(organization_uuid, why_reminder_other, anticipated_reminder, deadline) |>
  distinct() |>
  filter(!is.na(why_reminder_other) & why_reminder_other != "") |>
  mutate(English = c(
    "Because Fintech is interested in having customers take advantage of promotions",
    "Because they thought I hadn’t read it",
    "Because the promotion wasn’t addressed the first time",
    "It was an important promotion",
    "Because it’s important for us as a company",
    "To offer better service",
    "Because the registration hadn’t been completed",
    "Didn’t read the first one",
    "Because the form hadn’t been completed",
    "Doesn’t know",
    "To remind the customer of the promotion",
    "Because they hadn’t registered",
    "Because they hadn’t activated it",
    "There were cookies or an automatic response email",
    "Because they didn’t pay attention at first",
    "To activate the commission",
    "To verify that it was indeed the user",
    "To remind the customer of the offer",
    "As a strategy",
    "The offer was about to expire",
    "Because the process wasn’t completed",
    "The customer doesn’t trust promotions; they had to call support to clarify doubts"
  ))

why_deadline_other_table <- survey_proc |>
  select(organization_uuid, why_deadline_other, anticipated_reminder, deadline) |>
  distinct() |>
  filter(!is.na(why_deadline_other) & why_deadline_other != "") |>
  mutate(English = c(
    "To take advantage of promotion",
    "Because it wasn’t going to be for all users, only those who had the opportunity to register",
    "Because due to the pandemic there will be an economic change",
    "Because it is increasing again",
    "To have a database, I thought it wasn’t from the company",
    "Because it was a time-limited promotion",
    "As support due to the pandemic",
    "Due to the pandemic",
    "The offers have an expiration date",
    "Because it was a temporary commission",
    "Because it’s an offer",
    "To encourage the use of Fintech",
    "Limited-time special promotion",
    "For being a special promotion",
    "Because I thought it was going to improve",
    "As help due to the pandemic",
    "To encourage business sales",
    "For supporting a business during the pandemic",
    "Like any other promotion",
    "To respond to the survey",
    "Due to the situation",
    "Due to the pandemic situation",
    "I thought it was a pilot program",
    "Seasonal offer",
    "After the deadline, another fee would apply",
    "Support due to the pandemic",
    "Temporary promotion",
    "For special promotion and help during the pandemic",
    "To test Fintech",
    "To support businesses due to the pandemic",
    "For help due to the pandemic",
    "So that people take advantage of it",
    "It was support due to the pandemic"
  ))

why_activate_later_other_table <- survey_proc |>
  select(organization_uuid, why_activate_later_other, anticipated_reminder, deadline) |>
  distinct() |>
  filter(!is.na(why_activate_later_other) & why_activate_later_other != "") |>
  mutate(English = c("Went on a trip",
                     "Forgot it",
                     "Because they hadn't seen their email until that day",
                     "Opened the message in the early morning",
                     "Because they don't check their emails daily",
                     "Needed to think about it",
                     "Carefully read the promotion before accepting",
                     "Doesn't remember",
                     "Doesn't check their email daily",
                     "Went to spam",
                     "Was in spam",
                     "Doesn't use the terminal much",
                     "Saw the message only that day",
                     "Didn't think it was something good",
                     "Hadn't seen it",
                     "Was afraid it was spam",
                     "Had time until that day",
                     "That's when they saw the email",
                     "Saw the email on that date",
                     "Time",
                     "Saw the email at that moment"))



rm(survey_raw)

##########################################################
##    (2): Recode section 1 (general characteristics).  ##
##########################################################
# (2.1): Define variable lists.
# profits_vars <- c("monthlyprofits", "monthlyprofits_guess", "periodprofits", "periodprofits_ans", "periodprofits_when", "periodprofits_when_other")
# Question 1.9.  What share of your total pesos of sales did you make through the fintech in the past week? 
sales_vars <- c("value_transactions", "percent_sales")
# Question 1.10  What was your commission with the fintech the week before you received the offer?
# Question 1.11. We offered you a ${new_rate} commission valid until March 31. 
#                Was this ${new_rate} commission higher/lower/the same as the previous commission you had?
fee_comp_vars <- c("recall_old_fee", "compare_fee")
# Question 1.1.	How many employees work in your business, including yourself?
char_vars <- c(fee_comp_vars, sales_vars, "nb_employees")

# (2.2): Tab non-responses.
survey_proc %<>% 
  mutate(req_ans_q1.1 = ifelse(successful == 1, 1, 0),
         req_ans_q1.2 = ifelse(successful == 1, 1, 0),
         req_ans_q1.4 = ifelse(successful == 1, 1, 0),
         req_ans_q1.7 = ifelse(successful == 1, 1, 0),
         req_ans_q1.8 = ifelse(successful == 1, 1, 0),
         req_ans_q1.9 = ifelse(successful == 1 & value_transactions > 0 & !is.na(value_transactions), 1, 0),
         req_ans_q1.10 = ifelse(successful == 1, 1, 0),
         req_ans_q1.11 = ifelse(successful == 1, 1, 0),
         req_ans_q1.12 = ifelse(successful == 1, 1, 0))
tabna("nb_employees")
print_no_answer("nb_employees")
tabna(sales_vars) # 139 don't know value transactions; 15 don't know percent sales
tabna("recall_old_fee") # 130 businesses don't know their old fee.
tabna("amount_paid_fee") # 207 did not know answer, 86 refused to answer

# (2.3): Save number of don't knows.
print_dont_know("value_transactions")
print_no_answer("value_transactions")
print_dont_know("percent_sales")
print_no_answer("percent_sales")
print_dont_know("recall_old_fee")
print_no_answer("recall_old_fee")

# (2.4): Save number of firms that did not answer the question.
print_no_answer("nb_employees")

# (2.5): For the very few firms getting the income/time question, convert to monthly profits.
# 1.5.	Can you tell me your income after expenses for a different time period, such as yesterday, the last week, or the last year?
# Period: __ Day (yesterday) __ Week __ 2 Weeks __ Year __ Other (Specify __________)
survey_proc %<>%
  mutate(periodprofits = case_when(periodprofits_when == 1 ~ as.numeric(periodprofits)*365/12, 
                                   periodprofits_when == 2 ~ as.numeric(periodprofits)*52/12, 
                                   periodprofits_when == 3 ~ as.numeric(periodprofits)*26/12, 
                                   periodprofits_when == 4 ~ as.numeric(periodprofits)*1/12, )) %>% 
  select(-periodprofits_ans, -periodprofits_when, -periodprofits_when_other)

# (2.6): Recode monthly profits guess variable.
# 1.6.	It is important for us to have an idea of the income of your business. 
# Even though you cannot give us an exact number, please let us know the range of your income. 
# Was the income after expenses earned by your business in the past month…?
survey_proc %<>%
  mutate(monthlyprofits_guess_recode = case_when(monthlyprofits_guess == 1  ~  2500,
                                                 monthlyprofits_guess == 2  ~  7500, 
                                                 monthlyprofits_guess == 3  ~  12500,
                                                 monthlyprofits_guess == 4  ~  17500,
                                                 monthlyprofits_guess == 5  ~  22500,
                                                 monthlyprofits_guess == 6  ~  27500,
                                                 monthlyprofits_guess == 7  ~  32500,
                                                 monthlyprofits_guess == 8  ~  37500,
                                                 monthlyprofits_guess == 9  ~  42500,
                                                 monthlyprofits_guess == 10 ~  47500,
                                                 monthlyprofits_guess == 11 ~  55000))

# (2.7): Create new monthly profits/income variable
# Create new profits variable that has by default the values of monthlyprofits,
# as a second alternative periodprofits, and as a third alternative monthlyprofits_guess_recode
survey_proc %<>%
  mutate(monthly_profits = case_when(monthlyprofits >= 0 ~ monthlyprofits,
                                     monthlyprofits < 0 & !is.na(periodprofits) ~ periodprofits,
                                     monthlyprofits < 0 & is.na(periodprofits) & !is.na(monthlyprofits_guess_recode) ~ monthlyprofits_guess_recode,
                                     monthlyprofits < 0 & !is.na(monthlyprofits_guess) ~ monthlyprofits_guess, 
                                     !is.na(monthlyprofits) ~ monthlyprofits,
                                     TRUE ~ -888)) # All firms were asked question, if they have no answer they refused to reply

# (2.8): Save number of firms that did not answer the question.
print_dont_know("monthly_profits")
print_no_answer("monthly_profits")

# (2.9): Recode tech importance (3 firms with out of range values).
survey_proc %<>%
  mutate(tech_importance = case_when(tech_importance %in% c(4, 5) ~ -888,
                                     TRUE ~ tech_importance))

# (2.10): Save raw percent sale data before recoding.
survey_proc %>% 
  filter(successful == 1) %>% 
  select(organization_uuid, value_transactions, percent_sales) %>% 
  write_csv(here("proc", "survey_percent_sales.csv"))

# (2.5): Now that we've saved the number of don't knows for footnotes: convert all -777 and -888 answers to NA.
# This reduces need for additional processing when generating graphs.
# Better to be aware of this!
# -777 ~ "Don't know"
# -888 ~ "Refuses to answer",
# survey_proc %<>% mutate_at(vars(all_of(char_vars)), ~ifelse(. %in% c(-777, -888), NA, .))

# (2.11): Calculate difference in perceived old fee vs actual old fee.
survey_proc %<>% 
  # Get old fee from administrative data
  left_join(fintech_fee %>% 
              mutate(old_fee = ifelse(commission_model == "FIXED_RATE", 3.5, 3.75)) %>% 
              select(organization_uuid, old_fee),
            by = "organization_uuid") %>% 
  # Calculate difference between actual fee and recall
  mutate(fee_diff = case_when(!(recall_old_fee %in% c(-666, -777, -888)) ~ recall_old_fee - old_fee)) %>% 
  # Relocate variables
  relocate(old_fee, .after = recall_old_fee) %>% 
  relocate(fee_diff, .after = old_fee)
survey_proc %>% tab(fee_diff)

rm(char_vars, fee_comp_vars, sales_vars)

###################################################
##    (3): Recode section 2 (recall questions).  ##
###################################################

# Brief recap of experiment timeline:
# 1. Original offer was sent through an EMAIL and SMS message.
#    We record opening of emails but no openings of SMS messages.
#    We know firms opened messages if they accepted without opening the email.
# 2. Reminder was sent through EMAIL and SMS message.
#    We record opening of reminder email but no openings of reminder SMS messages.

# (3.1): Generate survey email opening, link clicking and offer acceptance variables.
# Survey accept offer and open email variables have missing values for some firms.
# Use administrative data instead.
survey_proc %<>%
  left_join(fintech_fee %>% 
              select(organization_uuid, contains("_date")),
            by = "organization_uuid") %>% 
  mutate(open_email_reminder_survey = ifelse(!is.na(open_email_reminder_date) & open_email_reminder_date <= submission_date, 1, 0),
         clicked_email_reminder_link_survey = ifelse(!is.na(clicked_email_reminder_link_date) & clicked_email_reminder_link_date <= submission_date, 1, 0),
         accepted_offer_survey = ifelse(!is.na(accepted_offer_date) & accepted_offer_date <= submission_date, 1, 0)) %>% 
  select(-opened_email, -opened_reminder, -open_email_reminder_date,
         -clicked_link, -clicked_reminder, -clicked_email_reminder_link_date,
         -accepted_offer, accepted_offer_date)

# (3.2): Recode email and sms recall questions.
# Why recode? If they don't know the answer to the question, then they don't recall receiving email/SMS
# Both questions were asked to all firms, even if the did not open the email
# Question 2.1.	Do you remember receiving an email about the offer? 
# Question 2.2.	You may have also received SMS text messages from Fintech recently. 
#               Do you remember receiving text messages from Fintech with an offer about your merchant fee?
# Convert all -777 (did not know answer to this question) to 0.
survey_proc %<>% mutate_at(vars(c("firstemail_recall", "recall_sms")), ~ifelse(. %in% c(-777), 0, .))

# (3.3): Tab non-responses.
survey_proc %<>% 
  mutate(
    req_ans_q2.1 = (successful == 1) %>% as.numeric(),
    req_ans_q2.2 = (successful == 1) %>% as.numeric(),
    req_ans_q2.3 = (req_ans_q2.1 == 1 & 
                      (recall_sms == 1 & !is.na(recall_sms))) %>% as.numeric(),
    req_ans_q2.4 = ((firstemail_recall == 1 & !is.na(firstemail_recall)) |
                      (recall_sms == 1 & !is.na(recall_sms))) %>% as.numeric(),
    req_ans_q2.5 = (click_recall < 1 & !is.na(click_recall) &
                      accepted_offer_survey == 1) %>% as.numeric())
tabna("firstemail_recall")
tabna("recall_sms")
tabna("read_sms")
tabna("click_recall")
tabna("accept_offer_recall")

# (3.4): Save number of non-responses.
print_no_answer("firstemail_recall")
print_no_answer("recall_sms")
print_dont_know("read_sms")
print_dont_know("click_recall")
print_dont_know("accept_offer_recall")
print_no_answer("accept_offer_recall")

###########################################################
##    (4): Recode section 3-5 (non-adoption questions).  ##
###########################################################
# (4.1): Tab other responses.
survey_other_responses <- survey_proc %>% 
  filter(successful == 1 &
           !(why_no_open_other == "" &
               why_no_click_other == "" & 
               why_no_accept_other == "")) %>% 
  select(why_no_open_other,
         why_no_click_other,
         why_no_accept_other) %>% 
  arrange(why_no_accept_other, why_no_click_other, why_no_open_other)

# (4.2): Import recoding mapping and define recoding parameters.
other_recode <- read_csv(here("data", "SurveyRecodes", "why_no_open_click_accept_other.csv"),
                         col_names = c("why_no_open_other", "why_no_click_other", "why_no_accept_other", "why_no_accept_other_alt"),
                         show_col_types = FALSE) %>% 
  select(-why_no_accept_other_alt)
responses <- tribble(~response, ~why_no_open_recode, ~why_no_click_recode, ~why_no_accept_recode, ~short_response_recode,
                     "Did not see email",                    1,  -888,  -888, "did_not_see_email", 
                     "Did not see where to activate offer",  -888,  1,  -888, "did_not_see_link",
                     "Forgot",                               2,  2,  1,  "distracted",
                     "Ran out of time",                      3,  3,  2,  "no_time",
                     "Thought it would take too much time",  4,  4,  3,  "too_much_time",
                     "Didn't consider important",            5,  5,  4,  "not_important",
                     "Thought it was scam or fake",          6,  6,  5,  "scam_fake",
                     "Wasn't sure if it would benefit",      7,  7,  6,  "no_benefit",
                     "Had already clicked on SMS link",      8,  -888,  -888,  "click_sms",
                     "Didn't fill out form correctly",       9,  8,  8,  "no_form",
                     "Does not remember receiving emails or messages", 10, 10, 10, "no_email_no_recall",
                     "Opened email but did not remember doing so", 11, 11, 11, "open_email_no_recall",
                     "Does not remember clicking on link", 12, 12, 12, "no_link_no_recall",
                     "Clicked on link but did not remember doing so", 13, 13, 13, "click_link_no_recall",
                     # "Does not recall clicking on link", 11, 11, 11, "no_recall_click", # previously 11
                     "Did not answer", -777, -777, -777, "no_answer",
                     "Other",          -666, -666, -666, "other")

# (4.3): Convert responses to numeric.

other_recode <- other_recode %>%
  var_to_num("why_no_open_other") %>%
  var_to_num("why_no_click_other") %>%
  var_to_num("why_no_accept_other")


# (4.4): Generate alternative version of other responses.
update_responses <- tibble(response = unlist(survey_other_responses, use.names = FALSE)[!unlist(survey_other_responses, use.names = FALSE) == ""],
                           why_no_open_recode = unlist(other_recode, use.names = FALSE)[!is.na(unlist(other_recode, use.names = FALSE))]) %>% 
  mutate(why_no_click_recode = why_no_open_recode,
         why_no_accept_recode = why_no_open_recode)
other_vars <- str_c(c("why_no_open", "why_no_click", "why_no_accept"), "_other")
survey_proc %<>% left_join(survey_proc %>%
                             var_to_num("why_no_open_other", update_responses) %>%
                             var_to_num("why_no_click_other", update_responses) %>%
                             var_to_num("why_no_accept_other", update_responses) %>% 
                             mutate_at(other_vars, ~ifelse(is.na(.), "", as.character(.))) %>% 
                             rename_at(other_vars, ~str_c(., "_alt")) %>% 
                             select(organization_uuid, all_of(str_c(other_vars, "_alt"))),
                           by = "organization_uuid")

# (4.5): Replace other responses.
survey_proc %<>%
  mutate(why_no_open_alt = paste(str_remove(why_no_open, fixed("-666")), why_no_open_other_alt),
         why_no_click_alt = paste(str_remove(why_no_click, fixed("-666")), why_no_click_other_alt),
         why_no_accept_alt = paste(str_remove(why_no_accept, fixed("-666")), why_no_accept_other_alt))

rm(survey_other_responses, other_recode, var_to_num, update_responses, other_vars, responses)

############################################################
##    (5): Recode section 6 (time to complete the form).  ##
############################################################
# (5.1): Recode offer effect time variable: convert to days (some responses are in weeks).
survey_proc %<>%
  mutate(offer_effect_time = ifelse(offer_time_format == 2, offer_effect_time * 7, offer_effect_time))

# (5.2): Tab non-responses.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 6: accepted offer and recalls accepting survey or clicking on link.
survey_proc %<>% 
  mutate(
    # This is required for sections 3-10 (but is redundant due to skip patterns)
    recalls_messages_filter = ((recall_sms == 1 & !is.na(recall_sms)) | 
                                 (firstemail_recall == 1 & !is.na(firstemail_recall))) %>% as.numeric(),
    req_ans_q6.1 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                      accepted_offer_survey == 1 & 
                      ((accept_offer_recall == 1 & !is.na(accept_offer_recall)) | 
                         (click_recall == 1 & !is.na(click_recall)))) %>% as.numeric(),
    req_ans_q6.12 = (req_ans_q6.1 & is.na(form_time_cat)) %>% as.numeric(),
    req_ans_q6.2 = (recalls_messages_filter == 1) %>% as.numeric(),
    req_ans_q6.22 = (req_ans_q6.2 & is.na(exp_form_time_cat)) %>% as.numeric(),
    req_ans_q6.3 = req_ans_q6.1,
    form_time_cat_alt = form_time_cat2,
    exp_form_time_cat_alt = exp_form_time_cat2)
tabna("form_time_cat_alt")
tabna("exp_form_time_cat_alt")
tabna("offer_effect_time")

# (5.5): Save number of non-responses.
print_dont_know("form_time_cat_alt")
print_no_answer("form_time_cat_alt")
print_dont_know("exp_form_time_cat_alt")
print_no_answer("exp_form_time_cat_alt")
print_dont_know("offer_effect_time")
print_no_answer("offer_effect_time")

########################################################
##    (6): Recode section 7 (why deadline question).  ##
########################################################
# (6.1): Import open question recodes.
recode_7.2 <- read_csv(here("data", "SurveyRecodes", "recode_7.2_why_deadline_other.csv"))

why_deadline_other_table_final <- recode_7.2 |>
  filter(why_deadline_alt == "Other") |>
  select(organization_uuid) |>
    left_join(why_deadline_other_table, by = "organization_uuid")

# (6.2): Send blank answers to missing (these are not -777 or -888).
survey_proc %<>% mutate(why_deadline = ifelse(why_deadline == "", NA, why_deadline))

# (6.3): Recode "other" responses in why deadline question.
survey_proc %<>% recode_other("why_deadline", recode_7.2)
survey_proc %>% tab(why_deadline)

# (6.4): Tab non-responses.
survey_proc %<>% 
  mutate(req_ans_q7.1 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                           deadline == 1) %>% as.numeric(),
         req_ans_q7.2 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                           deadline == 1 & noticed_deadline == 1 & !is.na(noticed_deadline)) %>% as.numeric())
tabna("noticed_deadline")
tabna("why_deadline")

# (6.5): Save number of non-responses.
print_no_answer("noticed_deadline")
print_other("why_deadline")
print_dont_know("why_deadline")
print_no_answer("why_deadline")

rm(recode_7.2)

############################################################
##    (7): Recode section 8 (why activate on first day).  ##
############################################################
# (7.1): Import open question recodes.
recode_8.1 <- read_csv(here("data", "SurveyRecodes", "recode_8.1_why_activate_firstday.csv"))

# (7.2): Send blank answers to missing (these are not -777 or -888).
survey_proc %<>% mutate(why_activate_firstday = ifelse(why_activate_firstday == "", NA, why_activate_firstday))

# (7.3): Recode "other" responses in why activate first day question.
survey_proc %<>% recode_other("why_activate_firstday", recode_8.1)
survey_proc %>% tab(why_activate_firstday)

# (7.4): Tab non-responses.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filters for section 8: deadline == 1 & accepted_offer_date == ymd("2020-09-29")
survey_proc %<>% 
  mutate(req_ans_q8.1 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                           deadline == 1 & accepted_offer_date == ymd("2020-09-29") & !is.na(accepted_offer_date)) %>% as.numeric())
tabna("why_activate_firstday")

# (7.5): Save number of non-responses.
print_other("why_activate_firstday")
print_dont_know("why_activate_firstday")

rm(recode_8.1)

#####################################################################
##    (8): Recode section 9 (why activate offer after first day).  ##
#####################################################################
# (8.1): Import open question recodes.
recode_9.1 <- read_csv(here("data", "SurveyRecodes", "recode_9.1_why_activate_later_other.csv"))

# (8.2): Recode "other" responses in why activate later question.
# 9.	[If merchant received a one-week deadline for the offer and activated after September 29]
# 9.1.	We sent you the emails and SMS to let you know about this offer on September 29, 
#       but we see that you filled the form on ${activation_date}. 
#       Why did you wait until ___ day(s) later? 

survey_proc %<>% recode_other("why_activate_later", recode_9.1)
survey_proc %>% tab(why_activate_later)

# (8.3): Send blank answers to missing (these are not -777 or -888).
survey_proc %<>% mutate(why_activate_later = ifelse(why_activate_later == "", NA, why_activate_later))

# (8.4): Tab non-responses.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 9: deadline == 1 & accepted_offer_date > ymd("2020-09-29")
survey_proc %<>% 
  mutate(req_ans_q9.1 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                           deadline == 1 & 
                           accepted_offer_date > ymd("2020-09-29") & 
                           !is.na(accepted_offer_date)) %>% as.numeric())
tabna("why_activate_later")
print_other("why_activate_later")
print_dont_know("why_activate_later")
print_no_answer("why_activate_later")

# (8.5): Replace one category with only one response.
# survey_proc %>% filter(req_ans_q9.1 == 1) %>% tab(why_activate_later)
# survey_proc %<>%
#   mutate(why_activate_later = str_replace(why_activate_later, "4", "-666"))
# survey_proc %>% filter(req_ans_q9.1 == 1) %>% tab(why_activate_later)

rm(recode_9.1)

######################################################
##    (9): Recode section 10 (reminder questions).  ##
######################################################
# (9.1): Import open question recodes.
recode_10.3_other <- read_csv(here("data", "SurveyRecodes", "recode_10.3_why_reminder.csv"))
recode_10.5_missing <- read_csv(here("data", "SurveyRecodes", "recode_10.5_reminder_feel_cat_missing.csv"))
recode_10.5_other <- read_csv(here("data", "SurveyRecodes", "recode_10.5_reminder_feel_cat.csv"))

why_reminder_other_table_final <- recode_10.3_other |>
  filter(why_reminder == -666) |>
  select(organization_uuid) |>
  left_join(why_reminder_other_table, by = "organization_uuid")

reminder_feel_cat_other_table <- recode_10.5_missing |>
  filter(!is.na(reminder_feel_cat_other)) |>
  select(organization_uuid, reminder_feel_cat_other)|>
  left_join(survey_proc |>
              select(organization_uuid, anticipated_reminder, deadline), by = "organization_uuid") |>
  filter(!is.na(anticipated_reminder)) |>
  mutate(English = c(
    "Good, but thinks the discount is very little",
    "In a hurry due to time",
    "Didn't read it",
    "That the company hadn't paid attention to the emails from customers who had already responded to the promotion",
    "Knew about the offer but didn’t take it because they didn’t want to",
    "Felt fine because it was only one reminder",
    "No problem",
    "Good",
    "Good, excellent",
    "A lot of distrust",
    "Couldn't read the reminder",
    "Doesn't remember seeing the reminder"
  ))

tables_list <- list(
  "Why Reminder Other" = why_reminder_other_table_final,
  "Why Deadline Other" = why_deadline_other_table_final,
  "Reminder Feel Other" = reminder_feel_cat_other_table,
  "Why Activate Later" = why_activate_later_other_table
)

# Write the tables to an Excel file with multiple sheets
write_xlsx(tables_list, path = here("proc", "survey_other_tables.xlsx"))

# (9.2): Input missing responses in reminder feel question.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Question 10.5 How did you feel about receiving a reminder? (multiple categories).
# For this question, we're using the same recoding mechanism for "other"responses, 
# but this is including new responses that were not previously in survey.
survey_proc %>% tab(reminder_feel_cat)
survey_proc %<>% recode_other("reminder_feel_cat", recode_10.5_missing)
survey_proc %>% tab(reminder_feel_cat)

# (9.3): Convert combination of -777 responses to only -777 in question 10.5.
survey_proc %<>% 
  mutate(reminder_feel_cat = ifelse(reminder_feel_cat %in% c("-666 -777", "3 -777"), "-777", reminder_feel_cat))
survey_proc %>% tab(reminder_feel_cat)

# (9.4): Send blank answers to missing in question 10.5(these are not -777 or -888).
survey_proc %<>% mutate(reminder_feel_cat = ifelse(reminder_feel_cat == "", NA, reminder_feel_cat))

# (9.5): Recode "other" responses in questions 10.3 and 10.5.
survey_proc %<>% recode_other("why_reminder", recode_10.3_other)
survey_proc %<>% recode_other("reminder_feel_cat", recode_10.5_other)

# (9.6): Tab non-responses for question 10.3.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.1: (no additional filters)
# Filter for question 10.2: reminder_recall == 1
# Filter for question 10.3: reminder_recall == 1
survey_proc %<>% 
  mutate(req_ans_q10.1 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                            (anticipated_reminder == 1 | unanticipated_reminder == 1) & 
                            (accepted_offer_date  >= ymd("2020-10-05") & !is.na(accepted_offer_date) | 
                               accepted_offer_survey == 0)) %>% as.numeric(),
         req_ans_q10.2 = (recalls_messages_filter == 1 & # this is redundant due to skip patterns
                            (anticipated_reminder == 1 | unanticipated_reminder == 1) & 
                            (accepted_offer_date  >= ymd("2020-10-05") & !is.na(accepted_offer_date) | 
                               accepted_offer_survey == 0) &
                            reminder_recall == 1 & !is.na(reminder_recall)) %>% as.numeric(),
         req_ans_q10.3 = req_ans_q10.2)
tabna("reminder_recall")
tabna("open_reminder_recall")
tabna("why_reminder")
print_no_answer("reminder_recall")
print_other("why_reminder")
print_dont_know("why_reminder")

# (9.7): Convert "Increased trust in the offer" in question 10.5 to "Other".
survey_proc %<>%
  mutate(reminder_feel_cat = str_replace(reminder_feel_cat, "5", "-666"))

# (9.8): Tab non-responses for question 10.5.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.5: reminder_recall == 1
survey_proc %<>% 
  mutate(req_ans_q10.5 = req_ans_q10.2)
tabna("reminder_feel_cat")
print_other("reminder_feel_cat")
print_dont_know("reminder_feel_cat")

# (9.9): Tab non-responses for question 10.4.
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Question 10.4.	Did the reminder change your perception of the offer’s value?
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.4: reminder_recall == 1
survey_proc %<>% 
  mutate(req_ans_q10.4 = req_ans_q10.2)
tabna("offer_value_change")
print_dont_know("offer_value_change")

# (9.10): Tab non-responses for question 10.6.
# Question 10.6 We see that you received an anticipated notice in the first email and SMS of the offer,
#               that you would receive a reminder about the offer a week after if you had not yet activated it.
#               Did you do anything between receiving the initial email and receiving the reminder so that you
#               would know whether to take up the offer when you received the reminder?
# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.6: anticipated_reminder == 1
survey_proc %<>% 
  mutate(req_ans_q10.6 = ((anticipated_reminder == 1 | unanticipated_reminder == 1) & 
                            (accepted_offer_date  >= ymd("2020-10-05") & !is.na(accepted_offer_date) | 
                               accepted_offer_survey == 0)) &
           anticipated_reminder == 1)
tabna("a_reminder_takeup")
print_dont_know("a_reminder_takeup")
print_no_answer("a_reminder_takeup")

rm(recode_10.3_other, recode_10.5_missing, recode_10.5_other)

#######################################################
##  (10): Recode section 11 (offer value question).  ##
#######################################################
# (10.1): Import open question recodes.
recode_11.1_missing <- read_csv(here("data", "SurveyRecodes", "recode_11.1_offer_impact_cat_missing.csv"))
recode_11.1_other <- read_csv(here("data", "SurveyRecodes", "recode_11.1_offer_impact_cat.csv"))

# (10.2): Input missing responses in offer impact question.
# Question 11.1 Is this offer working for your business? What impact has it had? (multiple categories).
# For this question, we're using the same recoding mechanism for "other"responses, 
# but this is including new responses that were not previously in survey.
survey_proc %>% tab(offer_impact_cat)
survey_proc %<>% recode_other("offer_impact_cat", recode_11.1_missing)
survey_proc %>% tab(offer_impact_cat)

# (10.3): Convert combination of -777 responses to only -777.
survey_proc %<>% 
  mutate(offer_impact_cat = ifelse(offer_impact_cat %in% c("-777 -888"), "-777", offer_impact_cat))

# (10.5): Send blank answers to missing (these are not -777 or -888).
survey_proc %<>% mutate(offer_impact_cat = ifelse(offer_impact_cat == "", NA, offer_impact_cat))

# (10.5): Recode "other" responses in reminder feel question.
survey_proc %<>% recode_other("offer_impact_cat", recode_11.1_other)
survey_proc %>% tab(offer_impact_cat)

# (10.6): Merge two categories: "Greater profits" and "Saved commission".
survey_proc %<>% 
  mutate(offer_impact_cat = str_replace(offer_impact_cat, "5", "2"))

# (10.7): Merge no impact categories.
survey_proc %>% tab(offer_impact_cat)
survey_proc %<>% 
  mutate(offer_impact_cat = str_replace_exact(offer_impact_cat, 7, 12) %>% 
           str_replace_exact(8, 12) %>% 
           str_replace_exact(11, 12))

# (10.8): Send "Good offer" and "Good support during the pandemic" to "Other".
survey_proc %<>% 
  mutate(offer_impact_cat = str_replace_exact(offer_impact_cat, 9, -666) %>% 
           str_replace_exact(10, -666))

# (10.9): Tab non-responses.
survey_proc %<>% 
  mutate(req_ans_q11.1 = (accepted_offer_survey == 1 & 
                            ((accept_offer_recall == 1 & !is.na(accept_offer_recall)) | 
                               (click_recall == 1 & !is.na(click_recall)))) %>% as.numeric())
tabna("offer_impact_cat")
print_other("offer_impact_cat")
print_dont_know("offer_impact_cat")
print_no_answer("offer_impact_cat")

rm(recode_11.1_missing, recode_11.1_other)

###################################################
##    (11): Recode section 12 (survey measures).  ##
###################################################
# (11.1): Rename survey measure variables.
survey_proc %<>%
  rename(trust_scale_1 = confidence_scale1,
         trust_scale_2 = confidence_scale2,
         memory_scale_1 = memory_scale1,
         memory_scale_2 = memory_scale2) 

# (11.2): Recode non-completes as NAs.
# Firms that did not complete the whole survey did not complete this section.
survey_proc %>% tab(survey_complete)
measure_vars <- c("trust_scale_1", "trust_scale_2", "reciprocity_scale", 
                  "procrastination_scale", "memory_scale_1", "memory_scale_2", 
                  "attention_scale")
survey_proc %<>% mutate_at(all_of(measure_vars), ~ifelse(survey_complete == 0, NA, .))

# (11.3): Tab non-responses.
survey_proc %<>% 
  mutate(req_ans_q12.11 = ifelse(successful == 1, 1, 0),
         req_ans_q12.12 = ifelse(successful == 1, 1, 0),
         req_ans_q12.13 = ifelse(successful == 1, 1, 0),
         req_ans_q12.14 = ifelse(successful == 1, 1, 0),
         req_ans_q12.15 = ifelse(successful == 1, 1, 0),
         req_ans_q12.16 = ifelse(successful == 1, 1, 0),
         req_ans_q12.17 = ifelse(successful == 1, 1, 0))
tabna(measure_vars)

# (11.4): Convert missing responses.
# There is ONE firm that did not answer one of these 7 questions.
# Send responses to NA.
# Why? We want complete answers to the WHOLE section. 
survey_proc %<>% mutate_at(all_of(measure_vars), ~ifelse(is.na(memory_scale_2) | memory_scale_2 == -888, NA, .))
tabna(measure_vars)

# (11.5): Print non-responses.
print_no_answer("trust_scale_1")

# (11.6): Generate pooled firm measure dummies.
binary_variable <- function(data, variable, min_scale) {
  new_name_binary <- str_c(variable, "_binary")
  data %>% 
    mutate(!!new_name_binary := ifelse(!is.na(eval(as.name(variable))) & (eval(as.name(variable)) >= min_scale), 1,  
                                       ifelse(!is.na(eval(as.name(variable))) & !(eval(as.name(variable)) >= min_scale), 0, NA)))
}
for (var in measure_vars) {
  survey_proc %<>% binary_variable(var, 4)
}
rm(measure_vars, var)

##########################################
##   (12): Define samples and weights.  ##
##########################################
# (12.1): Define survey weights.
response_rate_survey <- survey_all %>%
  left_join(fintech_fee %>% 
              select(organization_uuid, accepted_offer_date),
            by = "organization_uuid") %>% 
  mutate(survey_response = ifelse(organization_uuid %in% survey_proc$organization_uuid, 1, 0),
         accepted_offer_survey = ifelse(!is.na(accepted_offer_date) & accepted_offer_date <= submission_date, 1, 0)) %>% 
  group_by(accepted_offer_survey) %>% 
  summarise(response_rate = mean(survey_response, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(ipw_takeup = 1 / response_rate)

# (12.2): Merge with survey data.
survey_proc %<>% left_join(response_rate_survey, by = "accepted_offer_survey")

# (12.3): Confirm weights are correct.
survey_proc %>% filter(successful == 1) %>% count()
survey_proc %>% filter(successful == 1) %>% tab(ipw_takeup)

# (12.4): Save restricted (successful survey) dataset.
survey_proc %>% 
  filter(successful == 1) %>% 
  select(
    # Firm ID and treatment variables
    organization_uuid, successful, 
    reminder, deadline, anticipated_reminder, unanticipated_reminder,
    # Offer acceptance and email opening variables
    open_email_reminder_survey, clicked_email_reminder_link_survey, accepted_offer_survey,
    # Section 1: General characteristics
    owner_receives_emails, nb_employees, perform_review, perform_review_monthly, budget_costs, tech_importance,
    monthly_profits, value_transactions, percent_sales, recall_old_fee, compare_fee, fee_diff, amount_paid_fee, exp_savings,
    # Section 2: Recall questions
    firstemail_recall, recall_sms, read_sms, click_recall, accept_offer_recall,
    # Sections 3-5: Non-adoption questions
    why_no_open, why_no_open_alt, why_no_click, why_no_click_alt, why_no_accept, why_no_accept_alt,
    why_no_open_6, why_no_click_6, why_no_accept_5, why_no_open_other, why_no_click_other, why_no_accept_other,
    # Section 6: Time to complete the offer
    form_time_cat, form_time_cat_alt, exp_form_time_cat, exp_form_time_cat_alt, offer_effect_time,
    # Sections 7-9: Why deadline, why first day, why later
    why_deadline, why_activate_firstday, why_activate_later, why_activate_later_other,
    # Section 10: Reminder questions
    reminder_recall, open_reminder_recall, why_reminder, reminder_feel, reminder_feel_cat, offer_value_change, a_reminder_takeup, a_reminder_takeup_spec, 
    # Section 11: value
    offer_impact_cat, offer_impact_cat_spec,
    # Section 12: survey measures
    contains("binary"),
    # Other
    noticed_deadline, ipw_takeup, trust_scale_2,
    # Requires answer
    contains("req_ans")) %>% 
  write_csv_(here("proc", "survey_successful.csv"))


