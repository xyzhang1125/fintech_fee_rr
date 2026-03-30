#--------------------------------------------------------------------------------------------
# File name: 		      04a_cleanresults.R
# Creation date:      2021-01-01
# Author:          		Noah Forougi & César Landín
# Files used:
#   - here("proc", "08_randomization.csv")
#   - here("data", "Sent_20201202", "daily", "daily.csv")
#   - here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx")
#   - here("data", "Sent_20210409", "42_samplecheck (1).xlsx")
#   - here("proc", "temp", "baseline_sales.Rds"))
#   - here("proc", "benefits_data_all.rds")
#   - here("proc", "monthly.rds")
#   - here("data", "SMS_data_sent20201029", "Delivered Extra SMS UUID 10AM 29 sept.xlsx")
#   - here("data", "SMS_data_sent20201029", "Delivered Extra SMS UUID 1005AM 29 sept.xlsx")
# Files created:
#   - here("proc", "monthly_panel.rds")
#   - here("proc", "fintech_fee.csv")
#   - here("proc", "fintech_fee_light.csv")
# Purpose:
# 	- Clean the data that fintech sent with outcomes from the fee experiment. 
#     Create outcomes for regressions.
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
conflicted::conflicts_prefer(dplyr::last)
conflicted::conflicts_prefer(dplyr::filter)
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "myfunctions.R"), 
       encoding = "UTF-8")
#########################################################

#################################################
##    (1): Read in data and define functions.  ##
#################################################
# (1.1): Read randomization data.
randomization <- read_csv(here("proc", "08_randomization.csv"), show_col_types = FALSE)

# (1.2): Import data on experimental sample users.
daily_first <- fread(here("data", "Sent_20201202", "daily", "daily.csv"))
fintech_users <- unique(daily_first$organization_uuid)

# (1.3): Import main results.
results <- here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx") %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  purrr::map(read_excel, path = here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx"))

# (1.4): Read in new results.
fintech_samplecheck <- read_excel(here("data", "Sent_20210409", "42_samplecheck (1).xlsx"))

# (1.5): Import sales data to construct heterogeneity variables.
baseline_sales <- readRDS(here("proc", "temp", "baseline_sales.Rds"))
benefits <- readRDS(here("proc", "benefits_data_all.rds")) %>% select(organization_uuid, fee_reduction)

# (1.6): Get transaction data to construct Covid-19 shock variable.
monthly_panel <- readRDS(here("proc", "monthly.rds"))

# (1.7): Import text message data.
sms_10am  <- read_excel(here("data", "SMS_data_sent20201029", "Delivered Extra SMS UUID 10AM 29 sept.xlsx"))
sms_1005am  <- read_excel(here("data", "SMS_data_sent20201029", "Delivered Extra SMS UUID 1005AM 29 sept.xlsx"))

####################################
##    (2): Process main results.  ##
####################################
# (2.1): Create monthly panel dataset that only has firms in experiment.
monthly_panel %<>% filter(organization_uuid %in% randomization$organization_uuid)
write_rds(monthly_panel, here("proc", "monthly_panel.rds"))

# (2.2): Split up excel workbook.
lower_fee <- results$`solicitud baja`
open_email <- results$`open - 1er envío`
open_reminder <- results$`open - Reminder`
rm(results)

# (2.3): Get lower fee from most recent dataset (sent on 2021-04-09).
lower_fee <- fintech_samplecheck %>%
  transmute(organization_uuid, 
            takeup_date = ymd(str_sub(request_date, start = 1 , end = 10)))
# Notice that there is a typo in the sample check data that fintech sent us
lower_fee %<>% filter(organization_uuid != "4d9a7f00-f8b1-11e7-aba3-c84545c3af5")

# (2.4): Clean open email and reminder variables.
open_email %<>% 
  lowercase_names() %>%
  transmute(organization_uuid,
            email_send_date = ymd(str_sub(send_date, end = -10)),
            open_email_date  =  ymd(str_sub(open_date, end = -10)), 
            open_email_count =  open_count, 
            email_linkcount =  clicked_links_count,
            click_email_link_date = as_date(formulario)) # Formulario is what matters: there are other links that are not the form (e.g. terms & conditions)
open_reminder %<>% 
  lowercase_names() %>%
  transmute(organization_uuid, 
            reminder_send_date = ymd(str_sub(send_date, end = -10)), 
            open_reminder_date  = ymd(str_sub(open_date, end = -10)), 
            open_reminder_count = open_count,
            reminder_linkcount = clicked_links_count,
            click_reminder_link_date = as_date(formulario))

# (2.5): Join datasets.
randomization %<>% 
  left_join(lower_fee, by = "organization_uuid") %>%
  left_join(open_email, by = "organization_uuid") %>%
  left_join(open_reminder, by = "organization_uuid")
randomization %>% 
  summarise(takeup_rate = sum(!is.na(takeup_date))/nrow(.)) # 25.5% took the lower rate 

# (2.6): Create main outcomes.
randomization %<>% 
  mutate(
  # Accepted offer
    accepted_offer_date = takeup_date,
    accepted_offer_firstday = ifelse(!is.na(takeup_date) & takeup_date == ymd("2020-09-29"), 1, 0),   # Accept lower fee offer on day offer was sent
    accepted_offer_ontime = if_else(!is.na(takeup_date) & takeup_date <= ymd("2020-10-06"), 1, 0, 0), # Accept lower fee offer by deadline
    accepted_offer_late =  if_else(!is.na(takeup_date), 1, 0, 0),                                     # Accept lower fee offer at all
    # Opened email
    open_email_firstday = if_else(!is.na(open_email_date) & open_email_date == ymd("2020-09-29"), 1, 0, 0), # Opened email on day first email was sent
    open_email_before_reminder = if_else(!is.na(open_email_date) & open_email_date <= ymd("2020-10-04"), 1, 0),
    open_email_ontime = if_else(!is.na(open_email_date) & open_email_date <= ymd("2020-10-06"), 1, 0, 0),   # Opened email by deadline
    open_email_late = if_else(!is.na(open_email_date), 1, 0, 0),                                           # Opened email at all
    # Opened reminder
    open_reminder_ontime = if_else(!is.na(open_reminder_date) & open_reminder_date <= ymd("2020-10-06"), 1, 0, 0), # Opened reminder by deadline
    open_reminder_late = if_else(!is.na(open_reminder_date), 1, 0, 0),                                           # Opened reminder at all
    # Date opened reminder or email
    open_email_reminder_date = case_when(!is.na(open_email_date) & !is.na(open_reminder_date) & open_email_date < open_reminder_date ~ open_email_date,
                                         !is.na(open_email_date) & !is.na(open_reminder_date) & open_email_date > open_reminder_date ~ open_reminder_date,
                                         is.na(open_email_date) & !is.na(open_reminder_date) ~ open_reminder_date,
                                         !is.na(open_email_date) & is.na(open_reminder_date) ~ open_email_date),
    # Opened reminder or email
    open_email_reminder_late = if_else(!is.na(open_email_reminder_date), 1, 0, 0))
# Sanity check
assert_that(randomization %>% filter(accepted_offer_late == 1) %>% nrow(.) == length(lower_fee$organization_uuid))
assert_that(randomization %>% filter(open_email_late == 1) %>% nrow(.) == sum(!is.na(open_email$open_email_date)))

# (2.7): Create clicking link outcomes.
randomization %<>%
  mutate(clicked_email_reminder_link_date = case_when(!is.na(click_email_link_date) & !is.na(click_reminder_link_date) ~ min(click_email_link_date, click_reminder_link_date),
                                                      is.na(click_email_link_date) & !is.na(click_reminder_link_date) ~ click_reminder_link_date,
                                                      !is.na(click_email_link_date) & is.na(click_reminder_link_date) ~ click_email_link_date), # Date first clicked on email or reminder link
         clicked_link_firstday = if_else(!is.na(clicked_email_reminder_link_date) & clicked_email_reminder_link_date == ymd("2020-09-29"), 1, 0, 0), # Clicked email link on day first email was sent
         clicked_link_ontime = if_else(!is.na(clicked_email_reminder_link_date) & clicked_email_reminder_link_date <= ymd("2020-10-06"), 1, 0, 0),   # Clicked email or reminder link by deadline
         clicked_link_late = if_else(!is.na(clicked_email_reminder_link_date), 1, 0, 0))                                                  # Clicked email or reminder link at all

# (2.8): Create daily outcomes.
dates <- seq.Date(from = min(randomization$takeup_date, na.rm = T), 
                  to = max(randomization$takeup_date, na.rm = T), 
                  by = "day")
for (i in 1:length(dates)) {
  column <- paste0("accept_fee_", gsub("-", "_", dates[i]) )
  print(column)
  randomization[[column]] <- if_else(randomization$takeup_date == ymd(dates[i]) & randomization$accepted_offer_late == 1,1,0,0)
  
  column2 <- paste0("accept_fee_cum", gsub("-", "_", dates[i]) )
  print(column2)
  randomization[[column2]] <- if_else(randomization$takeup_date <= ymd(dates[i]) & randomization$accepted_offer_late == 1,1,0,0)
  
  column3 <- paste0("open_email_", gsub("-", "_", dates[i]) )
  print(column3)
  randomization[[column3]] <- if_else(randomization$open_email_date == ymd(dates[i]),1,0,0)
  
  column4 <- paste0("open_email_cum", gsub("-", "_", dates[i]) )
  print(column4)
  randomization[[column4]] <- if_else(randomization$open_email_date <= ymd(dates[i]),1,0,0)
  
  column5 <- paste0("open_email_reminder_cum", gsub("-", "_", dates[i]) )
  print(column5)
  randomization[[column5]] <- if_else(randomization$open_email_reminder_date <= ymd(dates[i]),1,0,0)
  
}
assert_that(sum(randomization$accept_fee_2020_10_04) == 
              randomization %>% count(takeup_date) %>% filter(takeup_date == ymd(20201004)) %>% summarise(sum(n)))

# (2.9): Create weekly outcomes.
weeks <- seq.Date(from = min(randomization$takeup_date, na.rm = T), 
                  to = max(randomization$takeup_date, na.rm = T), 
                  by = "week")
randomization %<>% 
  mutate(accept_fee_week1 = as.numeric(accepted_offer_late == 1 & takeup_date <= ymd("2020-10-06")),
         accept_fee_week2 = as.numeric(accepted_offer_late == 1 & takeup_date >  ymd("2020-10-06") & takeup_date <= ymd("2020-10-13")),
         accept_fee_week3 = as.numeric(accepted_offer_late == 1 & takeup_date >  ymd("2020-10-13") & takeup_date <= ymd("2020-10-19")),
         
         open_email_week1 = as.numeric(accepted_offer_late == 1 & open_email_date <= ymd("2020-10-06")),
         open_email_week2 = as.numeric(accepted_offer_late == 1 & open_email_date >  ymd("2020-10-06") & open_email_date <= ymd("2020-10-13")),
         open_email_week3 = as.numeric(accepted_offer_late == 1 & open_email_date >  ymd("2020-10-13") & open_email_date <= ymd("2020-10-19")),
         
         accept_fee_cumweek1 = as.numeric(accepted_offer_late == 1 & takeup_date <= ymd("2020-10-06")),
         accept_fee_cumweek2 = as.numeric(accepted_offer_late == 1 & takeup_date <= ymd("2020-10-13")),
         accept_fee_cumweek3 = as.numeric(accepted_offer_late == 1 & takeup_date <= ymd("2020-10-19")), 
         
         open_email_cumweek1 = as.numeric(accepted_offer_late == 1 & open_email_date <= ymd("2020-10-06")),
         open_email_cumweek2 = as.numeric(accepted_offer_late == 1 & open_email_date <= ymd("2020-10-13")),
         open_email_cumweek3 = as.numeric(accepted_offer_late == 1 & open_email_date <= ymd("2020-10-19")))

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

# (2.14): Fix sales quartiles variables.
# We have to remake the sales quartiles because of coding error in 07_prep_randomization.R.
baseline_sales %<>% 
  mutate(sales_quartiles = ntile(sales, 4)) 
randomization %<>%
  select(!sales_quartiles) %>%
  left_join(baseline_sales %>% select(!flag), by = "organization_uuid") %>%
  left_join(benefits, by = "organization_uuid")

# (2.15): Create alternate strata FE.
randomization %<>% mutate(strata_fe_actualsalesq = paste(sales_quartiles, new_buss_type, sep = "_"))
assert_that(length(unique(randomization$strata_fe_actualsalesq)) == 24) 
randomization$strata_fe %>% head()
randomization$strata_fe_actualsalesq %>% head()

# (2.16): Create expected gain variable.
randomization %<>% 
  mutate(expected_gain = fee_reduction - sales - 6, 
         gain_quartiles = ntile(expected_gain, 4), 
         gain_hilow = ifelse(gain_quartiles >= 3, "high_gain", "low_gain"), 
         gain_quartiles = case_when(gain_quartiles == 1 ~ "gain_q1", 
                                    gain_quartiles == 2 ~ "gain_q2", 
                                    gain_quartiles == 3 ~ "gain_q3", 
                                    gain_quartiles == 4 ~ "gain_q4", ))

# (2.17): Create variables for heterogeneity tests.
randomization %<>% 
  mutate(hilow_sales = as.factor(if_else(sales_quartiles >= 3, "above_med", "below_med")),      # Above/below median sales
         lower_fee = case_when(fee_type == "2.75% offer" ~ 1,                                       # New fee type
                               fee_type == "3% offer" ~ 0, 
                               is.na(fee_type) ~ 0),
         length = if_else(length >= median(length), "OldUser", "NewUser", missing = "Missing"),  # Old vs New user
         length = as.factor(length),
         gender = if_else(gender == "H", "Male", "Female", missing = "Missing"),                # Gender
         gender = as.factor(gender),
         # Sales Quartiles
         salesq_fac = case_when(sales_quartiles == 1 ~ "quartile1",
                                sales_quartiles == 2 ~ "quartile2", 
                                sales_quartiles == 3 ~ "quartile3", 
                                sales_quartiles == 4 ~ "quartile4"),
         salesq_fac = as.factor(salesq_fac),
         covid_shock = case_when(covid_shock_quartile == "covid_shock_q3" | covid_shock_quartile == "covid_shock_q4" ~ "above_median" , 
                                 covid_shock_quartile == "covid_shock_q1" | covid_shock_quartile == "covid_shock_q2" ~ "below_median" , 
                                 covid_shock_quartile == "covid_shock_undefined"                                     ~ "undefined"),
         above_median_covid_shock = as.numeric(covid_shock == "above_median"), 
         below_median_covid_shock = as.numeric(covid_shock == "below_median"), 
         undefined_covid_shock    = as.numeric(covid_shock == "undefined"))

# (2.18): Create variables for pooled heterogeneity.
randomization %<>% 
  mutate(high_sales = as.numeric(hilow_sales == "above_med"),
         old_user = as.numeric(length == "OldUser"),
         fee_2.75 = as.numeric(fee_type == "2.75% offer"),
         high_gain = as.numeric(gain_hilow == "high_gain"),
         fixed_rate = as.numeric(commission_model == "FIXED_RATE"))

# We want only those firms that received a fee offer
#sum(!is.na(randomization$email_send_date)) # Only 29,715 received email (well some of those are in control, but still 285 we aren't sure about)
#randomization %<>% filter(!(is.na(email_send_date) & treat_id != 1))
# UPDATE on 4-12-2021
# We are going to include everyone, even if they did not receive an email. 

# (2.19): Exclude one user who got a permanent extension.
randomization %<>% filter(organization_uuid != "0d7623ca-977f-11e9-8cec-62d790593222")

################################################
##    (3): Include text message information.  ##
################################################
# (3.1): Remove observations with no firm ID.
sms_10am %<>% filter(!is.na(organization_uuid))
sms_1005am %<>% filter(!is.na(organization_uuid))

# (3.2): Keep only necessary variables and firms that are in both datasets.
sms_10am %<>%
  transmute(organization_uuid, 
            message_10am = MENSAJE, 
            sent_messages_10am = `Total envios`)
sms_1005am %<>%
  transmute(organization_uuid, 
            message_1005am = MENSAJE, 
            sent_messages_1005am = `Total enviados`)

# (3.3): Merge with treatment results data frame.
randomization %<>%
  left_join(sms_10am, "organization_uuid") %>%
  left_join(sms_1005am, "organization_uuid")

# (3.4): Create message reception variables.
randomization %<>% 
  mutate(received_1_10am = if_else(sent_messages_10am == 1,1,0,0), 
         received_2_10am = if_else(sent_messages_10am == 2,1,0,0),
         received_3_10am = if_else(sent_messages_10am == 3,1,0,0), 
         received_4_10am = if_else(sent_messages_10am == 4,1,0,0), 
         
         received_1_1005am = if_else(sent_messages_1005am == 1,1,0,0), 
         received_2_1005am = if_else(sent_messages_1005am == 2,1,0,0),
         received_3_1005am = if_else(sent_messages_1005am == 3,1,0,0), 
         received_4_1005am = if_else(sent_messages_1005am == 4,1,0,0))

# (3.5): Create grouped variables.
randomization %<>%
  mutate(messages_10am = case_when(received_1_10am == 1 ~ "one message", 
                                   received_2_10am == 1 ~ "two messages", 
                                   received_3_10am == 1 ~ "three messages", 
                                   received_4_10am == 1 ~ "four messages", ), 
         messages_1005am = case_when(received_1_1005am == 1 ~ "one message", 
                                     received_2_1005am == 1 ~ "two messages", 
                                     received_3_1005am == 1 ~ "three messages", 
                                     received_4_1005am == 1 ~ "four messages",))
randomization %<>%
  mutate(messages_10am = if_else(is.na(messages_10am), "zero messages", messages_10am), 
         messages_1005am = if_else(is.na(messages_1005am), "zero messages", messages_1005am))

# (3.6): Restrict sample to users fintech sends us data on.
# fintech sends us data on 33979 users, not the full 34009. 
# Restrict the sample to those users for clarity.
# We have a sample of 33978, because of the one user we filtered out.
randomization %<>% filter(organization_uuid %in% fintech_users)

# (3.7): Create a lighter version of this data frame.
randomization2 <- randomization %>% 
  select(-colnames(randomization)[str_detect(colnames(randomization), "accept_fee_cum202|accept_fee_202|open_email_cum202|open_email_202|open_email_reminder_cum202")])

# (3.8): Export datasets.
write_csv_(randomization, here("proc", "fintech_fee.csv"))
write_csv_(randomization2, here("proc", "fintech_fee_light.csv"))

