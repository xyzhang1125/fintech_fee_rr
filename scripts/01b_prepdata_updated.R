#--------------------------------------------------------------------------------------------
# File name: 		      01b_prepdata_updated.R
# Creation date:      2022-01-03
# Author:          		Noah Forougi & César Landín & Xinyu Zhang
# Files used:
# 	- here("data", "Sent_20200903", "october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv")
# 	- here("data", "Sent_20200903", "october2020_pricing_ABtest_USERS_0903.csv")
# 	- here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv")
# Files created:
# 	- here("proc", "users.rds")
# 	- here("proc", "monthly.rds")
# Purpose:
# 	- This script updates the monthly.rds produced in 01_prepdata to a completed dataset 
#     with the new data sent by fintech on 09-03-2020.
#--------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(here)
library(magrittr)
library(assertthat)

# Source functions
source(here("scripts", "programs", "myfunctions.R"))

#################################
##  (1): Update monthly data.  ##
#################################
# (1.1): Read in data.
monthly_data <- fread(here("data", "Sent_20200903", "october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv")) %>% 
  mutate(timestamp_month = as_date(timestamp_month))
users_new <- fread(here("data", "Sent_20200903", "october2020_pricing_ABtest_USERS_0903.csv"))
users <- fread(here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv"))

# (1.2): Make sure that this data is in fact unique observations for each month.
assert_that(length(unique(paste(monthly_data$organization_uuid, monthly_data$timestamp_month))) == length(monthly_data$organization_uuid))

# (1.3): Complete data frame.
monthly_data %<>%
  complete(organization_uuid, timestamp_month) %>%
  replace_na(list(nr_valid_payments = 0, 
                  valid_volume = 0))

# (1.4): Get each user's start date and first valid payment date
# start date
users %<>% 
  mutate(start_date = created_date) %>%
  mutate(start_date = ymd(paste(substr(start_date, 1,7), "01", sep = "-")))
users_new %<>% 
  mutate(start_date = created_date) %>%
  mutate(start_date = ymd(paste(substr(start_date, 1,7), "01", sep = "-")))

# first valid payment date
users %<>% 
  mutate(first_payment_date = first_valid_payment) %>%
  mutate(first_payment_date = as.Date(first_payment_date))

users_new %<>% 
  mutate(first_payment_date = first_valid_payment) %>%
  mutate(first_payment_date = as.Date(first_payment_date))

# (1.5): Filter users. 
`%nin%` = Negate(`%in%`)
users_new <- users_new[users_new$organization_uuid %nin% users$organization_uuid,]

# (1.6): Save updated users dataset.
users <- bind_rows(users, users_new)
saveRDS(users, here("proc", "users.rds"))

# (1.7): Join and filter based on start date.
monthly_data %<>% 
  left_join(., users, by = "organization_uuid") %>% 
  filter(ymd(timestamp_month) >= ymd(start_date))

# (1.8): Exclude zero commission groups.
monthly_data %<>% filter(commission_model != "ZERO_COMMISSION")

# (1.9): Create groups.
monthly_data %<>%
  mutate(group0 = as.numeric(valid_volume == 0), 
         grouplow = as.numeric(valid_volume > 0 & valid_volume < 1400), 
         group1 = as.numeric(valid_volume >= 1400 & valid_volume <= 20000),
         group2 = as.numeric(valid_volume > 20000), 
         group2_sr = as.numeric(valid_volume > 20000 & commission_model == "SMART_RATE"), 
         group2_fr = as.numeric(valid_volume > 20000 & commission_model == "FIXED_RATE"))

# (1.10): Save monthly data.
saveRDS(monthly_data, here("proc", "monthly.rds"))
