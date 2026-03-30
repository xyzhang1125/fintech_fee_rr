#--------------------------------------------------------------------------------------------
# File name: 		      01a_prepdata.R
# Creation date:      2022-01-03
# Author:          		Noah Forougi & César Landín
# Files used:
# 	- here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_WEEKLY_ACTIVITY.csv")
# 	- here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv"))
# Files created:
# 	- here("proc", "weekly_grouped.rds")
# 	- here("proc", "monthly.rds")
# Purpose:
#   - In this script, the weekly data is aggregated into months. The data is also prepared to be 
#     scaled by week to generate monthly values that will serve as inputs for the graphs.
#--------------------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(data.table)
library(lubridate)
library(here)
library(magrittr)
library(assertthat)
library(conflicted)
conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::year)
# Source functions
source(here("scripts", "programs", "myfunctions.R"))

#######################################################
##  (1): Clean weekly data and create monthly data.  ##
#######################################################
# (1.1): Read in data.
weekly_data <- fread(here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_WEEKLY_ACTIVITY.csv"))
users <- fread(here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv"))

# (1.2): Get each user's start date.
users %<>%
  select(organization_uuid, commission_model, start_date = created_date) %>%
  mutate(start_date = ymd(substr(start_date, 1, 11)))

# (1.3): Complete data frame for all merchants and all weeks. Then get observations of firms only for weeks after their start date.
weekly_data %<>%
  mutate(week = ymd(f0_)) %>% 
  complete(organization_uuid, week) %>%
  left_join(., users, by = "organization_uuid") %>%
  mutate(week_end = week + 7) %>% # The start date is given daily, while the volume is given weekly; need to condition on week END date
  filter(week_end >= start_date)

# (1.4): Drop zero commission group.
weekly_data %<>% filter(commission_model != "ZERO_COMMISSION")

# (1.5): Prepare mapping to monthly data.
setDT(weekly_data)
weekly_data[, month := month(week)]
weekly_data[, year := year(week)]

# (1.6): Create august data separately because it needs to be scaled.
august <- weekly_data %>%
  filter(month == 8 & year == 2020 & week != "2020-08-16") # select august data and ignore most recent week (only monday data)
august %<>%
  mutate(valid_volume_scaled = valid_volume * 30.42 / 7,
         nr_valid_payment_scaled = nr_valid_payments * 30.42 / 7) %>% # Get scaled value for each week in august (monthly value)
  group_by(organization_uuid, month, year) %>%
  summarise(monthly_volume = mean(valid_volume_scaled, na.rm = T),
            monthly_trans = mean(nr_valid_payment_scaled, na.rm = T)) %>% # If multiple observations for a firm in august, monthly volume is the average of their two monthly scales
  mutate(monthly_volume = na_to_0(monthly_volume),
         monthly_trans = na_to_0(monthly_trans))

# (1.7): Ensure if you make positive sales you have positive number of transactions.
assert_that(august %>% filter(monthly_volume > 0 & monthly_trans == 0) %>% nrow() == 0)

# (1.8): Create groups.
august %<>%
  left_join(users, by = "organization_uuid") %>%
  mutate(group0 = as.numeric(monthly_volume == 0),
         grouplow = as.numeric(monthly_volume < 1400 & monthly_volume > 0),
         group1 = as.numeric(monthly_volume >= 1400 & monthly_volume <= 20000),
         group2 = as.numeric(monthly_volume > 20000),
         group2_sr = as.numeric(monthly_volume > 20000 & commission_model == "SMART_RATE"),
         group2_fr = as.numeric(monthly_volume > 20000 & commission_model == "FIXED_RATE"))

# (1.9): Replace June 30th week into July month.
weekly_data %<>%
  mutate(month = ifelse(month == 6 & year == 2019, 7, month))

# (1.10): Convert to monthly data.
monthly_data <- weekly_data[, keyby = .(organization_uuid, month, year),
                            .(monthly_volume = sum(valid_volume, na.rm = T),
                              monthly_trans = sum(nr_valid_payments, na.rm = T))] %>%
  left_join(users, by = "organization_uuid")

# (1.11): Classify monthly data
setDT(monthly_data)
monthly_data[, group0 := as.numeric(monthly_volume == 0)]
monthly_data[, grouplow := as.numeric(monthly_volume < 1400 & monthly_volume > 0)]
monthly_data[, group1 := as.numeric(monthly_volume >= 1400 & monthly_volume <= 20000)]
monthly_data[, group2 := as.numeric(monthly_volume > 20000)]
monthly_data[, group2_sr := as.numeric(monthly_volume > 20000 & commission_model == "SMART_RATE")]
monthly_data[, group2_fr := as.numeric(monthly_volume > 20000 & commission_model == "FIXED_RATE")]

# (1.12): Drop August 2020 observations because those need to be scaled separately
monthly_data %<>%
  filter(!(month == 8 & year == 2020))

# (1.13): Join in August data separately
monthly_data %<>% rbind(august)

############################################
##  (2): Prepare weekly data for graphs.  ##
############################################
# (2.1): Create scaled values for each organization observation.
weekly_data_scaled <- weekly_data[, keyby = .(organization_uuid, week),
                                  .(valid_volume = valid_volume,
                                    valid_volume_scaled = valid_volume * (30.42 / 7))] # To scale up to a monthly value, multiply the weekly value by the average number of weeks in a month

# (2.2): Join with user data. 
weekly_data_scaled %<>%
  left_join(users, by = "organization_uuid")

# (2.3): Classify weekly observations based on scaled value
weekly_data_scaled %<>%
  mutate(valid_volume_scaled = na_to_0(weekly_data_scaled$valid_volume_scaled),
         valid_volume = na_to_0(weekly_data_scaled$valid_volume),
         group0 = as.numeric(valid_volume_scaled == 0),
         grouplow = as.numeric(valid_volume_scaled > 0 & valid_volume_scaled < 1400),
         group1 = as.numeric(valid_volume_scaled >= 1400 & valid_volume_scaled <= 20000),
         group2 = as.numeric(valid_volume_scaled > 20000),
         group2_sr = as.numeric(valid_volume_scaled > 20000 & commission_model == "SMART_RATE"),
         group2_fr = as.numeric(valid_volume_scaled > 20000 & commission_model == "FIXED_RATE"))

# (2.4): Create a count of merchants each week
weekly_merchants <- weekly_data %>%
  group_by(week) %>%
  count()

# (2.5): Create variables of interest each week
weekly_data_grouped <- weekly_data_scaled[, by = week,
                                          .(group0_count = sum(group0),
                                            grouplow_count = sum(grouplow),
                                            group1_count = sum(group1),
                                            group2_count = sum(group2),
                                            group2sr_count = sum(group2_sr),
                                            group2fr_count = sum(group2_fr),
                                            total_count = length(grouplow))]
weekly_data_grouped %<>%
  mutate(group0_share = group0_count / total_count,
         grouplow_share = grouplow_count / total_count,
         group1_share = group1_count / total_count,
         group2_share = group2_count / total_count,
         group2sr_share = group2sr_count / total_count,
         group2fr_share = group2fr_count / total_count)

#########################
##  (3): Export data.  ##
#########################
# (3.1): Export monthly and weekly data
saveRDS(weekly_data_grouped, here("proc", "weekly_grouped.rds"))
saveRDS(monthly_data, here("proc", "monthly.rds"))
