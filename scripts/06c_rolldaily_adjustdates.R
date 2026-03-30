#--------------------------------------------------------------------------------------------
# File name: 		      06c_rolldaily_adjustdates.R
# Creation date:      2022-02-08
# Author:          		Noah Forougi, César Landín, and Xinyu Zhang
# Files used:
# 	- here("proc", "fintech_fee_daily.qs")
# 	- here("proc", "users.rds")
# 	- here("proc", "fintech_fee.csv")
# Files created:
# 	- here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
# 	- here("proc", "fintech_fee_dtm_adjusteddates.qs")
# Purpose:
# 	Convert daily to monthly/weekly data, while converting Sep 29/30 to October.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(data.table)
library(qs)
library(assertthat)
library(lubridate)
library(magrittr)
source(here("scripts", "programs", "myfunctions.R"))
source(here("scripts", "programs", "winsorize.R"))
#########################################################

####################################
##  (1): Create monthly dataset.  ##
####################################
# (1.1): Import daily data, user details and treatment assingment.
daily <- qread(here("proc", "fintech_fee_daily.qs")) %>% 
  select(organization_uuid, timestamp_day, valid_volume, nr_valid_payments)
# Xinyu added first_valid_payment for robustness check
users <- read_rds(here("proc", "users.rds")) %>% select(organization_uuid, created_date, first_valid_payment)
fintech_fee <- fread(here("proc", "fintech_fee.csv"))

# (1.2): Drop daily indicators for fintech_fee.
fintech_fee %<>% select(-starts_with("accept_fee_2")) %>% 
  select(-starts_with("accept_fee_cum2")) %>% 
  select(-starts_with("open_email_2")) %>% 
  select(-starts_with("open_email_cum2"))

# (1.3): Create daily to monthly dataset.
daily <- setDT(daily)
daily[, timestamp_month := substr(timestamp_day, start = 6, stop = 7)]
daily[, timestamp_year := substr(timestamp_day, start = 1, stop = 4)]

# (1.4): Convert Sep 29, Sep 30 to October 1.
daily[timestamp_day == "2020-09-29", timestamp_month := 10]
daily[timestamp_day == "2020-09-30", timestamp_month := 10]

# (1.5): Group into monthly.
daily_to_monthly <- daily[, by = .(organization_uuid, timestamp_month, timestamp_year),
                          .(valid_volume = sum(valid_volume), 
                            nr_valid_payments = sum(nr_valid_payments))]
rm(daily)

# (1.6): Join in fintech information. 
# Xinyu also added first valid payment for robustness check
daily_to_monthly <- fintech_fee[daily_to_monthly, on = "organization_uuid"] %>% 
  left_join(users, by = "organization_uuid")

# (1.7): Join in fintech information.
daily_to_monthly %<>% winsorize(outcome = "valid_volume", 
                                newvar  = "valid_volume_w5",  
                                by   = c("group_id", "timestamp_month"),
                                highonly = TRUE)
daily_to_monthly %<>% winsorize(outcome = "nr_valid_payments", 
                                newvar  = "nr_valid_payments_w5",  
                                by   = c("group_id", "timestamp_month"),
                                highonly = TRUE)
daily_to_monthly[,valid_volume_w5_per_transaction := ifelse(nr_valid_payments != 0, valid_volume_w5/nr_valid_payments_w5, 0)]

# (1.8): Create ITT and TOT variables.
daily_to_monthly <- setDT(daily_to_monthly) %>% 
  .[, timestamp_month       := ymd(paste(timestamp_year,timestamp_month, "01"))] %>%
  .[, adopted               := accepted_offer_late] %>%
  .[, post                  := if_else(timestamp_month >= ymd("2020-10-01"), 1, 0)] %>%
  .[, treated               := ifelse(treat_type %in% c("T2", "T3", "T4", "T5", "T6", "T7", "T8"), 1, 0)] %>%
  .[, postXtreated          := post*treated] %>%
  .[, postXadopted          := post*accepted_offer_late] %>%
  .[, log_valid_volume      := log(valid_volume + 1)] %>%
  .[, log_valid_volume_w5   := log(valid_volume_w5 + 1)] %>%
  .[, arcsinh_valid_volume  := asinh(valid_volume)] %>%
  .[, fee_2.75              := if_else(fee_type == "2.75% offer", 1, 0,0)] %>% # Code it such that control is 0 
  .[, adoptedXfee_2.75      := adopted * fee_2.75] %>%
  .[, treatedXfee_2.75      := treated * fee_2.75] %>%
  .[, postXadoptedXfee_2.75 := postXadopted * fee_2.75] %>%
  .[, postXtreatedXfee_2.75 := postXtreated * fee_2.75] %>%
  .[, make_sale             := as.numeric(nr_valid_payments > 0)] %>%
  .[, log_nr_payments       := log(nr_valid_payments + 1)] %>%
  .[, log_nr_payments_w5    := log(nr_valid_payments_w5 + 1)] %>%
  .[, asinh_nr_payments     := asinh(nr_valid_payments)] %>%
  .[, valid_volume_per_transaction       := ifelse(nr_valid_payments != 0, valid_volume/nr_valid_payments, 0)] %>%
  .[, log_valid_volume_per_transaction   := log(valid_volume_per_transaction + 1)] %>%
  .[, asinh_valid_volume_per_transaction := asinh(valid_volume_per_transaction)]


# (1.9): Export daily to monthly.
qsave(daily_to_monthly, here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs"))

daily_to_monthly_original <- daily_to_monthly %>% 
  filter(timestamp_month <= "2021-03-01") %>% 
  select(all_of(colnames(daily_to_monthly)[!str_detect(colnames(daily_to_monthly), 
                                                       paste0("mar_2021|apr_2021|may_2021|june_2021|july_2021|aug_2021|sep_2021|oct_2021|",
                                                              "nov_2021|dec_2021|jan_2022|feb_2022|mar_2021|apr_2021|may_2021|june_2021"))]))

qsave(daily_to_monthly_original, here("proc", "fintech_fee_dtm_adjusteddates.qs"))

rm(daily_to_monthly, daily_to_monthly_original)

# CL (2022-10-26): if survey randomization does not work for some reason, it will be because of this file
# treat_description was updated, the original definition was wrong

# ###################################
# ##  (2): Create weekly dataset.  ##
# ###################################
# # (2.1): Import weekly data.
# fintech_fee_weekly <- qread(here("proc", "fintech_fee_weekly.qs")) %>% 
#   select(timestamp_week)
# min_weekly <-  min(fintech_fee_weekly$timestamp_week)
# rm(fintech_fee_weekly)
# 
# # (2.2): Import daily data and generate month and year variables.
# daily <- qread(here("proc", "fintech_fee_daily.qs")) %>% 
#     select(organization_uuid, timestamp_day, valid_volume, nr_valid_payments)
# daily <- setDT(daily)
# daily[, timestamp_month := substr(timestamp_day, start = 6, stop = 7)]
# daily[, timestamp_year := substr(timestamp_day, start = 1, stop = 4)]
# daily[timestamp_day == "2020-09-29", timestamp_month := 10]
# daily[timestamp_day == "2020-09-30", timestamp_month := 10]
# daily[, timestamp_day := ymd(timestamp_day)]
# 
# # (2.3): Filter to keep days >= earliest week.
# daily %<>% filter(timestamp_day >= min_weekly)
# min(daily$timestamp_day) == min_weekly
# 
# # (2.4): Get sequence of days.
# days <- tibble(timestamp_day = seq.Date(ymd(min(daily$timestamp_day)), ymd(max(daily$timestamp_day)), "1 day")) %>%
#   left_join(tibble(timestamp_day = seq.Date(ymd(min(daily$timestamp_day)), ymd(max(daily$timestamp_day)), "1 week"), 
#                    week = seq.Date(ymd(min(daily$timestamp_day)), ymd(max(daily$timestamp_day)), "1 week"))) %>% 
#   fill(week)
# 
# # (2.5): Change it so that any data from 2020-09-29 is assigned to the next week.
# days %<>% 
#   mutate(week = if_else(timestamp_day == ymd("20200928"), ymd("20200921"), week))
# 
# # (2.6): Generate daily to weekly dataset.
# weekly <- daily %>% left_join(days, by = "timestamp_day")
# rm(daily, days)
# weekly <- setDT(weekly)
# weekly[, timestamp_week := week]
# 
# 
# weekly[, keyby = .(organization_uuid, timestamp_week),
#        .(count      = .N)]
# length(unique(weekly$organization_uuid))
# 
# test <- weekly[, keyby = .(organization_uuid, timestamp_day), 
#                .N]
# all(test$N == 1)
# 
# weekly <- weekly[, keyby = .(organization_uuid, timestamp_week),
#                  .(valid_volume = sum(valid_volume), 
#                    nr_valid_payments = sum(nr_valid_payments))]
# rm(test)
# 
# # (2.7): Complete the dataset, so we have observations for each firm-week combination.
# weekly %>% head(10)
# users <- read_rds(here("proc", "users.rds")) %>% select(organization_uuid, created_date)
# weekly %<>%  
#   ungroup() %>%
#   complete(organization_uuid, timestamp_week) %>%
#   left_join(users %>% select(organization_uuid, created_date), by = "organization_uuid")
# rm(users)
# 
# weekly_obs <- weekly %>% 
#   group_by(timestamp_week) %>% 
#   count() # We have 33,978 observations for each week 
# assertthat::assert_that(all(weekly_obs$n == 33978))
# 
# # (2.8): Change the timestamp week to the end of the week to filter:
# # We want only observations where the timestamp is after they created an account. Timestamp_week is the START of the week.
# weekly <- setDT(weekly)
# weekly <- weekly[,timestamp_week_end := ymd(timestamp_week) + 7]
# weekly <- weekly[timestamp_week_end >= ymd(created_date)]
# weekly[, !"timestamp_week_end"]
# 
# # (2.9): Change NA to zero because these firms have the technology in these months, they just choose not to use it.
# weekly %<>%
#   mutate(nr_valid_payments = ifelse(is.na(nr_valid_payments), 0, nr_valid_payments),
#          valid_volume = ifelse(is.na(valid_volume), 0, valid_volume))
# 
# weekly_obs <- weekly %>% 
#   group_by(timestamp_week) %>% 
#   count() #%>% View
# 
# # (2.10): Save intermediate dataset.
# assert_that(max(weekly_obs$n) <= 33978)
# write_csv(weekly, here("proc", "weekly_adjusted_temp.csv"))
# weekly <- read_csv(here("proc", "weekly_adjusted_temp.csv"))
# 
# # (2.11): Join with experiment data.
# fintech_fee <- fread(here("proc", "fintech_fee.csv"))
# fintech_fee %<>% 
#   select(-starts_with(c("accept_fee_20", "accept_fee_cum20", "open_email_20", "open_email_cum20")))
# weekly <- setDT(weekly)
# weekly <- fintech_fee[weekly, on = "organization_uuid"]
# rm(fintech_fee)
# 
# # (2.12): Create variables for regressions.
# weekly %<>%
#   .[, adopted := accepted_offer_late] %>% 
#   .[, post                  := if_else(timestamp_week >= ymd(20200930), 1, 0)] %>%  # Recall that we changed 9-29-2020 to week of 9-30-2020
#   .[, treated               := ifelse(treat_type %in% c("T2", "T3", "T4", "T5", "T6", "T7", "T8"), 1, 0)] %>% 
#   .[, postXtreated          := post * treated] %>% 
#   .[, postXadopted          := post * accepted_offer_late] %>% 
#   .[, log_valid_volume := log(valid_volume + 1)] %>% 
#   .[, arcsinh_valid_volume := asinh(valid_volume)] %>% 
#   .[,fee_2.75              := if_else(fee_type == "2.75% offer", 1, 0,0)] %>%  # Code it such that control is 0 
#   .[, treatedXfee_2.75          := treated * fee_2.75] %>% 
#   .[, adoptedXfee_2.75          := adopted * fee_2.75] %>% 
#   .[,postXadoptedXfee_2.75 := postXadopted * fee_2.75] %>% 
#   .[,postXtreatedXfee_2.75 := postXtreated * fee_2.75] %>% 
#   .[,make_sale := as.numeric(nr_valid_payments > 0)] %>% 
#   .[,log_nr_payments := log(nr_valid_payments + 1)] %>% 
#   .[,asinh_nr_payments := asinh(nr_valid_payments)] %>% 
#   .[,valid_volume_per_transaction := ifelse(nr_valid_payments != 0, valid_volume/nr_valid_payments, 0)] %>% 
#   .[,log_valid_volume_per_transaction := log(valid_volume_per_transaction + 1)] %>% 
#   .[,asinh_valid_volume_per_transaction := asinh(valid_volume_per_transaction)]
# 
# # (2.13): Create winsorized sales.
# weekly_w <- weekly %>% select(organization_uuid, valid_volume, group_id, timestamp_week) 
# weekly_w %<>% 
#   winsorize(outcome = "valid_volume", 
#             newvar  = "valid_volume_w5",  
#             by   = c("group_id", "timestamp_week"),
#             highonly = TRUE) %>%
#   select(organization_uuid, timestamp_week, valid_volume_w5)
# 
# weekly <- setDT(weekly)
# weekly_w <- setDT(weekly_w)
# 
# weekly <- weekly_w[weekly, on =c("organization_uuid", "timestamp_week")]
# rm(weekly_w)
# 
# # (2.14):Create winsorized payments.
# weekly_wp <- weekly %>% select(organization_uuid, nr_valid_payments, group_id, timestamp_week) 
# weekly_wp %<>% 
#   winsorize(outcome = "nr_valid_payments", 
#             newvar  = "nr_valid_payments_w5",  
#             by   = c("group_id", "timestamp_week"),
#             highonly = TRUE) %>%
#   select(organization_uuid, timestamp_week, nr_valid_payments_w5)
# 
# weekly <- setDT(weekly)
# weekly_wp <- setDT(weekly_wp)
# 
# weekly <- weekly_wp[weekly, on =c("organization_uuid", "timestamp_week")]
# 
# # (2.15): Create winsorized sales per transaction.
# weekly[,valid_volume_w5_per_transaction := ifelse(nr_valid_payments != 0, valid_volume_w5/nr_valid_payments_w5, 0)]
# 
# # (2.16):Export daily to weekly.
# qsave(weekly, here("proc", "fintech_fee_dtw_adjusteddates.qs"))

