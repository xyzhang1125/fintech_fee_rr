#--------------------------------------------------------------------------------------------
# File name: 		      06b_rolldaily.R
# Creation date:      2022-02-08
# Author:          		Noah Forougi and César Landín
# Files used:
# 	- here("proc", "fintech_fee_daily.qs")
# 	- here("proc", "users.rds")
# 	- here("proc", "fintech_fee.csv")
# Files created:
# 	- here("proc", "fintech_fee_dtm_dataupdate.qs")
# 	- here("proc", "fintech_fee_dtm.qs")
# Purpose:
# 	- Create monthly panel dataset of transactions and sales volumes from daily data.
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
users <- read_rds(here("proc", "users.rds")) %>% select(organization_uuid, created_date)
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
daily_to_monthly <- daily[, keyby = .(organization_uuid, timestamp_month, timestamp_year),
                          .(valid_volume = sum(valid_volume), 
                            nr_valid_payments = sum(nr_valid_payments))]
rm(daily)

# (1.4): Join in fintech information.
daily_to_monthly <- fintech_fee[daily_to_monthly, on = "organization_uuid"]

# (1.5): Create ITT and TOT variables.
daily_to_monthly <- setDT(daily_to_monthly) %>% 
  .[, timestamp_month              := ymd(paste(timestamp_year,timestamp_month, "01"))] %>% 
  .[, adopted                      := accepted_offer_late] %>% 
  .[, post                         := if_else(timestamp_month > ymd(20200901), 1, 0)] %>% 
  .[, adopted                      := accepted_offer_late] %>% 
  .[, post                         := if_else(timestamp_month >= ymd(20200901), 1, 0)] %>% 
  .[, treated                      := ifelse(treat_type %in% c("T2", "T3", "T4", "T5", "T6", "T7", "T8"), 1, 0)] %>% 
  .[, postXtreated                 := post*treated] %>% 
  .[, postXadopted                 := post*accepted_offer_late] %>% 
  .[, log_valid_volume             := log(valid_volume + 1)] %>% 
  .[, arcsinh_valid_volume         := asinh(valid_volume)] %>% 
  .[, fee_2.75                     := if_else(fee_type == "2.75% offer", 1, 0,0)] %>%  # Code it such that control is 0 
  .[, adoptedXfee_2.75             := adopted * fee_2.75] %>% 
  .[, treatedXfee_2.75             := treated * fee_2.75] %>% 
  .[, postXadoptedXfee_2.75        := postXadopted * fee_2.75] %>% 
  .[, postXtreatedXfee_2.75        := postXtreated * fee_2.75] %>% 
  .[, make_sale                    := as.numeric(nr_valid_payments > 0)] %>% 
  .[, log_nr_payments              := log(nr_valid_payments + 1)] %>% 
  .[, asinh_nr_payments            := asinh(nr_valid_payments)] %>% 
  .[, valid_volume_per_transaction := ifelse(nr_valid_payments != 0, valid_volume/nr_valid_payments, 0)]

daily_to_monthly %<>% winsorize(outcome = "valid_volume", 
                       newvar  = "valid_volume_w5",  
                       by   = c("group_id", "timestamp_month"))
daily_to_monthly %<>% winsorize(outcome = "nr_valid_payments", 
                       newvar  = "nr_valid_payments_w5",  
                       by   = c("group_id", "timestamp_month"))

daily_to_monthly <- daily_to_monthly[, valid_volume_w5_per_transaction := ifelse(nr_valid_payments != 0, valid_volume_w5/nr_valid_payments_w5, 0)] %>% 
  .[, log_valid_volume_per_transaction := log(valid_volume_per_transaction + 1)] %>% 
  .[, asinh_valid_volume_per_transaction := asinh(valid_volume_per_transaction)]

# (1.6): Create monthly indicators.
daily_to_monthly %<>%
  .[, july_2019 := as.numeric(timestamp_month == "2019-07-01")] %>%
  .[, aug_2019  := as.numeric(timestamp_month == "2019-08-01")] %>%
  .[, sep_2019  := as.numeric(timestamp_month == "2019-09-01")] %>%
  .[, oct_2019  := as.numeric(timestamp_month == "2019-10-01")] %>%
  .[, nov_2019  := as.numeric(timestamp_month == "2019-11-01")] %>%
  .[, dec_2019  := as.numeric(timestamp_month == "2019-12-01")] %>%
  .[, jan_2020  := as.numeric(timestamp_month == "2020-01-01")] %>%
  .[, feb_2020  := as.numeric(timestamp_month == "2020-02-01")] %>%
  .[, mar_2020  := as.numeric(timestamp_month == "2020-03-01")] %>%
  .[, apr_2020  := as.numeric(timestamp_month == "2020-04-01")] %>%
  .[, may_2020  := as.numeric(timestamp_month == "2020-05-01")] %>%
  .[, june_2020 := as.numeric(timestamp_month == "2020-06-01")] %>%
  .[, july_2020 := as.numeric(timestamp_month == "2020-07-01")] %>%
  .[, aug_2020  := as.numeric(timestamp_month == "2020-08-01")] %>%
  .[, sep_2020  := as.numeric(timestamp_month == "2020-09-01")] %>%
  .[, oct_2020  := as.numeric(timestamp_month == "2020-10-01")] %>%
  .[, nov_2020  := as.numeric(timestamp_month == "2020-11-01")] %>%
  .[, dec_2020  := as.numeric(timestamp_month == "2020-12-01")] %>%
  .[, jan_2021  := as.numeric(timestamp_month == "2021-01-01")] %>%
  .[, feb_2021  := as.numeric(timestamp_month == "2021-02-01")] %>%
  .[, mar_2021  := as.numeric(timestamp_month == "2021-03-01")] %>%
  .[, apr_2021  := as.numeric(timestamp_month == "2021-04-01")] %>%
  .[, may_2021  := as.numeric(timestamp_month == "2021-05-01")] %>%
  .[, june_2021 := as.numeric(timestamp_month == "2021-06-01")] %>%
  .[, july_2021 := as.numeric(timestamp_month == "2021-07-01")] %>%
  .[, aug_2021  := as.numeric(timestamp_month == "2021-08-01")] %>%
  .[, sep_2021  := as.numeric(timestamp_month == "2021-09-01")] %>% 
  .[, oct_2021  := as.numeric(timestamp_month == "2021-10-01")] %>% 
  .[, nov_2021  := as.numeric(timestamp_month == "2021-11-01")] %>% 
  .[, dec_2021  := as.numeric(timestamp_month == "2021-12-01")] %>% 
  .[, jan_2022  := as.numeric(timestamp_month == "2022-01-01")] %>% 
  .[, feb_2022  := as.numeric(timestamp_month == "2022-02-01")] %>% 
  .[, mar_2022  := as.numeric(timestamp_month == "2022-03-01")] %>% 
  .[, apr_2022  := as.numeric(timestamp_month == "2022-04-01")] %>% 
  .[, may_2022  := as.numeric(timestamp_month == "2022-05-01")] %>% 
  .[, jun_2022  := as.numeric(timestamp_month == "2022-06-01")] %>% 
  .[, jul_2022  := as.numeric(timestamp_month == "2022-07-01")] %>% 
  .[, aug_2022  := as.numeric(timestamp_month == "2022-08-01")] %>% 
  .[, sep_2022  := as.numeric(timestamp_month == "2022-09-01")] %>% 
  .[, oct_2022  := as.numeric(timestamp_month == "2022-10-01")]

# (1.7): Create monthly interactions.
daily_to_monthly %<>%
  .[, adopted := accepted_offer_late] %>% 
  .[, july_2019Xadopted := as.numeric(timestamp_month == "2019-07-01")*adopted] %>%
  .[, aug_2019Xadopted  := as.numeric(timestamp_month == "2019-08-01")*adopted] %>%
  .[, sep_2019Xadopted  := as.numeric(timestamp_month == "2019-09-01")*adopted] %>%
  .[, oct_2019Xadopted  := as.numeric(timestamp_month == "2019-10-01")*adopted] %>%
  .[, nov_2019Xadopted  := as.numeric(timestamp_month == "2019-11-01")*adopted] %>%
  .[, dec_2019Xadopted  := as.numeric(timestamp_month == "2019-12-01")*adopted] %>%
  .[, jan_2020Xadopted  := as.numeric(timestamp_month == "2020-01-01")*adopted] %>%
  .[, feb_2020Xadopted  := as.numeric(timestamp_month == "2020-02-01")*adopted] %>%
  .[, mar_2020Xadopted  := as.numeric(timestamp_month == "2020-03-01")*adopted] %>%
  .[, apr_2020Xadopted  := as.numeric(timestamp_month == "2020-04-01")*adopted] %>%
  .[, may_2020Xadopted  := as.numeric(timestamp_month == "2020-05-01")*adopted] %>%
  .[, june_2020Xadopted := as.numeric(timestamp_month == "2020-06-01")*adopted] %>%
  .[, july_2020Xadopted := as.numeric(timestamp_month == "2020-07-01")*adopted] %>%
  .[, aug_2020Xadopted  := as.numeric(timestamp_month == "2020-08-01")*adopted] %>%
  .[, sep_2020Xadopted  := as.numeric(timestamp_month == "2020-09-01")*adopted] %>%
  .[, oct_2020Xadopted  := as.numeric(timestamp_month == "2020-10-01")*adopted] %>%
  .[, nov_2020Xadopted  := as.numeric(timestamp_month == "2020-11-01")*adopted] %>%
  .[, dec_2020Xadopted  := as.numeric(timestamp_month == "2020-12-01")*adopted] %>%
  .[, jan_2021Xadopted  := as.numeric(timestamp_month == "2021-01-01")*adopted] %>%
  .[, feb_2021Xadopted  := as.numeric(timestamp_month == "2021-02-01")*adopted] %>%
  .[, mar_2021Xadopted  := as.numeric(timestamp_month == "2021-03-01")*adopted] %>%
  .[, apr_2021Xadopted  := as.numeric(timestamp_month == "2021-04-01")*adopted] %>%
  .[, may_2021Xadopted  := as.numeric(timestamp_month == "2021-05-01")*adopted] %>%
  .[, june_2021Xadopted := as.numeric(timestamp_month == "2021-06-01")*adopted] %>%
  .[, july_2021Xadopted := as.numeric(timestamp_month == "2021-07-01")*adopted] %>%
  .[, aug_2021Xadopted  := as.numeric(timestamp_month == "2021-08-01")*adopted] %>%
  .[, sep_2021Xadopted  := as.numeric(timestamp_month == "2021-09-01")*adopted] %>% 
  .[, oct_2021Xadopted  := as.numeric(timestamp_month == "2021-10-01")*adopted] %>% 
  .[, nov_2021Xadopted  := as.numeric(timestamp_month == "2021-11-01")*adopted] %>% 
  .[, dec_2021Xadopted  := as.numeric(timestamp_month == "2021-12-01")*adopted] %>% 
  .[, jan_2022Xadopted  := as.numeric(timestamp_month == "2022-01-01")*adopted] %>% 
  .[, feb_2022Xadopted  := as.numeric(timestamp_month == "2022-02-01")*adopted] %>% 
  .[, mar_2022Xadopted  := as.numeric(timestamp_month == "2022-03-01")*adopted] %>% 
  .[, apr_2022Xadopted  := as.numeric(timestamp_month == "2022-04-01")*adopted] %>% 
  .[, may_2022Xadopted  := as.numeric(timestamp_month == "2022-05-01")*adopted] %>% 
  .[, jun_2022Xadopted  := as.numeric(timestamp_month == "2022-06-01")*adopted] %>% 
  .[, jul_2022Xadopted  := as.numeric(timestamp_month == "2022-07-01")*adopted] %>% 
  .[, aug_2022Xadopted  := as.numeric(timestamp_month == "2022-08-01")*adopted] %>% 
  .[, sep_2022Xadopted  := as.numeric(timestamp_month == "2022-09-01")*adopted] %>% 
  .[, oct_2022Xadopted  := as.numeric(timestamp_month == "2022-10-01")*adopted]

daily_to_monthly %<>%
  .[, july_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-07-01")*adopted*fee_2.75] %>%
  .[, aug_2019XadoptedXfee_2.75  := as.numeric(timestamp_month == "2019-08-01")*adopted*fee_2.75] %>%
  .[, sep_2019XadoptedXfee_2.75  := as.numeric(timestamp_month == "2019-09-01")*adopted*fee_2.75] %>%
  .[, oct_2019XadoptedXfee_2.75  := as.numeric(timestamp_month == "2019-10-01")*adopted*fee_2.75] %>%
  .[, nov_2019XadoptedXfee_2.75  := as.numeric(timestamp_month == "2019-11-01")*adopted*fee_2.75] %>%
  .[, dec_2019XadoptedXfee_2.75  := as.numeric(timestamp_month == "2019-12-01")*adopted*fee_2.75] %>%
  .[, jan_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-01-01")*adopted*fee_2.75] %>%
  .[, feb_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-02-01")*adopted*fee_2.75] %>%
  .[, mar_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-03-01")*adopted*fee_2.75] %>%
  .[, apr_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-04-01")*adopted*fee_2.75] %>%
  .[, may_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-05-01")*adopted*fee_2.75] %>%
  .[, june_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-06-01")*adopted*fee_2.75] %>%
  .[, july_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-07-01")*adopted*fee_2.75] %>%
  .[, aug_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-08-01")*adopted*fee_2.75] %>%
  .[, sep_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-09-01")*adopted*fee_2.75] %>%
  .[, oct_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-10-01")*adopted*fee_2.75] %>%
  .[, nov_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-11-01")*adopted*fee_2.75] %>%
  .[, dec_2020XadoptedXfee_2.75  := as.numeric(timestamp_month == "2020-12-01")*adopted*fee_2.75] %>%
  .[, jan_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-01-01")*adopted*fee_2.75] %>%
  .[, feb_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-02-01")*adopted*fee_2.75] %>%
  .[, mar_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-03-01")*adopted*fee_2.75] %>%
  .[, apr_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-04-01")*adopted*fee_2.75] %>%
  .[, may_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-05-01")*adopted*fee_2.75] %>%
  .[, june_2021XadoptedXfee_2.75 := as.numeric(timestamp_month == "2021-06-01")*adopted*fee_2.75] %>%
  .[, july_2021XadoptedXfee_2.75 := as.numeric(timestamp_month == "2021-07-01")*adopted*fee_2.75] %>%
  .[, aug_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-08-01")*adopted*fee_2.75] %>%
  .[, sep_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-09-01")*adopted*fee_2.75] %>% 
  .[, oct_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-10-01")*adopted*fee_2.75] %>% 
  .[, nov_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-11-01")*adopted*fee_2.75] %>% 
  .[, dec_2021XadoptedXfee_2.75  := as.numeric(timestamp_month == "2021-12-01")*adopted*fee_2.75] %>% 
  .[, jan_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-01-01")*adopted*fee_2.75] %>% 
  .[, feb_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-02-01")*adopted*fee_2.75] %>% 
  .[, mar_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-03-01")*adopted*fee_2.75] %>% 
  .[, apr_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-04-01")*adopted*fee_2.75] %>% 
  .[, may_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-05-01")*adopted*fee_2.75] %>% 
  .[, jun_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-06-01")*adopted*fee_2.75] %>% 
  .[, jul_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-07-01")*adopted*fee_2.75] %>% 
  .[, aug_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-08-01")*adopted*fee_2.75] %>% 
  .[, sep_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-09-01")*adopted*fee_2.75] %>% 
  .[, oct_2022XadoptedXfee_2.75  := as.numeric(timestamp_month == "2022-10-01")*adopted*fee_2.75]

daily_to_monthly %<>%
  .[, july_2019Xtreated := as.numeric(timestamp_month == "2019-07-01")*treated] %>%
  .[, aug_2019Xtreated  := as.numeric(timestamp_month == "2019-08-01")*treated] %>%
  .[, sep_2019Xtreated  := as.numeric(timestamp_month == "2019-09-01")*treated] %>%
  .[, oct_2019Xtreated  := as.numeric(timestamp_month == "2019-10-01")*treated] %>%
  .[, nov_2019Xtreated  := as.numeric(timestamp_month == "2019-11-01")*treated] %>%
  .[, dec_2019Xtreated  := as.numeric(timestamp_month == "2019-12-01")*treated] %>%
  .[, jan_2020Xtreated  := as.numeric(timestamp_month == "2020-01-01")*treated] %>%
  .[, feb_2020Xtreated  := as.numeric(timestamp_month == "2020-02-01")*treated] %>%
  .[, mar_2020Xtreated  := as.numeric(timestamp_month == "2020-03-01")*treated] %>%
  .[, apr_2020Xtreated  := as.numeric(timestamp_month == "2020-04-01")*treated] %>%
  .[, may_2020Xtreated  := as.numeric(timestamp_month == "2020-05-01")*treated] %>%
  .[, june_2020Xtreated := as.numeric(timestamp_month == "2020-06-01")*treated] %>%
  .[, july_2020Xtreated := as.numeric(timestamp_month == "2020-07-01")*treated] %>%
  .[, aug_2020Xtreated  := as.numeric(timestamp_month == "2020-08-01")*treated] %>%
  .[, sep_2020Xtreated  := as.numeric(timestamp_month == "2020-09-01")*treated] %>%
  .[, oct_2020Xtreated  := as.numeric(timestamp_month == "2020-10-01")*treated] %>%
  .[, nov_2020Xtreated  := as.numeric(timestamp_month == "2020-11-01")*treated] %>%
  .[, dec_2020Xtreated  := as.numeric(timestamp_month == "2020-12-01")*treated] %>%
  .[, jan_2021Xtreated  := as.numeric(timestamp_month == "2021-01-01")*treated] %>%
  .[, feb_2021Xtreated  := as.numeric(timestamp_month == "2021-02-01")*treated] %>%
  .[, mar_2021Xtreated  := as.numeric(timestamp_month == "2021-03-01")*treated] %>%
  .[, apr_2021Xtreated  := as.numeric(timestamp_month == "2021-04-01")*treated] %>%
  .[, may_2021Xtreated  := as.numeric(timestamp_month == "2021-05-01")*treated] %>%
  .[, june_2021Xtreated := as.numeric(timestamp_month == "2021-06-01")*treated] %>%
  .[, july_2021Xtreated := as.numeric(timestamp_month == "2021-07-01")*treated] %>%
  .[, aug_2021Xtreated  := as.numeric(timestamp_month == "2021-08-01")*treated] %>%
  .[, sep_2021Xtreated  := as.numeric(timestamp_month == "2021-09-01")*treated] %>% 
  .[, oct_2021Xtreated  := as.numeric(timestamp_month == "2021-10-01")*treated] %>% 
  .[, nov_2021Xtreated  := as.numeric(timestamp_month == "2021-11-01")*treated] %>% 
  .[, dec_2021Xtreated  := as.numeric(timestamp_month == "2021-12-01")*treated] %>% 
  .[, jan_2022Xtreated  := as.numeric(timestamp_month == "2022-01-01")*treated] %>% 
  .[, feb_2022Xtreated  := as.numeric(timestamp_month == "2022-02-01")*treated] %>% 
  .[, mar_2022Xtreated  := as.numeric(timestamp_month == "2022-03-01")*treated] %>% 
  .[, apr_2022Xtreated  := as.numeric(timestamp_month == "2022-04-01")*treated] %>% 
  .[, may_2022Xtreated  := as.numeric(timestamp_month == "2022-05-01")*treated] %>% 
  .[, jun_2022Xtreated  := as.numeric(timestamp_month == "2022-06-01")*treated] %>% 
  .[, jul_2022Xtreated  := as.numeric(timestamp_month == "2022-07-01")*treated] %>% 
  .[, aug_2022Xtreated  := as.numeric(timestamp_month == "2022-08-01")*treated] %>% 
  .[, sep_2022Xtreated  := as.numeric(timestamp_month == "2022-09-01")*treated] %>% 
  .[, oct_2022Xtreated  := as.numeric(timestamp_month == "2022-10-01")*treated]

daily_to_monthly %<>%
  .[, july_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-07-01")*treated*fee_2.75] %>%
  .[, aug_2019XtreatedXfee_2.75  := as.numeric(timestamp_month == "2019-08-01")*treated*fee_2.75] %>%
  .[, sep_2019XtreatedXfee_2.75  := as.numeric(timestamp_month == "2019-09-01")*treated*fee_2.75] %>%
  .[, oct_2019XtreatedXfee_2.75  := as.numeric(timestamp_month == "2019-10-01")*treated*fee_2.75] %>%
  .[, nov_2019XtreatedXfee_2.75  := as.numeric(timestamp_month == "2019-11-01")*treated*fee_2.75] %>%
  .[, dec_2019XtreatedXfee_2.75  := as.numeric(timestamp_month == "2019-12-01")*treated*fee_2.75] %>%
  .[, jan_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-01-01")*treated*fee_2.75] %>%
  .[, feb_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-02-01")*treated*fee_2.75] %>%
  .[, mar_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-03-01")*treated*fee_2.75] %>%
  .[, apr_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-04-01")*treated*fee_2.75] %>%
  .[, may_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-05-01")*treated*fee_2.75] %>%
  .[, june_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-06-01")*treated*fee_2.75] %>%
  .[, july_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-07-01")*treated*fee_2.75] %>%
  .[, aug_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-08-01")*treated*fee_2.75] %>%
  .[, sep_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-09-01")*treated*fee_2.75] %>%
  .[, oct_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-10-01")*treated*fee_2.75] %>%
  .[, nov_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-11-01")*treated*fee_2.75] %>%
  .[, dec_2020XtreatedXfee_2.75  := as.numeric(timestamp_month == "2020-12-01")*treated*fee_2.75] %>%
  .[, jan_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-01-01")*treated*fee_2.75] %>%
  .[, feb_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-02-01")*treated*fee_2.75] %>%
  .[, mar_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-03-01")*treated*fee_2.75] %>%
  .[, apr_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-04-01")*treated*fee_2.75] %>%
  .[, may_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-05-01")*treated*fee_2.75] %>%
  .[, june_2021XtreatedXfee_2.75 := as.numeric(timestamp_month == "2021-06-01")*treated*fee_2.75] %>%
  .[, july_2021XtreatedXfee_2.75 := as.numeric(timestamp_month == "2021-07-01")*treated*fee_2.75] %>%
  .[, aug_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-08-01")*treated*fee_2.75] %>%
  .[, sep_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-09-01")*treated*fee_2.75] %>% 
  .[, oct_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-10-01")*treated*fee_2.75] %>% 
  .[, nov_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-11-01")*treated*fee_2.75] %>% 
  .[, dec_2021XtreatedXfee_2.75  := as.numeric(timestamp_month == "2021-12-01")*treated*fee_2.75] %>% 
  .[, jan_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-01-01")*treated*fee_2.75] %>% 
  .[, feb_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-02-01")*treated*fee_2.75] %>% 
  .[, mar_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-03-01")*treated*fee_2.75] %>% 
  .[, apr_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-04-01")*treated*fee_2.75] %>% 
  .[, may_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-05-01")*treated*fee_2.75] %>% 
  .[, jun_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-06-01")*treated*fee_2.75] %>% 
  .[, jul_2022XXtreatedXfee_2.75 := as.numeric(timestamp_month == "2022-07-01")*treated*fee_2.75] %>% 
  .[, aug_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-08-01")*treated*fee_2.75] %>% 
  .[, sep_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-09-01")*treated*fee_2.75] %>% 
  .[, oct_2022XtreatedXfee_2.75  := as.numeric(timestamp_month == "2022-10-01")*treated*fee_2.75]

# (1.8): Export daily to monthly.
qsave(daily_to_monthly, here("proc", "fintech_fee_dtm_dataupdate.qs"))

daily_to_monthly_original <- daily_to_monthly %>% 
  filter(timestamp_month <= "2021-03-01") %>% 
  select(all_of(colnames(daily_to_monthly)[!str_detect(colnames(daily_to_monthly), 
                                                       paste0("mar_2021|apr_2021|may_2021|june_2021|july_2021|aug_2021|sep_2021|oct_2021|",
                                                              "nov_2021|dec_2021|jan_2022|feb_2022|mar_2021|apr_2021|may_2021|june_2021"))]))
qsave(daily_to_monthly_original, here("proc", "fintech_fee_dtm.qs"))

rm(daily_to_monthly, daily_to_monthly_original)