#--------------------------------------------------------------------------------------------
# File name: 		      06a_createdaily_panel.R
# Creation date:      2022-02-08
# Author:          		Noah Forougi and César Landín
# Files used:
# 	- here("proc", "fintech_fee_light.csv")
# 	- here("proc", "users.rds")
# 	- here("data", "Sent_20210202", "02022021", "daily_02022021.csv")
# 	- here("data", "Sent_20210122", "Archive_22012021", "daily.csv")
# 	- here("data", "Sent_20201202", "daily", "daily.csv")
# 	- here("data", "Sent_20210406", "daily_20200406.csv")
# 	- here("data", "Sent_20210622", "bq-results-20210622-120549-378weo6d45d0.csv")
# 	- here("data", "Sent_20210907", "bq-results-20210907-093808-j5m7o43y2wi1.csv")
# 	- here("data", "Sent_20210906", "bq-results-20210906-102221-a0n8jfdtxzwd.csv")
# 	- here("data", "Sent_20211005", "daily_activity_092021.csv")
# 	- here("data", "Sent_20211102", "bq-results-20211102-191052-ft0ldw5edhtm.csv")
# 	- here("data", "Sent_20220624", "transactions 20211101 to 20220621.csv")
# 	- Files in here("data", "Sent_20221111")
# Files created:
# 	- here("proc", "fintech_fee_daily.qs"))
# Purpose:
# 	- Create daily panel dataset of transactions and sales volumes.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(data.table)
library(qs)
library(assertthat)
library(lubridate)
library(magrittr)
source(file = here("scripts", "programs", "myfunctions.R"))
source(file = here("scripts", "programs", "winsorize.R"))
#########################################################

######################################################
##    (1): Import and combine all daily datasets.   ##
######################################################
# (1.1): Import Fintech treatment assignment and users.
fintech_fee <- fread(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, treat_type, treat_description, group_id, 
         accepted_offer_firstday, accepted_offer_ontime, accepted_offer_late, 
         takeup_date, fee_type)
users <- read_rds(here("proc", "users.rds")) %>% select(organization_uuid, created_date, first_payment_date)

# (1.2): Import daily data.
daily_2021 <- fread(here("data", "Sent_20210202", "02022021", "daily_02022021.csv")) %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid))

daily_nov <- fread(here("data", "Sent_20210122", "Archive_22012021", "daily.csv")) %>% 
  filter(organization_uuid %chin% fintech_fee$organization_uuid)  %>%
  filter(timestamp_day < ymd("2021-01-01"))

daily_first <- fread(here("data", "Sent_20201202", "daily", "daily.csv")) %>% 
  filter(organization_uuid %chin% fintech_fee$organization_uuid) %>% 
  filter(timestamp_day < ymd("2020-11-01"))

daily_febmarch <- fread(here("data", "Sent_20210406", "daily_20200406.csv")) %>% 
  filter(organization_uuid %chin% fintech_fee$organization_uuid) %>%
  filter(timestamp_day < ymd("2021-04-01"))

daily_aprmay <- fread(here("data", "Sent_20210622", "bq-results-20210622-120549-378weo6d45d0.csv"))  %>%
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid)) %>% 
  filter(timestamp_day < ymd("2021-06-01"))

daily_june <- fread(here("data", "Sent_20210907", "bq-results-20210907-093808-j5m7o43y2wi1.csv")) %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid))

daily_julyaug <- fread(here("data", "Sent_20210906", "bq-results-20210906-102221-a0n8jfdtxzwd.csv")) %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid))

daily_september <- fread(here("data", "Sent_20211005", "daily_activity_092021.csv")) %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid))

daily_october <- fread(here("data", "Sent_20211102", "bq-results-20211102-191052-ft0ldw5edhtm.csv")) %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid))

daily_2022_1 <- fread(here("data", "Sent_20220624", "transactions 20211101 to 20220621.csv")) %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid)) %>% 
  filter(timestamp_day < "2022-05-01")

files_2022_2 <- list.files(here("data", "Sent_20221111")) %>% .[.!= "summary table.csv"]
daily_2022_2 <- lapply(files_2022_2, function(x) fread(here("data", "Sent_20221111", x))) %>% 
  bind_rows() %>% 
  filter(organization_uuid %in% unique(fintech_fee$organization_uuid))

# (1.3): Review data dates.
monthly_dataset_dir <- c("daily_first", "daily_nov", "daily_2021", "daily_febmarch", 
                         "daily_aprmay", "daily_june", "daily_julyaug", "daily_september", "daily_october",
                         "daily_2022_1", "daily_2022_2")
for (data in monthly_dataset_dir) {
  print("****************************************")
  print(paste0("Working on data ", data))
  print(paste0("Number of unique businesses: ",
               eval(as.name(data)) %>% 
                 select(organization_uuid) %>% 
                 unique() %>% 
                 nrow()))
  print(paste0("Minimum date: ",
               eval(as.name(data)) %>% 
                 pull(timestamp_day) %>% 
                 min()))
  print(paste0("Maximum date: ",
               eval(as.name(data)) %>% 
                 pull(timestamp_day) %>% 
                 max()))
}

# (1.4): Test no duplicates.
for (data in monthly_dataset_dir) {
  print(paste0("Check that file ", data, " has no duplicates."))
  test <- copy(eval(as.name(data))) %>% .[, keyby = .(organization_uuid, timestamp_day), .N]
  print(assert_that(all(test$N == 1)))
}

# (1.5): Bind all daily datasets and get rid of negative refund volume.
daily <- lapply(monthly_dataset_dir, as.name) %>% 
  lapply(eval) %>% 
  bind_rows() %>% 
  transmute(organization_uuid, 
            timestamp_day,
            nr_valid_payments, 
            valid_volume = ifelse(valid_volume < 0, 0, valid_volume))
rm(list = monthly_dataset_dir)

####################################
##    (2): Process daily panel.   ##
####################################
# (2.1): Create complete firm-day level panel.
daily <- setDT(daily)
daily <- daily[CJ(organization_uuid = unique(daily$organization_uuid), 
                  timestamp_day = unique(daily$timestamp_day)),
               on=c("organization_uuid", "timestamp_day")]
# Confirm we an observation for each firm-day.
test <- daily[, keyby = .(timestamp_day), .N]
assert_that(all(test$N==length(unique(fintech_fee$organization_uuid))))
rm(test)

# (2.2): Remove observations where the timestamp_day is older than when they first created.
daily <- users[daily, on = "organization_uuid"]
assert_that(sum(is.na(daily$created_date)) == 0)
daily <- daily[timestamp_day >= created_date]

# (2.3): Convert the NAs in valid_volume and nr_valid_payments to 0, because these are observations after the firm had the technology.
daily[, valid_volume := ifelse(is.na(valid_volume), 0, valid_volume)]
daily[, nr_valid_payments := ifelse(is.na(nr_valid_payments), 0, nr_valid_payments)]

# (2.4): Get most recent transaction and merge date in.
most_recent_usage <- daily[nr_valid_payments > 0 ] %>% 
  .[, keyby = .(organization_uuid), .(most_recent_use = max(timestamp_day))]
daily <- most_recent_usage[daily, on = "organization_uuid"]
daily[, active := ifelse(timestamp_day <= most_recent_use, 1, 0)]
rm(most_recent_usage)

# (2.5): Merge Zette identifying info.
fintech_fee <- setDT(fintech_fee)
daily <- fintech_fee[daily, on = "organization_uuid"]

# (2.6): Create outcome variables.
winsorized <- daily %>%
  select(organization_uuid, valid_volume, treat_type, timestamp_day) %>%
  winsorize(outcome = "valid_volume",
            newvar = "valid_volume_w5", 
            by = c("treat_type" , "timestamp_day"),
            highonly = TRUE)
daily %<>%
  select(!c(valid_volume)) %>%
  left_join(winsorized, by = c("organization_uuid", "treat_type", "timestamp_day"))
winsorized <- daily %>% 
  select(organization_uuid, nr_valid_payments, treat_type, timestamp_day) %>% 
  winsorize(outcome = "nr_valid_payments", 
            newvar  = "nr_valid_payments_w5",  
            by   = c("treat_type", "timestamp_day"),
            highonly = TRUE)
daily %<>%
  select(!c(nr_valid_payments)) %>%
  left_join(winsorized, by = c("organization_uuid", "treat_type", "timestamp_day"))
rm(winsorized)

daily <- setDT(daily) %>% 
  .[, asinh_sales := asinh(valid_volume)] %>% 
  .[, log_valid_volume := log(valid_volume + 1)] %>% 
  .[, asinh_nr_payments := asinh(nr_valid_payments)] %>% 
  .[, log_nr_payments := log(nr_valid_payments + 1)] %>% 
  .[, make_sale := as.numeric(nr_valid_payments > 0)] %>% 
  .[, adopted               := accepted_offer_late] %>% 
  .[, post                  := if_else(timestamp_day >= ymd(20200928), 1, 0)] %>% 
  .[, treated               := ifelse(treat_type %in% c("T2", "T3", "T4", "T5", "T6", "T7", "T8"), 1, 0)] %>% 
  .[, postXtreated          := post * treated] %>% 
  .[, postXadopted          := post * accepted_offer_late] %>% 
  .[, log_valid_volume      := log(valid_volume + 1)] %>% 
  .[, arcsinh_valid_volume  := asinh(valid_volume)] %>% 
  .[, fee_2.75              := if_else(fee_type == "2.75% offer", 1, 0, 0)] %>% # Code it such that control is 0 
  .[, adoptedXfee_2.75      := adopted * fee_2.75] %>% 
  .[, treatedXfee_2.75      := treated * fee_2.75] %>% 
  .[, daily                 := postXadopted * fee_2.75] %>% 
  .[, postXtreatedXfee_2.75 := postXtreated * fee_2.75] %>% 
  .[, make_sale             := as.numeric(nr_valid_payments > 0)] %>% 
  .[, log_nr_payments       := log(nr_valid_payments + 1)] %>% 
  .[, asinh_nr_payments     := asinh(nr_valid_payments)] %>% 
  .[, valid_volume_per_transaction := ifelse(nr_valid_payments != 0, valid_volume/nr_valid_payments, 0)] %>% 
  .[, log_valid_volume_per_transaction := log(valid_volume_per_transaction + 1)] %>% 
  .[, asinh_valid_volume_per_transaction := asinh(valid_volume_per_transaction)] %>% 
  .[, valid_volume_w5_per_transaction := ifelse(nr_valid_payments != 0, valid_volume_w5/nr_valid_payments_w5, 0)]

# (2.7): Save panel dataset.
qsave(daily, here("proc", "fintech_fee_daily.qs"))
