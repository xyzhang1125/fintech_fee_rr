#--------------------------------------------------------------------------------------------
# File name: 		      06d_cleanpanel.R
# Creation date:      2021-10-21
# Author:          		Noah Forougi & César Landín
# Files used:
#   - here("proc", "fintech_fee_light.csv")
#   - here("proc", "monthly_panel.rds")
#   - here("proc", "users.rds")
#   - here("data", "Sent_20201105", "base_test_mensual_sept_oct.csv")
#   - here("data", "Sent_20201202", "Archive", "monthly.csv")
#   - here("data", "Sent_20210122", "Archive_22012021", "monthly.csv")
#   - here("data", "Sent_20210202", "02022021", "monthly_02022021.csv")
#   - here("data", "Sent_20210406", "monthly_20200406.csv")
#   - here("data", "Sent_20201117", "base_test_semanal_20201115.csv")
#   - here("data", "Sent_20201202", "Archive", "weekly.csv")
#   - here("data", "Sent_20210122", "Archive_22012021", "weekly.csv")
#   - here("data", "Sent_20210202", "02022021", "weekly_02022021.csv")
#   - here("data", "Sent_20210406", "weekly_20200406.csv")
#   - here("data", "Sent_20210622", "bq-results-20210622-120549-378weo6d45d0.csv")
# Files created:
#   - here("proc", "fintech_fee_monthly.qs")
#   - here("proc", "fintech_fee_weekly.qs")
# Purpose:
# 	- Clean and append the datasets to create panel of sales.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(here)
library(tidyverse)
library(magrittr)
library(assertthat)
library(tabulator)
library(lubridate)
library(qs)
library(data.table)
conflicted::conflicts_prefer(data.table::year)
conflicted::conflicts_prefer(data.table::month)
conflicted::conflicts_prefer(data.table::week)
conflicted::conflicts_prefer(dplyr::filter)
source(here("scripts", "programs", "myfunctions.R"))
source(here("scripts", "programs", "winsorize.R"))
#########################################################

#################################################
##  (1): Import data and generate April data.  ##
#################################################
# (1.1): Read in data.
fintech_fee <- read_csv(here("proc",  "fintech_fee_light.csv")) 
monthly_panel <- read_rds(here("proc", "monthly_panel.rds")) %>% 
  mutate(timestamp_month = as_date(timestamp_month))
users <- readRDS(here("proc", "users.rds"))

# (1.2): Read in monthly data.
monthly_sepoct <- read_csv(here("data", "Sent_20201105", "base_test_mensual_sept_oct.csv"))
monthly_nov <-    read_csv(here("data", "Sent_20201202", "Archive","monthly.csv")) %>% filter(timestamp_month == "2020-11-01")
monthly_dec <- read_csv(here("data", "Sent_20210122", "Archive_22012021", "monthly.csv"))  %>% filter(timestamp_month == "2020-12-01")
monthly_jan <- read_csv(here("data", "Sent_20210202", "02022021", "monthly_02022021.csv"))
monthly_febmar <- read_csv(here("data", "Sent_20210406", "monthly_20200406.csv")) %>% filter(timestamp_month < "2021-04-01")

# (1.3): Read in weekly data.
weekly_julnov <- read_csv(here("data", "Sent_20201117", "base_test_semanal_20201115.csv")) # 2019-07-29 - 2020-11-09 
weekly_dec2update <- read_csv(here("data", "Sent_20201202", "Archive","weekly.csv")) # 2020-11-16 - 2020-11-23
weekly_jan22update <- read_csv(here("data", "Sent_20210122", "Archive_22012021", "weekly.csv")) %>% filter(timestamp_week > "2020-11-23" & timestamp_week <  "2021-01-04") # 2020-11-30 - 2020-12-28
weekly_feb02update <- read_csv(here("data", "Sent_20210202", "02022021", "weekly_02022021.csv")) # 2021-01-04 - 2021-01-25
weekly_apr06update <- read_csv(here("data", "Sent_20210406", "weekly_20200406.csv")) %>% filter(timestamp_week != "2021-04-05") # 2021-02-01 - 2021-03-29

# (1.4): Import data from April 2021 and generate date variables.
daily_data <- read_csv(here("data", "Sent_20210622", "bq-results-20210622-120549-378weo6d45d0.csv")) %>% 
  filter(timestamp_day < "2021-05-01") %>% 
  mutate(timestamp_month = ymd(paste0(year(timestamp_day), "-", month(timestamp_day), "-01")),
         timestamp_week = ymd("2021-01-04") + weeks(week(timestamp_day) - 1 )) %>%
  relocate(timestamp_week, .after = timestamp_day) %>%
  relocate(timestamp_month, .after = timestamp_day)


# (1.5): Sum April data by week and organization id.
weekly_apr <- daily_data %>%
  select(organization_uuid, timestamp_week, nr_valid_payments, valid_volume) %>%
  group_by(organization_uuid, timestamp_week) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>% 
  arrange(timestamp_week, organization_uuid)

# (1.6): Sum April data by month and organization id.
monthly_apr <- daily_data %>%
  select(organization_uuid, timestamp_month, nr_valid_payments, valid_volume) %>%
  group_by(organization_uuid, timestamp_month) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  ungroup() %>% 
  arrange(timestamp_month, organization_uuid)

# (1.7): Convert weekly datasets to data.table.
names_to_convert <- grep("weekly", ls(envir = .GlobalEnv), value = TRUE)
lapply(names_to_convert, function(x) assign(x, as.data.table(get(x)), envir = .GlobalEnv))

#######################################
##  (2): Create monthly panel data.  ##
#######################################
# (2.1): Keep subset of observations.
# In the monthly_dec dataset, we now have valid_volume and total_volume, where valid_volume  = total_volume - refunded_amount. 
# We want only the total volume and total number of payments.
monthly_dec %<>% transmute(organization_uuid, 
                           timestamp_month, 
                           nr_valid_payments,
                           valid_volume = ifelse(valid_volume < 0, 0, valid_volume))
monthly_jan %<>% transmute(organization_uuid, 
                           timestamp_month, 
                           nr_valid_payments,
                           valid_volume = ifelse(valid_volume < 0, 0, valid_volume))
monthly_febmar %<>% transmute(organization_uuid, 
                              timestamp_month, 
                              nr_valid_payments,
                              valid_volume = ifelse(valid_volume < 0, 0, valid_volume))

# (2.2): Bind all monthly datasets.
monthly <- monthly_panel %>%
  filter(organization_uuid %in% fintech_fee$organization_uuid) %>%
  select(organization_uuid, timestamp_month, nr_valid_payments, valid_volume) %>% 
  bind_rows(monthly_sepoct %>% filter(organization_uuid %in% fintech_fee$organization_uuid)) %>% 
  bind_rows(monthly_nov    %>% filter(organization_uuid %in% fintech_fee$organization_uuid)) %>%
  bind_rows(monthly_dec    %>% filter(organization_uuid %in% fintech_fee$organization_uuid)) %>%
  bind_rows(monthly_jan    %>% filter(organization_uuid %in% fintech_fee$organization_uuid)) %>% 
  bind_rows(monthly_febmar %>% filter(organization_uuid %in% fintech_fee$organization_uuid)) %>% 
  bind_rows(monthly_apr    %>% filter(organization_uuid %in% fintech_fee$organization_uuid))

# (2.3): Make sure we only have firms in our experiment.
assert_that(length(unique(monthly$organization_uuid)) == nrow(fintech_fee))

# (2.4): Complete the dataset.
monthly %<>% complete(organization_uuid, timestamp_month)

# (2.5): Check that we have an observation for each firm in each month.
monthly_obs <- monthly %>% 
  group_by(timestamp_month) %>%
  summarise(length(unique(organization_uuid)))
assert_that(all(monthly_obs$`length(unique(organization_uuid))` == length(unique(fintech_fee$organization_uuid))))

# (2.6): Join with iZettle identifying info and treatment status.
monthly %<>%
  left_join(., fintech_fee, by = "organization_uuid")

# (2.7): Filter to only have observations after they joined the technology.
monthly %<>% filter(timestamp_month >= start_date)

# (2.8): Clean valid volume and payments variables.
# The complete() function creates NAs for observations not originally there. 
# Change NA to zero because these firms have the technology in these months, they just choose not to use it
monthly %<>%
  mutate(nr_valid_payments = ifelse(is.na(nr_valid_payments), 0, nr_valid_payments), 
         valid_volume = ifelse(is.na(valid_volume), 0, valid_volume))

# (2.9): Create variables for the regressions.
# Create ITT and TOT variables 
monthly <- setDT(monthly)
monthly[, adopted := accepted_offer_late]
monthly[, post                  := if_else(timestamp_month >= ymd("2020-09-01"), 1, 0)]
monthly[, treated               := ifelse(treat_type %in% c("T2", "T3", "T4", "T5", "T6", "T7", "T8"), 1, 0)]
monthly[, postXtreated          := post*treated]
monthly[, postXadopted          := post*accepted_offer_late]
monthly[, log_valid_volume := log1p(valid_volume)]
monthly[, arcsinh_valid_volume := asinh(valid_volume)]
monthly[,fee_2.75              := if_else(fee_type == "2.75% offer", 1, 0,0)] # Code it such that control is 0 
monthly[, adoptedXfee_2.75 := adopted * fee_2.75]
monthly[, treatedXfee_2.75 := treated * fee_2.75]
monthly[,postXadoptedXfee_2.75 := postXadopted * fee_2.75]
monthly[,postXtreatedXfee_2.75 := postXtreated * fee_2.75]
monthly[,make_sale := as.numeric(nr_valid_payments > 0)]
monthly[,log_nr_payments := log(nr_valid_payments + 1)]
monthly[,asinh_nr_payments := asinh(nr_valid_payments)]

monthly %<>% winsorize(outcome = "valid_volume", 
                       newvar  = "valid_volume_w5",  
                       by   = c("group_id", "timestamp_month"),
                       highonly = TRUE)
monthly %<>% winsorize(outcome = "nr_valid_payments", 
                       newvar  = "nr_valid_payments_w5",  
                       by   = c("group_id", "timestamp_month"),
                       highonly = TRUE)

monthly[,valid_volume_per_transaction := ifelse(nr_valid_payments != 0, valid_volume/nr_valid_payments, 0)]
monthly[,valid_volume_w5_per_transaction := ifelse(nr_valid_payments != 0, valid_volume_w5/nr_valid_payments_w5, 0)]
monthly[,log_valid_volume_per_transaction := log(valid_volume_per_transaction + 1)]
monthly[,asinh_valid_volume_per_transaction := asinh(valid_volume_per_transaction)]

# (2.10): Create monthly indicators.
monthly[, july_2019 := as.numeric(timestamp_month == "2019-07-01")]
monthly[, aug_2019  := as.numeric(timestamp_month == "2019-08-01")]
monthly[, sep_2019  := as.numeric(timestamp_month == "2019-09-01")]
monthly[, oct_2019  := as.numeric(timestamp_month == "2019-10-01")]
monthly[, nov_2019  := as.numeric(timestamp_month == "2019-11-01")]
monthly[, dec_2019  := as.numeric(timestamp_month == "2019-12-01")]
monthly[, jan_2020  := as.numeric(timestamp_month == "2020-01-01")]
monthly[, feb_2020  := as.numeric(timestamp_month == "2020-02-01")]
monthly[, mar_2020  := as.numeric(timestamp_month == "2020-03-01")]
monthly[, apr_2020  := as.numeric(timestamp_month == "2020-04-01")]
monthly[, may_2020  := as.numeric(timestamp_month == "2020-05-01")]
monthly[, june_2020 := as.numeric(timestamp_month == "2020-06-01")]
monthly[, july_2020 := as.numeric(timestamp_month == "2020-07-01")]
monthly[, aug_2020  := as.numeric(timestamp_month == "2020-08-01")]
monthly[, sep_2020  := as.numeric(timestamp_month == "2020-09-01")]
monthly[, oct_2020  := as.numeric(timestamp_month == "2020-10-01")]
monthly[, nov_2020  := as.numeric(timestamp_month == "2020-11-01")]
monthly[, dec_2020  := as.numeric(timestamp_month == "2020-12-01")]
monthly[, jan_2021  := as.numeric(timestamp_month == "2021-01-01")]
monthly[, feb_2021  := as.numeric(timestamp_month == "2021-02-01")]
monthly[, mar_2021  := as.numeric(timestamp_month == "2021-03-01")]
monthly[, apr_2021  := as.numeric(timestamp_month == "2021-04-01")]

# (2.11): Create monthly interactions.
monthly[, july_2019Xadopted := as.numeric(timestamp_month == "2019-07-01")*adopted]
monthly[,  aug_2019Xadopted := as.numeric(timestamp_month == "2019-08-01")*adopted]
monthly[,  sep_2019Xadopted := as.numeric(timestamp_month == "2019-09-01")*adopted]
monthly[,  oct_2019Xadopted := as.numeric(timestamp_month == "2019-10-01")*adopted]
monthly[,  nov_2019Xadopted := as.numeric(timestamp_month == "2019-11-01")*adopted]
monthly[,  dec_2019Xadopted := as.numeric(timestamp_month == "2019-12-01")*adopted]
monthly[,  jan_2020Xadopted := as.numeric(timestamp_month == "2020-01-01")*adopted]
monthly[,  feb_2020Xadopted := as.numeric(timestamp_month == "2020-02-01")*adopted]
monthly[,  mar_2020Xadopted := as.numeric(timestamp_month == "2020-03-01")*adopted]
monthly[,  apr_2020Xadopted := as.numeric(timestamp_month == "2020-04-01")*adopted]
monthly[,  may_2020Xadopted := as.numeric(timestamp_month == "2020-05-01")*adopted]
monthly[, june_2020Xadopted := as.numeric(timestamp_month == "2020-06-01")*adopted]
monthly[, july_2020Xadopted := as.numeric(timestamp_month == "2020-07-01")*adopted]
monthly[,  aug_2020Xadopted := as.numeric(timestamp_month == "2020-08-01")*adopted]
monthly[,  sep_2020Xadopted := as.numeric(timestamp_month == "2020-09-01")*adopted]
monthly[,  oct_2020Xadopted := as.numeric(timestamp_month == "2020-10-01")*adopted]
monthly[,  nov_2020Xadopted := as.numeric(timestamp_month == "2020-11-01")*adopted]
monthly[,  dec_2020Xadopted := as.numeric(timestamp_month == "2020-12-01")*adopted]
monthly[,  jan_2021Xadopted := as.numeric(timestamp_month == "2021-01-01")*adopted]
monthly[,  feb_2021Xadopted := as.numeric(timestamp_month == "2021-02-01")*adopted]
monthly[,  mar_2021Xadopted := as.numeric(timestamp_month == "2021-03-01")*adopted]
monthly[,  apr_2021Xadopted := as.numeric(timestamp_month == "2021-04-01")*adopted]

monthly[, july_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-07-01")*adopted*fee_2.75]
monthly[,  aug_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-08-01")*adopted*fee_2.75]
monthly[,  sep_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-09-01")*adopted*fee_2.75]
monthly[,  oct_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-10-01")*adopted*fee_2.75]
monthly[,  nov_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-11-01")*adopted*fee_2.75]
monthly[,  dec_2019XadoptedXfee_2.75 := as.numeric(timestamp_month == "2019-12-01")*adopted*fee_2.75]
monthly[,  jan_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-01-01")*adopted*fee_2.75]
monthly[,  feb_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-02-01")*adopted*fee_2.75]
monthly[,  mar_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-03-01")*adopted*fee_2.75]
monthly[,  apr_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-04-01")*adopted*fee_2.75]
monthly[,  may_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-05-01")*adopted*fee_2.75]
monthly[, june_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-06-01")*adopted*fee_2.75]
monthly[, july_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-07-01")*adopted*fee_2.75]
monthly[,  aug_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-08-01")*adopted*fee_2.75]
monthly[,  sep_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-09-01")*adopted*fee_2.75]
monthly[,  oct_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-10-01")*adopted*fee_2.75]
monthly[,  nov_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-11-01")*adopted*fee_2.75]
monthly[,  dec_2020XadoptedXfee_2.75 := as.numeric(timestamp_month == "2020-12-01")*adopted*fee_2.75]
monthly[,  jan_2021XadoptedXfee_2.75 := as.numeric(timestamp_month == "2021-01-01")*adopted*fee_2.75]
monthly[,  feb_2021XadoptedXfee_2.75 := as.numeric(timestamp_month == "2021-02-01")*adopted*fee_2.75]
monthly[,  mar_2021XadoptedXfee_2.75 := as.numeric(timestamp_month == "2021-03-01")*adopted*fee_2.75]
monthly[,  apr_2021XadoptedXfee_2.75 := as.numeric(timestamp_month == "2021-04-01")*adopted*fee_2.75]

monthly[, july_2019Xtreated := as.numeric(timestamp_month == "2019-07-01")*treated]
monthly[,  aug_2019Xtreated := as.numeric(timestamp_month == "2019-08-01")*treated]
monthly[,  sep_2019Xtreated := as.numeric(timestamp_month == "2019-09-01")*treated]
monthly[,  oct_2019Xtreated := as.numeric(timestamp_month == "2019-10-01")*treated]
monthly[,  nov_2019Xtreated := as.numeric(timestamp_month == "2019-11-01")*treated]
monthly[,  dec_2019Xtreated := as.numeric(timestamp_month == "2019-12-01")*treated]
monthly[,  jan_2020Xtreated := as.numeric(timestamp_month == "2020-01-01")*treated]
monthly[,  feb_2020Xtreated := as.numeric(timestamp_month == "2020-02-01")*treated]
monthly[,  mar_2020Xtreated := as.numeric(timestamp_month == "2020-03-01")*treated]
monthly[,  apr_2020Xtreated := as.numeric(timestamp_month == "2020-04-01")*treated]
monthly[,  may_2020Xtreated := as.numeric(timestamp_month == "2020-05-01")*treated]
monthly[, june_2020Xtreated := as.numeric(timestamp_month == "2020-06-01")*treated]
monthly[, july_2020Xtreated := as.numeric(timestamp_month == "2020-07-01")*treated]
monthly[,  aug_2020Xtreated := as.numeric(timestamp_month == "2020-08-01")*treated]
monthly[,  sep_2020Xtreated := as.numeric(timestamp_month == "2020-09-01")*treated]
monthly[,  oct_2020Xtreated := as.numeric(timestamp_month == "2020-10-01")*treated]
monthly[,  nov_2020Xtreated := as.numeric(timestamp_month == "2020-11-01")*treated]
monthly[,  dec_2020Xtreated := as.numeric(timestamp_month == "2020-12-01")*treated]
monthly[,  jan_2021Xtreated := as.numeric(timestamp_month == "2021-01-01")*treated]
monthly[,  feb_2021Xtreated := as.numeric(timestamp_month == "2021-02-01")*treated]
monthly[,  mar_2021Xtreated := as.numeric(timestamp_month == "2021-03-01")*treated]
monthly[,  apr_2021Xtreated := as.numeric(timestamp_month == "2021-04-01")*treated]

monthly[, july_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-07-01")*treated*fee_2.75]
monthly[,  aug_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-08-01")*treated*fee_2.75]
monthly[,  sep_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-09-01")*treated*fee_2.75]
monthly[,  oct_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-10-01")*treated*fee_2.75]
monthly[,  nov_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-11-01")*treated*fee_2.75]
monthly[,  dec_2019XtreatedXfee_2.75 := as.numeric(timestamp_month == "2019-12-01")*treated*fee_2.75]
monthly[,  jan_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-01-01")*treated*fee_2.75]
monthly[,  feb_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-02-01")*treated*fee_2.75]
monthly[,  mar_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-03-01")*treated*fee_2.75]
monthly[,  apr_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-04-01")*treated*fee_2.75]
monthly[,  may_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-05-01")*treated*fee_2.75]
monthly[, june_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-06-01")*treated*fee_2.75]
monthly[, july_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-07-01")*treated*fee_2.75]
monthly[,  aug_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-08-01")*treated*fee_2.75]
monthly[,  sep_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-09-01")*treated*fee_2.75]
monthly[,  oct_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-10-01")*treated*fee_2.75]
monthly[,  nov_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-11-01")*treated*fee_2.75]
monthly[,  dec_2020XtreatedXfee_2.75 := as.numeric(timestamp_month == "2020-12-01")*treated*fee_2.75]
monthly[,  jan_2021XtreatedXfee_2.75 := as.numeric(timestamp_month == "2021-01-01")*treated*fee_2.75]
monthly[,  feb_2021XtreatedXfee_2.75 := as.numeric(timestamp_month == "2021-02-01")*treated*fee_2.75]
monthly[,  mar_2021XtreatedXfee_2.75 := as.numeric(timestamp_month == "2021-03-01")*treated*fee_2.75]
monthly[,  apr_2021XtreatedXfee_2.75 := as.numeric(timestamp_month == "2021-04-01")*treated*fee_2.75]

######################################
##  (3): Create weekly panel data.  ##
######################################
# (3.1): Ensure that for each week we have only one observation per firm.
weeklycount <- weekly_julnov[, keyby = .(organization_uuid, timestamp_week),
                             .(count      = .N)] 

assert_that(all(weeklycount$count == 1))

weekly_dec2update <- setDT(weekly_dec2update)
weeklycount_dec2update <- weekly_dec2update[, keyby = .(organization_uuid, timestamp_week),
                                            .(count      = .N)] 
assert_that(all(weeklycount_dec2update$count == 1))


weekly_jan22update <- setDT(weekly_jan22update)
weeklycount_jan22update <- weekly_jan22update[, keyby = .(organization_uuid, timestamp_week),
                                            .(count      = .N)] 
assert_that(all(weeklycount_jan22update$count == 1))

weekly_feb02update <- setDT(weekly_feb02update)
weeklycount_feb02update <- weekly_feb02update[, keyby = .(organization_uuid, timestamp_week),
                                              .(count      = .N)] 
assert_that(all(weeklycount_feb02update$count == 1))

weekly_apr06update <- setDT(weekly_apr06update)
weeklycount_apr06update <- weekly_apr06update[, keyby = .(organization_uuid, timestamp_week),
                                              .(count      = .N)] 
assert_that(all(weeklycount_feb02update$count == 1))


weekly_apr <- setDT(weekly_apr)
weeklycount_apr <- weekly_apr[, keyby = .(organization_uuid, timestamp_week),
                                              .(count      = .N)] 
assert_that(all(weeklycount_apr$count == 1))

# (3.2): Keep subset of variables.
weekly_jan22update  %<>% transmute(organization_uuid, 
                                   timestamp_week, 
                                   nr_valid_payments,
                                   valid_volume = ifelse(valid_volume < 0, 0, valid_volume))

weekly_feb02update  %<>%  transmute(organization_uuid, 
                                    timestamp_week, 
                                    nr_valid_payments,
                                    valid_volume = ifelse(valid_volume < 0, 0, valid_volume))

weekly_apr06update  %<>%  transmute(organization_uuid, 
                                    timestamp_week, 
                                    nr_valid_payments,
                                    valid_volume = ifelse(valid_volume < 0, 0, valid_volume))

weekly_apr  %<>%  transmute(organization_uuid, 
                            timestamp_week, 
                            nr_valid_payments,
                            valid_volume = ifelse(valid_volume < 0, 0, valid_volume))

# (3.3): Append the datasets together.
weekly_julnov[, timestamp_week := ymd(timestamp_week)]
weekly_jan22update[, timestamp_week := ymd(timestamp_week)]
weekly <- bind_rows(weekly_julnov, weekly_dec2update, weekly_jan22update, weekly_feb02update, weekly_apr06update, weekly_apr)

# (3.4): Now, take only firms in the experiment and filter based on when they started using the technology.
length(unique(weekly$organization_uuid))
weekly <- weekly[organization_uuid %in% fintech_fee$organization_uuid]
length(unique(weekly$organization_uuid))

# (3.5): Complete the dataset, so we have observations for each firm-week combination.
weekly %<>%  
  complete(organization_uuid, timestamp_week) %>%
  left_join(., users %>% select(organization_uuid, created_date), by = "organization_uuid")

# (3.6): Confirm correct number of observations.
weekly_obs <- weekly %>% 
  group_by(timestamp_week) %>% 
  count() # We have 33978 observations for each week 
assertthat::assert_that(all(weekly_obs$n == nrow(fintech_fee)))

# (3.7): Update timestamp week variable.
# We want only observations where the timestamp is after they created an account. Timestamp_week is the START of the week.
# We need to change the timestamp week to the end of the week to create the filter. 
weekly %<>%
  mutate(timestamp_week_end = ymd(timestamp_week) + 7) %>% 
  filter(timestamp_week_end >= ymd(created_date)) %>%
  select(!timestamp_week_end)

# (3.8): Change NA to zero because these firms have the technology in these months, they just choose not to use it.
weekly %<>%
  mutate(nr_valid_payments = ifelse(is.na(nr_valid_payments), 0, nr_valid_payments), 
         valid_volume = ifelse(is.na(valid_volume), 0, valid_volume))
weekly_obs <- weekly %>% 
  group_by(timestamp_week) %>% 
  count() #%>% View
assert_that(max(weekly_obs$n) <= nrow(fintech_fee))

# (3.9): Join with experiment data.
weekly %<>%
  left_join(., fintech_fee, "organization_uuid")

# (3.10): Create variables for regressions.
weekly <- setDT(weekly)
weekly[, adopted := accepted_offer_late]
weekly[, post                  := if_else(timestamp_week >= ymd(20200928), 1, 0)]
weekly[, treated               := ifelse(treat_type %in% c("T2", "T3", "T4", "T5", "T6", "T7", "T8"), 1, 0)]
weekly[, postXtreated          := post*treated]
weekly[, postXadopted          := post*accepted_offer_late]
weekly[, log_valid_volume      := log1p(valid_volume)]
weekly[, arcsinh_valid_volume  := asinh(valid_volume)]
weekly[, fee_2.75              := if_else(fee_type == "2.75% offer", 1, 0,0)] # Code it such that control is 0 
weekly[, treatedXfee_2.75      := treated*fee_2.75]
weekly[, adoptedXfee_2.75      := adopted*fee_2.75]
weekly[, postXadoptedXfee_2.75 := postXadopted * fee_2.75]
weekly[, postXtreatedXfee_2.75 := postXtreated * fee_2.75]
weekly[, make_sale             := as.numeric(nr_valid_payments > 0)]
weekly[, log_nr_payments       := log(nr_valid_payments + 1)]
weekly[, asinh_nr_payments     := asinh(nr_valid_payments)]
weekly[, valid_volume_per_transaction       := ifelse(nr_valid_payments != 0, valid_volume/nr_valid_payments, 0)]
weekly[, log_valid_volume_per_transaction   := log(valid_volume_per_transaction + 1)]
weekly[, asinh_valid_volume_per_transaction := asinh(valid_volume_per_transaction)]

# Create winsorized sales
weekly_w <- weekly %>% select(organization_uuid, valid_volume, group_id, timestamp_week) 
weekly_w %<>% 
  winsorize(outcome = "valid_volume", 
            newvar  = "valid_volume_w5",  
            by   = c("group_id", "timestamp_week"),
            highonly = TRUE) %>%
  select(organization_uuid, timestamp_week, valid_volume_w5)

weekly <- setDT(weekly)
weekly_w <- setDT(weekly_w)

weekly <- weekly_w[weekly, on =c("organization_uuid", "timestamp_week")]

# Create winsorized payments
weekly_wp <- weekly %>% select(organization_uuid, nr_valid_payments, group_id, timestamp_week) 
weekly_wp %<>% 
  winsorize(outcome = "nr_valid_payments", 
            newvar  = "nr_valid_payments_w5",  
            by   = c("group_id", "timestamp_week"),
            highonly = TRUE) %>%
  select(organization_uuid, timestamp_week, nr_valid_payments_w5)

weekly <- setDT(weekly)
weekly_wp <- setDT(weekly_wp)

weekly <- weekly_wp[weekly, on =c("organization_uuid", "timestamp_week")]

# Create winsorized sales per transaction
weekly[,valid_volume_w5_per_transaction := ifelse(nr_valid_payments != 0, valid_volume_w5/nr_valid_payments_w5, 0)]

# Create weekly identifiers
weeks <- unique(weekly$timestamp_week)
for (i in 1:length(weeks)) {
  var_name <- paste("week_", gsub("-", "_", weeks[i]), sep = "")
  weekly[, paste(var_name) := as.numeric(timestamp_week == paste(weeks[i]))]
}

# Create weekly interaction terms
weeks <- names(weekly)[grep("week_.*$", weekly %>% names())]
for (i in 1:length(weeks)) {
  var_name <- paste(weeks[i], "Xadopted", sep = "")
  weekly[, paste(var_name) := get(weeks[i])*adopted]
  
}

for (i in 1:length(weeks)) {
  var_name <- paste(weeks[i], "XadoptedXfee_2.75", sep = "")
  weekly[, paste(var_name) := get(weeks[i])*adopted*fee_2.75]
  
}

for (i in 1:length(weeks)) {
  var_name <- paste(weeks[i], "Xtreated", sep = "")
  weekly[, paste(var_name) := get(weeks[i])*treated]
}

for (i in 1:length(weeks)) {
  var_name <- paste(weeks[i], "XtreatedXfee_2.75", sep = "")
  weekly[, paste(var_name) := get(weeks[i])*treated*fee_2.75]
}

# (3.11):  Export panel data.
qs::qsave(monthly, here("proc", "fintech_fee_monthly.qs"))
qs::qsave(weekly,  here("proc", "fintech_fee_weekly.qs"))
