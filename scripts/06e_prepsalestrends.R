#--------------------------------------------------------------------------------------------
# File name: 		      06e_prepsalestrends
# Creation date:      2021-02-15
# Author:          		Noah Forougi and César Landín
# Files used:
# - here("proc", "fintech_fee_monthly.qs")
# - here("proc", "fintech_fee.csv")
# - here("proc", "fintech_fee_weekly.qs")
# Files created:
# - here("proc", "24a_monthly_sales.csv")
# - here("proc", "24a_monthly_sales_het.csv")
# - here("proc", "24a_weekly_sales.csv")
# - here("proc", "24a_weekly_sales_het.csv")
# Purpose:
# 	- Process sales trends by week and month.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(magrittr)
source(here("scripts", "programs", "winsorize.R"))
source(here("scripts", "programs", "myfunctions.R"))
#########################################################

########################################
##  (1): Create monthly time series.  ##
########################################
# (1.1): Read in data.
fintech_fee_monthly <- qs::qread(here("proc", "fintech_fee_monthly.qs"))
fintech_fee_monthly %>% select(organization_uuid) %>% unique() %>% count()
fintech_fee <- fread(here("proc", "fintech_fee.csv"))

# (1.2): Convert monthly data to data table.
fintech_fee_monthly <- setDT(fintech_fee_monthly)

# (1.3): Create outcome variables.
fintech_fee_monthly %<>% winsorize(outcome = "valid_volume", newvar  = "valid_volume_w5",  by      = c("group_id", "timestamp_month"))
fintech_fee_monthly[, asinh_sales := asinh(valid_volume)]
fintech_fee_monthly[, log_valid_volume := log(valid_volume + 1)]

# (1.4): # Create control group time series.
monthly_ctl_sales <- fintech_fee_monthly %>% 
  filter(treat_type == "T1") %>% 
  group_by(timestamp_month) %>%
  summarise(sales_w = mean(valid_volume_w5), 
            sales_log = mean(log_valid_volume), 
            sales_asinh = mean(asinh_sales), 
            sales_atall = mean(make_sale), 
            sales_reg = mean(valid_volume)) %>%
  mutate(group = "control") 

# (1.5): Create offer group time series.
monthly_offer_sales_het <- fintech_fee_monthly %>% 
  filter(treat_type != "T1") %>% 
  group_by(timestamp_month, fee_2.75) %>%
  summarise(sales_w = mean(valid_volume_w5), 
            sales_log = mean(log_valid_volume), 
            sales_asinh = mean(asinh_sales), 
            sales_atall = mean(make_sale), 
            sales_reg = mean(valid_volume)) %>%
  mutate(group = "offer")

monthly_offer_sales <- fintech_fee_monthly %>% 
  filter(treat_type != "T1") %>% 
  group_by(timestamp_month) %>%
  summarise(sales_w = mean(valid_volume_w5), 
            sales_log = mean(log_valid_volume), 
            sales_asinh = mean(asinh_sales), 
            sales_atall = mean(make_sale), 
            sales_reg = mean(valid_volume)) %>%
  mutate(group = "offer")

# (1.6): Bind datasets.
monthly_sales_het <- monthly_ctl_sales %>% 
  bind_rows(monthly_offer_sales_het) %>%
  mutate(fee_2.75 = if_else(fee_2.75 == 1, "fee2.75", "fee3.00", NA_character_)) %>%
  mutate(group = gsub("NA", "", paste0(group, fee_2.75))) %>%
  select(!fee_2.75) %>%
  pivot_wider(names_from = group, values_from = c("sales_w", "sales_log", "sales_asinh", "sales_atall", "sales_reg")) 

monthly_sales <- monthly_ctl_sales %>% 
  bind_rows(monthly_offer_sales) %>%
  pivot_wider(names_from = group, values_from = c("sales_w", "sales_log", "sales_asinh", "sales_atall", "sales_reg")) 

# (1.7): Get average monthly sales for "treated" group.
takeup <- mean(fintech_fee$accepted_offer_late[fintech_fee$treat_type != "T1"])
takeup_2.75 <- mean(fintech_fee$accepted_offer_late[fintech_fee$treat_type != "T1" & fintech_fee$fee_2.75 == 1])
takeup_3.00 <- mean(fintech_fee$accepted_offer_late[fintech_fee$treat_type != "T1" & fintech_fee$fee_2.75 == 0])

monthly_sales %<>% 
  mutate(sales_w_accepted = sales_w_control +  (sales_w_offer - sales_w_control)/takeup, 
         sales_log_accepted = sales_log_control +  (sales_log_offer - sales_log_control)/takeup, 
         sales_asinh_accepted = sales_asinh_control +  (sales_asinh_offer - sales_asinh_control)/takeup, 
         sales_atall_accepted = sales_atall_control + (sales_atall_offer - sales_atall_control)/takeup, 
         sales_reg_accepted = sales_reg_control + (sales_reg_offer - sales_reg_control)/takeup)
monthly_sales_het %<>% 
  mutate(sales_w_accepted_2.75 = sales_w_control +  (sales_w_offerfee2.75 - sales_w_control)/takeup_2.75, 
         sales_log_accepted_2.75 = sales_log_control +  (sales_log_offerfee2.75 - sales_log_control)/takeup_2.75, 
         sales_asinh_accepted_2.75 = sales_asinh_control +  (sales_asinh_offerfee2.75 - sales_asinh_control)/takeup_2.75, 
         sales_atall_accepted_2.75 = sales_atall_control + (sales_atall_offerfee2.75 - sales_atall_control)/takeup_2.75, 
         sales_reg_accepted_2.75 = sales_reg_control + (sales_reg_offerfee2.75 - sales_reg_control)/takeup_2.75,
         
         sales_w_accepted_3.00 = sales_w_control +  (sales_w_offerfee3.00 - sales_w_control)/takeup_3.00, 
         sales_log_accepted_3.00 = sales_log_control +  (sales_log_offerfee3.00 - sales_log_control)/takeup_3.00, 
         sales_asinh_accepted_3.00 = sales_asinh_control +  (sales_asinh_offerfee3.00 - sales_asinh_control)/takeup_3.00, 
         sales_atall_accepted_3.00 = sales_atall_control + (sales_atall_offerfee3.00 - sales_atall_control)/takeup_3.00, 
         sales_reg_accepted_3.00 = sales_reg_control + (sales_reg_offerfee3.00 - sales_reg_control)/takeup_3.00)

# (1.8): Export data.
write_csv(monthly_sales, here("proc", "24a_monthly_sales.csv"))
write_csv(monthly_sales_het, here("proc", "24a_monthly_sales_het.csv"))

#######################################
##  (2): Create weekly time series.  ##
#######################################
# (2.1): Clean up previous content and read in data.
# rm(list = ls())
# gc()
fintech_fee_weekly <- qs::qread(here("proc", "fintech_fee_weekly.qs")) %>% select(1:70, fee_2.75,make_sale )
fintech_fee_weekly %>% select(organization_uuid) %>% unique() %>% count()
source(file = here("scripts", "programs", "winsorize.R"), encoding = "UTF-8")
source(file = here("scripts", "programs", "myfunctions.R"), encoding = "UTF-8")
fintech_fee_weekly <- setDT(fintech_fee_weekly)
fintech_fee <- fread(here("proc", "fintech_fee.csv"))

# (2.2): Create outcome variables.
winsorized <- fintech_fee_weekly %>%
  select(organization_uuid, valid_volume, group_id, timestamp_week) %>%
  winsorize(outcome = "valid_volume", newvar = "valid_volume_w5", by = c("group_id" , "timestamp_week"))

fintech_fee_weekly %<>%
  select(!c(valid_volume))
fintech_fee_weekly <- setDT(fintech_fee_weekly)
winsorized <- setDT(winsorized)
fintech_fee_weekly <- winsorized[fintech_fee_weekly, on =c("organization_uuid", "group_id", "timestamp_week")]

#fintech_fee_weekly %<>% winsorize(outcome = "valid_volume", newvar  = "valid_volume_w5",  by      = c("treat_type", "timestamp_week"))
fintech_fee_weekly[, asinh_sales := asinh(valid_volume)]
fintech_fee_weekly[, log_valid_volume := log(valid_volume + 1)]

# (2.2): Create control group time series.
weekly_ctl_sales <- fintech_fee_weekly %>% 
  filter(treat_type == "T1") %>% 
  group_by(timestamp_week) %>%
  summarise(sales_w = mean(valid_volume_w5), 
            sales_log = mean(log_valid_volume), 
            sales_asinh = mean(asinh_sales), 
            sales_atall = mean(make_sale), 
            sales_reg = mean(valid_volume)) %>%
  mutate(group = "control") 

# (2.3): Create offer group time series.
# Get average weekly sales for "offer" group
weekly_offer_sales <- fintech_fee_weekly %>% 
  filter(treat_type != "T1") %>% 
  group_by(timestamp_week) %>%
  summarise(sales_w = mean(valid_volume_w5), 
            sales_log = mean(log_valid_volume), 
            sales_asinh = mean(asinh_sales), 
            sales_atall = mean(make_sale), 
            sales_reg = mean(valid_volume)) %>%
  mutate(group = "offer")

weekly_offer_sales_het <- fintech_fee_weekly %>% 
  filter(treat_type != "T1") %>% 
  group_by(timestamp_week, fee_2.75) %>%
  summarise(sales_w = mean(valid_volume_w5), 
            sales_log = mean(log_valid_volume), 
            sales_asinh = mean(asinh_sales), 
            sales_atall = mean(make_sale), 
            sales_reg = mean(valid_volume)) %>%
  mutate(group = "offer")

# (2.4): Bind datasets together.
weekly_sales_het <- weekly_ctl_sales %>% 
  bind_rows(weekly_offer_sales_het) %>%
  mutate(fee_2.75 = if_else(fee_2.75 == 1, "fee2.75", "fee3.00", NA_character_)) %>%
  mutate(group = gsub("NA", "", paste0(group, fee_2.75))) %>%
  select(!fee_2.75) %>%
  pivot_wider(names_from = group, values_from = c("sales_w", "sales_log", "sales_asinh", "sales_atall", "sales_reg")) 

weekly_sales <- weekly_ctl_sales %>% 
  bind_rows(weekly_offer_sales) %>%
  pivot_wider(names_from = group, values_from = c("sales_w", "sales_log", "sales_asinh", "sales_atall", "sales_reg")) 

# (2.5): Get average weekly sales for "treated" group.
takeup <- mean(fintech_fee$accepted_offer_late[fintech_fee$treat_type != "T1"])
takeup_2.75 <- mean(fintech_fee$accepted_offer_late[fintech_fee$treat_type != "T1" & fintech_fee$fee_2.75 == 1])
takeup_3.00 <- mean(fintech_fee$accepted_offer_late[fintech_fee$treat_type != "T1" & fintech_fee$fee_2.75 == 0])

weekly_sales %<>% 
  mutate(sales_w_accepted = sales_w_control +  (sales_w_offer - sales_w_control)/takeup, 
         sales_log_accepted = sales_log_control +  (sales_log_offer - sales_log_control)/takeup, 
         sales_asinh_accepted = sales_asinh_control +  (sales_asinh_offer - sales_asinh_control)/takeup, 
         sales_atall_accepted = sales_atall_control + (sales_atall_offer - sales_atall_control)/takeup, 
         sales_reg_accepted = sales_reg_control + (sales_reg_offer  - sales_reg_control)/takeup)

weekly_sales_het %<>%
  mutate(sales_w_accepted_2.75 = sales_w_control +  (sales_w_offerfee2.75 - sales_w_control)/takeup_2.75, 
         sales_log_accepted_2.75 = sales_log_control +  (sales_log_offerfee2.75 - sales_log_control)/takeup_2.75, 
         sales_asinh_accepted_2.75 = sales_asinh_control +  (sales_asinh_offerfee2.75 - sales_asinh_control)/takeup_2.75, 
         sales_atall_accepted_2.75 = sales_atall_control + (sales_atall_offerfee2.75 - sales_atall_control)/takeup_2.75, 
         sales_reg_accepted_2.75 = sales_reg_control + (sales_reg_offerfee2.75 - sales_reg_control)/takeup_2.75,
         
         sales_w_accepted_3.00 = sales_w_control +  (sales_w_offerfee3.00 - sales_w_control)/takeup_3.00, 
         sales_log_accepted_3.00 = sales_log_control +  (sales_log_offerfee3.00 - sales_log_control)/takeup_3.00, 
         sales_asinh_accepted_3.00 = sales_asinh_control +  (sales_asinh_offerfee3.00 - sales_asinh_control)/takeup_3.00, 
         sales_atall_accepted_3.00 = sales_atall_control + (sales_atall_offerfee3.00 - sales_atall_control)/takeup_3.00, 
         sales_reg_accepted_3.00 = sales_reg_control + (sales_reg_offerfee3.00 - sales_reg_control)/takeup_3.00
  )

# (2.6): Export data.
write_csv(weekly_sales, here("proc", "24a_weekly_sales.csv"))
write_csv(weekly_sales_het, here("proc", "24a_weekly_sales_het.csv"))
