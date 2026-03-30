#--------------------------------------------------------------------------------------------
# File name: 		      07a_balance_table_data.R
# Creation date:      2022-01-07
# Author:          		César Landín
# Files used:
# 	- here("data", "Banxico", "tipoCambio.xls")
# 	- here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
#   - here("proc", "users.rds")
# Files created:
#   - here("proc", "balance_table_data.csv")
# Purpose:
# 	- Process data for balance tables and generate business-level dataset with 
#     data on baseline covariates and business characteristics.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(lubridate)
library(fastDummies)
library(readxl)
library(qs)
source(here("scripts", "programs", "output_directory.R"))
#########################################################

#####################################################
##    (1): Process sales and administrative data.  ##
#####################################################
# (1.1): Import and process Banxico exchange rate data.
exchange_rate <- read_xls(here("data", "Banxico", "tipoCambio.xls"), skip = 6)
colnames(exchange_rate) <- c("date", "usd_to_mxn", "dof", "payments")
exchange_rate %<>%
  select(date, usd_to_mxn) %>% 
  filter(usd_to_mxn != "N/E" & !is.na(usd_to_mxn)) %>% 
  mutate(date = dmy(date),
         month = month(date),
         year = year(date)) %>% 
  transmute(timestamp_month = dmy(str_c("01-", month, "-", year)),
            usd_to_mxn = as.numeric(usd_to_mxn)) %>% 
  group_by(timestamp_month) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup()

# (1.2): Import and filter monthly sales (keep pre-treatment months).
sales_monthly <- qread(here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")) %>% 
  select(organization_uuid, timestamp_month, new_buss_type,
         anticipated_reminder, unanticipated_reminder, deadline, fast_deadline, fee_2.75,
         valid_volume, valid_volume_w5, nr_valid_payments, nr_valid_payments_w5,
         make_sale) %>% 
  # Keep pre-treatment.
  filter(timestamp_month >= "2019-09-01" & timestamp_month < "2020-09-01")

# (1.3): Calculate USD monthly sales volume.
sales_monthly %<>%
  left_join(exchange_rate, by = "timestamp_month") %>% 
  mutate(valid_volume_usd = valid_volume / usd_to_mxn,
         valid_volume_usd_w5 = valid_volume_w5 / usd_to_mxn)

# (1.4): Get mean by business.
data_by_biz <- sales_monthly %>% 
  select(organization_uuid, anticipated_reminder, unanticipated_reminder, fast_deadline, deadline, fee_2.75, new_buss_type, 
         valid_volume, valid_volume_w5, valid_volume_usd, valid_volume_usd_w5, 
         nr_valid_payments, nr_valid_payments_w5, make_sale) %>% 
  group_by(organization_uuid, anticipated_reminder, unanticipated_reminder, fast_deadline, deadline, fee_2.75, new_buss_type) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(log_valid_volume = log(valid_volume),
         log_valid_volume_w5 = log(valid_volume_w5), 
         log_nr_valid_payments = log(nr_valid_payments),
         log_nr_valid_payments_w5 = log(nr_valid_payments_w5))

# (1.5): Generate business type dummies.
data_by_biz %<>%
  dummy_cols("new_buss_type") %>% 
  rename_all(~str_to_lower(.)) 

# (1.6): Calculate quartile of average pre-treatment monthly sales.
data_by_biz %<>% 
  mutate(sales_quartile = as.factor(ntile(valid_volume_w5, 4)),
         sales_quartile_q1 = ifelse(sales_quartile == 1, 1, 0),
         sales_quartile_q2 = ifelse(sales_quartile == 2, 1, 0),
         sales_quartile_q3 = ifelse(sales_quartile == 3, 1, 0),
         sales_quartile_q4 = ifelse(sales_quartile == 4, 1, 0))

# (1.7): Import and merge user data (sex).
users <- read_rds(here("proc", "users.rds")) %>% 
  select(organization_uuid, first_valid_payment, genre, start_date, birth_date) %>% 
  rename(owner_sex = genre)
data_by_biz %<>% left_join(users, by = "organization_uuid")
rm(users, sales_monthly)

# (1.8): Generate variables.
data_by_biz %<>%
  mutate(months_since_ft = (ymd("2020-09-01") - as_date(first_valid_payment)) / 30.437,
         months_since_ft = round(months_since_ft, 2) %>% as.numeric(),
         owner_sex = case_when(owner_sex == "H" ~ "male", 
                               owner_sex == "M" ~ "female", 
                               owner_sex == "" ~ "unknown"),
         months_since_reg = (ymd("2020-09-01") - as_date(start_date)) / 30.437,
         months_since_reg = round(months_since_reg, 2) %>% as.numeric(),
         owner_age = (ymd("2020-09-01") - as_date(birth_date)) / (30.437 * 12),
         owner_age = round(owner_age, 2) %>% as.numeric(),
         owner_age_unknown = ifelse(is.na(owner_age), 1, 0),
         median_baseline_sales = median(valid_volume_w5),
         above_median_baseline_sales = ifelse(valid_volume_w5 >= median_baseline_sales, 1, 0)) %>% 
  dummy_cols(c("owner_sex"))

# (1.9): Keep subset of columns and save dataset.
data_by_biz %<>%
  select(-starts_with("sales_quartile"), -first_valid_payment, -start_date, -birth_date)
write_csv_(data_by_biz, here("proc", "balance_table_data.csv"))

