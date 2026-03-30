#--------------------------------------------------------------------------------------------
# File name: 		      03a_prep_randomization.R
# Creation date:      2022-01-03
# Author:          		Noah Forougi & César Landín
# Files used:
# 	- here("data", "Sent_20200903", "october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv")
# 	- here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv")
# 	- here("data", "Sent_20200903", "october2020_pricing_ABtest_USERS_0903.csv")
# 	- here("data", "AB_TestMay2019", "OpenRates_FilesSent20190523", "open_and_conversion_by_user_23052019.csv")
# 	- here("data", "AB_TestMay2019", "FilesSent20191025", "results-20191023-164134.csv")
# 	- here("data", "Sent_20200124", "all_users_24012020_activity.csv")
# 	- here("data", "Received20190423", "iteration4", "first_new_price_abtesting_USER_INFO_iteration4.csv")
# Files created:
# 	- here("proc", "temp", "baseline_sales.Rds")
# 	- here("proc", "temp", "optimal_ss.rds")
# 	- here("proc", "temp", "07_prep_randomization.rds")
# Purpose:
# 	- This script prepares the randomization for the fintech merchant fee reduction 
#     experiment. It creates a panel of monthly transaction data, creates 
#     identifying info for firms, and runs power calculations from pilot data to inform 
#     the relevant randomization proportions.
#--------------------------------------------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)
library(magrittr)
library(here)
library(tabulator)

######################################################
##  (1): Read in data and create a balanced panel.  ##
######################################################
# (1.1): Read in data.
monthly_data <- fread(here("data", "Sent_20200903", "october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv")) %>%
  mutate(timestamp_month = ymd(timestamp_month))
users <- fread(here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv")) %>%
  mutate(created_date = ymd(created_date))
users_new <- fread(here("data", "Sent_20200903", "october2020_pricing_ABtest_USERS_0903.csv")) %>% mutate(created_date = ymd(created_date))
`%nin%` <- Negate(`%in%`)
users_new %<>%
  filter(organization_uuid %nin% users$organization_uuid)
users %<>% rbind(users_new)

# (1.2): Complete dataset (every month and every merchant).
monthly_data %<>%
  complete(organization_uuid, timestamp_month) %>%
  replace_na(list(nr_valid_payments = NA,
                  valid_volume = NA))

# (1.3): Get users' start date.
users %<>%
  mutate(start_date = created_date) %>%
  mutate(start_date = ymd(paste(substr(start_date, 1, 7), "01", sep = "-")))

# (1.4): Join data frames.
monthly_data %<>%
  left_join(users, by = "organization_uuid")

# (1.5): Remove observations that occurred before start date.
monthly_data %<>%
  filter(timestamp_month >= start_date)

# (1.6): Replace NA with 0 if the firm had started using the technology.
monthly_data %<>%
  replace_na(., replace = list(valid_volume = 0,
                               nr_valid_payments = 0))

# (1.7): Filter based on August sales data (over 1400 in sales in August) and not Zero Commission merchants.
merchants_over1400 <- monthly_data %>%
  mutate(group1 = as.numeric(valid_volume >= 1400 & valid_volume <= 20000 & timestamp_month == "2020-08-01"),
         group2_fr = as.numeric(valid_volume > 20000 & commission_model == "FIXED_RATE" & timestamp_month == "2020-08-01")) %>%
  filter((commission_model != "ZERO_COMMISSION" & group1 == 1) | group2_fr == 1) %>%
  select(organization_uuid)
users %>% 
  left_join(merchants_over1400 %>% 
              mutate(in_sample = 1),
            by = "organization_uuid") %>% 
  mutate(in_sample = ifelse(is.na(in_sample), 0, in_sample)) %>% 
  tab(in_sample, commission_model)
monthly_data %<>% filter(organization_uuid %in% merchants_over1400$organization_uuid)
length(unique(monthly_data$organization_uuid))

###############################################################
##  (2): Create baseline sales and business type variables.  ##
###############################################################
# (2.1): Baseline sales are defined as the average sales from whenever they started using the technology to Aug 2020.
baseline_sales <- monthly_data %>%
  group_by(organization_uuid) %>%
  summarise(sales = mean(valid_volume, na.rm = T)) %>%
  mutate(flag = 1)
saveRDS(baseline_sales, here("proc", "temp", "baseline_sales.Rds"))

# (2.2): Keep subset of users. 
users %<>% filter(organization_uuid %in% monthly_data$organization_uuid)

# (2.3): Define sales quartiles.
data_random <- left_join(users, baseline_sales, by = "organization_uuid") %>%
  mutate(sales = ifelse(is.na(flag), 0, flag),
         sales_quartiles = ntile(sales, 4))

# (2.4): Create business type.
business_to_new_bus <- tribble(~business_type_description, ~new_buss_type,
                               "Otro tipo de venta al detalle", "Small_retailers",
                               "Alimentos preparados", "Small_retailers",
                               "Tiendas de abarrotes", "Small_retailers",
                               "Regalos/Florerías", "Small_retailers",
                               "Mercados", "Small_retailers",
                               "Tiendas especializadas y de todo tipo", "Small_retailers",
                               
                               "Restaurantes/Cafeterias", "Restaurants",
                               "Bares/Clubs", "Restaurants",
                               "Catering/Banquetes", "Restaurants",
                               "Lugares para comer, Restaurantes", "Restaurants",
                               
                               "Servicios Médicos", "Professionals",
                               "Dentistas", "Professionals",
                               "Veterinarios", "Professionals",
                               "Contables/Financieros/Legales ", "Professionals",
                               "Servicios de computación & TI", "Professionals",
                               "Servicios profesionales", "Professionals",
                               " Arte, diseño gráfico, fotografía", "Professionals",
                               "Tecnologías de información servicios y consultorías", "Professionals",
                               "Arte/Diseño/Manualidades", "Professionals",
                               
                               "Ropa y accesorios", "Clothing",
                               "Tiendas de accesorios y ropa", "Clothing",
                               
                               "Peluquerías y salones de belleza", "Beauty",
                               "Peluquerías/Estéticas/Spas", "Beauty")
data_random <- data_random %>%
  left_join(business_to_new_bus, "business_type_description") %>%
  mutate(new_buss_type = ifelse(is.na(new_buss_type), "Other", new_buss_type))

# (2.5): Fix customer type data.
data_random %<>% mutate(customer_type = ifelse(customer_type == "", "Missing", customer_type))

# (2.6): Create length of usage variable.
data_random %<>%
  mutate(length = ymd(20200901) - ymd(created_date)) %>%
  mutate(usage_length = case_when(length >= median(length) ~ "Older",
                                  length < median(length) ~ "Newer"),
         usage_length_binary = ifelse(length >= median(length), 1, 0))

# (2.7): Create Covid 19 Shock Variable.
transactions <- monthly_data %>%
  select(organization_uuid, timestamp_month, valid_volume, start_date) %>%
  filter(timestamp_month >= start_date) %>%
  filter(timestamp_month >= ymd("20190901") & timestamp_month <= ymd("20200801")) %>%
  mutate(date_group = case_when(timestamp_month >= ymd("20190901") & timestamp_month <= ymd("20200201") ~ "sep19_feb20",
                                timestamp_month >= ymd("20200301") & timestamp_month <= ymd("20200801") ~ "mar20_aug20")) 

transactions %<>%
  group_by(organization_uuid, date_group) %>%
  summarise(avg_sales = mean(valid_volume, na.rm = T)) %>%
  pivot_wider(names_from = "date_group", 
              values_from = "avg_sales") %>%
  mutate(sales_change = (mar20_aug20 - sep19_feb20) / sep19_feb20) %>% 
  ungroup() 

transactions$sales_change[transactions$sales_change == Inf] <- NA

transactions %<>%
  mutate(covid_shock_quartile = ntile(sales_change, 4)) 

transactions %<>% 
  select(organization_uuid, covid_shock_quartile) %>%
  mutate(covid_shock_quartile = case_when(covid_shock_quartile == 1 ~ "covid_shock_q1", 
                                          covid_shock_quartile == 2 ~ "covid_shock_q2", 
                                          covid_shock_quartile == 3 ~ "covid_shock_q3", 
                                          covid_shock_quartile == 4 ~ "covid_shock_q4", 
                                          is.na(covid_shock_quartile) ~ "covid_shock_undefined"))

data_random %<>% 
  left_join(transactions, by = "organization_uuid") 

####################################################################################
##  (3): # Calculate the optimal sample size for each arm (using May 2019 data).  ##
####################################################################################
# (3.1): Read AB_old from May 2019.
data_23052019 <- read.csv(here("data", "AB_TestMay2019", "OpenRates_FilesSent20190523", "open_and_conversion_by_user_23052019.csv"),
                          header = T,
                          stringsAsFactors = F) %>%
  mutate(fecha_solicito_baja = mdy_hm(fecha_solicito_baja),
         date = date(fecha_solicito_baja),
         treatment_group = factor(treatment_group))

# (3.2): Read AB_new from Oct 2019.
AB_new <- read.csv(here("data", "AB_TestMay2019", "FilesSent20191025", "results-20191023-164134.csv"),
                   header = T,
                   stringsAsFactors = F)

# (3.3): Make sure every firms in AB_new are shown as adopted in AB_old.
data_23052019 <- data_23052019 %>%
  mutate(solicito_baja_comision = ifelse(organization_uuid %in% AB_new$organization_uuid,
                                         1, solicito_baja_comision))

# (3.4): Get most recent AB firm transaction data (until Dec).
AB_transaction <- read_csv(here("data", "Sent_20200124", "all_users_24012020_activity.csv")) %>%
  filter(organization_uuid %in% data_23052019$organization_uuid) %>% # make sure they are in AB firm data
  mutate(timestamp_month = ymd(timestamp_month)) %>%
  filter(timestamp_month <= ymd("2019-12-01")) # only keep AB data until 2019 Dec

# (3.5): Convert to datatable.
AB_transaction_bla <- setDT(AB_transaction,
                            key = c("organization_uuid", "timestamp_month"))

# (3.6): Create balance panel using CJ
AB_transaction_bla <- AB_transaction_bla[CJ(organization_uuid, timestamp_month, unique = T)] %>%
  mutate(nr_valid_payments = replace_na(nr_valid_payments, 0)) %>% # replace NA with 0
  mutate(valid_volume = replace_na(valid_volume, 0)) %>%
  filter(valid_volume >= 0) %>%
  mutate(month = month(timestamp_month),
         year = year(timestamp_month))

# (3.7): Read registration date data.
Registration_date <- read_csv(here("data", "Received20190423", "iteration4", "first_new_price_abtesting_USER_INFO_iteration4.csv")) %>%
  mutate(registration_date = ymd(registration_date)) %>%
  select(organization_uuid, registration_date) %>%
  mutate(firstmonth = floor_date(registration_date, unit = "month")) # get the Monday date for each registration date (the transaction data is on the first date of each month)

# (3.8): Drop any transaction data before registration - these transaction are all 0s and are created because of the CJ.
AB_transaction_bla <- inner_join(Registration_date, AB_transaction_bla,
                                 by = "organization_uuid") %>%
  filter(timestamp_month >= firstmonth)

# (3.9): Prepare baseline sales data
baseline_sales <- AB_transaction_bla %>%
  filter(timestamp_month <= as.Date("2019-04-30") & timestamp_month >= as.Date("2018-05-01")) %>%
  group_by(organization_uuid, month, year) %>%
  summarise(sales = sum(valid_volume)) %>%
  ungroup() %>%
  group_by(organization_uuid) %>%
  summarise(sales = mean(sales)) %>%
  mutate(quartile = ntile(sales, 4)) %>%
  mutate(i.p75 = ifelse(sales >= quantile(sales, 0.75), 1, 0),
         i.p70 = ifelse(sales >= quantile(sales, 0.70), 1, 0),
         i.p65 = ifelse(sales >= quantile(sales, 0.65), 1, 0),
         i.p60 = ifelse(sales >= quantile(sales, 0.60), 1, 0),
         i.p55 = ifelse(sales >= quantile(sales, 0.55), 1, 0),
         i.p50 = ifelse(sales >= quantile(sales, 0.50), 1, 0))

# (3.10): Merge the baselines sales with the firm adoption data.
baseline_sales <- inner_join(baseline_sales, data_23052019, "organization_uuid")

AB_firm <- inner_join(data_23052019,
                      baseline_sales %>% select(organization_uuid, quartile),
                      "organization_uuid")

AB_firm %>%
  filter(quartile == 4) %>%
  filter(treatment_group == "T3" | treatment_group == "T4") %>%
  summarise(takeup = mean(solicito_baja_comision))

P75 <- quantile(baseline_sales$sales, 0.75)

# (3.11): Define function to perform power calculation.
Binomial_optimal_size <- function(p0, p1, alpha = 0.05, beta = 0.8) {
  p_bar <- (p0 + p1) / 2
  t_alpha <- qnorm(1 - alpha / 2)
  t_beta <- qnorm(beta)
  n <- (t_alpha * sqrt(2 * p_bar * (1 - p_bar)) + t_beta * sqrt(p0 * (1 - p0) + p1 * (1 - p1)))^2 * (abs(p1 - p0))^(-2)
  return(n)
}

# (3.12): Use final group treatment numbers to make these comparisons.
# T1 (Control) vs T2 (No deadline, no reminder)
Treatment_Control <- AB_firm %>%
  filter(quartile == 4) %>%
  mutate(treatment = ifelse(treatment_group == "T3", 1,
                            ifelse(treatment_group == "T1", 0, NA)
  )) %>%
  filter(!is.na(treatment))

p0 <- mean(setDT(Treatment_Control)[treatment == 0, solicito_baja_comision])
p1 <- mean(setDT(Treatment_Control)[treatment == 1, solicito_baja_comision])

T1_vs_T2 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T1_vs_T2_text <- paste("T2 vs T1:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T1_vs_T2, 0))

#  (Deadline, no reminder) vs  (Control)
# T1 vs T5
Treatment_Control <- AB_firm %>%
  filter(quartile == 4) %>%
  mutate(treatment = ifelse(treatment_group == "T4", 1,
                            ifelse(treatment_group == "T1", 0, NA)
  )) %>%
  filter(!is.na(treatment))

p0 <- mean(setDT(Treatment_Control)[treatment == 0, solicito_baja_comision])
p1 <- mean(setDT(Treatment_Control)[treatment == 1, solicito_baja_comision])

T1_vs_T5 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T1_vs_T5_text <- paste("T5 vs T1:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T1_vs_T5, 0))

# (Deadline, no reminder) vs (No deadline, no reminder)
# T2 vs T5
Treatment_Control <- AB_firm %>%
  filter(quartile == 4) %>%
  mutate(treatment = ifelse(treatment_group == "T4", 1,
                            ifelse(treatment_group == "T3", 0, NA)
  )) %>%
  filter(!is.na(treatment))

p0 <- mean(setDT(Treatment_Control)[treatment == 0, solicito_baja_comision])
p1 <- mean(setDT(Treatment_Control)[treatment == 1, solicito_baja_comision])

T2_vs_T5 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T2_vs_T5_text <- paste("T5 vs T2:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T2_vs_T5, 0))

# No Dl, Ant. Rem vs No Dl, Unant. Rem.
# T4 vs T3
p0 <- nrow(AB_firm %>%
             filter(quartile == 4) %>%
             filter(treatment_group == "T3") %>%
             filter(fecha_solicito_baja < mdy_hm("05-10-2019 11:00"))) / nrow(AB_firm %>%
                                                                                filter(quartile == 4) %>%
                                                                                filter(treatment_group == "T3"))

p0_0 <- nrow(AB_firm %>%
               filter(quartile == 4) %>%
               filter(treatment_group == "T3") %>%
               filter(fecha_solicito_baja < mdy_hm("05-09-2019 11:00"))) / nrow(AB_firm %>%
                                                                                  filter(quartile == 4) %>%
                                                                                  filter(treatment_group == "T3"))

p1 <- p0 + 1.23 * (p0 - p0_0)
T4_vs_T3 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T4_vs_T3_text <- paste("T3 vs T4:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T4_vs_T3, 0))

# Deadline, Ant.Rem. vs Deadline, Unant.Rem
# T7 vs T6
p0 <- nrow(AB_firm %>%
             filter(quartile == 4) %>%
             filter(treatment_group == "T4") %>%
             filter(fecha_solicito_baja < mdy_hm("05-10-2019 11:00"))) / nrow(AB_firm %>%
                                                                                filter(quartile == 4) %>%
                                                                                filter(treatment_group == "T4"))
p0_0 <- nrow(AB_firm %>%
               filter(quartile == 4) %>%
               filter(treatment_group == "T4") %>%
               filter(fecha_solicito_baja < mdy_hm("05-09-2019 11:00"))) / nrow(AB_firm %>%
                                                                                  filter(quartile == 4) %>%
                                                                                  filter(treatment_group == "T4"))
p1 <- p0 + 1.23 * (p0 - p0_0)
T7_vs_T6 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T7_vs_T6_text <- paste("T6 vs T7:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T7_vs_T6, 0))

# No Dl, Unant. Rem. vs No Dl, No rem.
# T2 vs T4
p0 <- nrow(AB_firm %>%
             filter(quartile == 4) %>%
             filter(treatment_group == "T3") %>%
             filter(fecha_solicito_baja < mdy_hm("05-09-2019 11:00"))) / nrow(AB_firm %>%
                                                                                filter(quartile == 4) %>%
                                                                                filter(treatment_group == "T3"))
p1 <- nrow(AB_firm %>%
             filter(quartile == 4) %>%
             filter(treatment_group == "T3") %>%
             filter(fecha_solicito_baja < mdy_hm("05-10-2019 11:00"))) / nrow(AB_firm %>%
                                                                                filter(quartile == 4) %>%
                                                                                filter(treatment_group == "T3"))

T2_vs_T4 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T2_vs_T4_text <- paste("T4 vs T2:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T2_vs_T4, 0))

# Dl, Unant. Rem. vs Dl, No Rem.
# T5 vs T7
p0 <- nrow(AB_firm %>%
             filter(quartile == 4) %>%
             filter(treatment_group == "T4") %>%
             filter(fecha_solicito_baja < mdy_hm("05-09-2019 11:00"))) / nrow(AB_firm %>%
                                                                                filter(quartile == 4) %>%
                                                                                filter(treatment_group == "T4"))
p1 <- nrow(AB_firm %>%
             filter(quartile == 4) %>%
             filter(treatment_group == "T4") %>%
             filter(fecha_solicito_baja < mdy_hm("05-10-2019 11:00"))) / nrow(AB_firm %>%
                                                                                filter(quartile == 4) %>%
                                                                                filter(treatment_group == "T4"))
T5_vs_T7 <- Binomial_optimal_size(p0, p1, beta = 0.8)
T5_vs_T7_text <- paste("T7 vs T5:", "P0 =", round(p0, 2), "P1 =", round(p1, 2), "Optimal sample Size=", round(T5_vs_T7, 0))

# Tests we have:  T2 vs T1,  T5 vs T1, T5 vs T2, T3 vs T4, T6 vs T7, T4 vs T2, T7 vs T5
T1 <- max(T1_vs_T2, T1_vs_T5, 4010)
T2 <- max(T1_vs_T2, T2_vs_T5, T2_vs_T4)
T3 <- max(T4_vs_T3)
T4 <- max(T4_vs_T3, T2_vs_T4)
T5 <- max(T1_vs_T5, T2_vs_T5, T5_vs_T7)
T6 <- max(T7_vs_T6)
T7 <- max(T5_vs_T7, T7_vs_T6)
T8 <- T5

# We have the optimal sample size for each group. 
# T1 (control) will be fixed at 4010.
# We technically don't need to compare against T1, but it doesn't change anything in these calculations.
optimal_ss <- tibble(group = c("T1", "T2", "T3", "T4", "T5", "T6", "T7", "T8"),
                     optsize = c(T1, T2, T3, T4, T5, T6, T7, T8),
                     sum_optsize = sum(optsize[group != "T1"]),
                     sample = rep(34010, 8),
                     sample_exc_control = rep(30000, 8)) %>%
  mutate(prob = if_else(group != "T1", optsize / sum_optsize, 0))

#########################
##  (4): Export data.  ##
#########################
# (4.1): Export data for randomization
saveRDS(optimal_ss, here("proc", "temp", "optimal_ss.rds"))
saveRDS(data_random, here("proc", "temp", "07_prep_randomization.rds"))
