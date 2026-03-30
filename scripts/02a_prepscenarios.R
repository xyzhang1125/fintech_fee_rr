#--------------------------------------------------------------------------------------------
# File name: 		      02a_prepscenarios.R
# Creation date:      2021-01-01
# Author:          		Noah Forougi & César Landín
# Files used:
#   - here("proc", "monthly.rds")
# Files created:
#   - here("proc", "benefits_data.rds")
#   - here("proc", "benefits_data_all.rds")
# Purpose:
# 	- This script calculates expected costs, revenues and profits.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(here)
library(tidyverse)
library(magrittr)
library(magicfor)
#########################################################

########################################################
##  (1): Create measures to compute expected profit.  ##
########################################################
# (1.1): Read in monthly data.
monthly <- read_rds(here("proc", "monthly.rds"))

# (1.2): Calculate whole period sales.
wp <- monthly %>% 
  group_by(organization_uuid) %>%
  summarise(wholeperiod_sales = mean(valid_volume))

# (1.3): Calculate August sales.
aug <- monthly %>% 
  filter(timestamp_month == "2020-08-01") %>% 
  select(organization_uuid, august_sales = valid_volume) 

# (1.4): Calculate July sales.
july <- monthly %>% 
  filter(timestamp_month == "2020-07-01") %>% 
  select(organization_uuid, july_sales = valid_volume) 

`%nin%` = Negate(`%in%`)
complete_july <- aug[aug$organization_uuid %nin% july$organization_uuid,]
complete_july %<>% rename(july_sales = august_sales)

july <- bind_rows(july, complete_july)

# (1.5): Calculate pandemic users.
prepan_users <- monthly %>% 
  filter(start_date < "2020-03-01")
prepan_users_avg <- prepan_users %>% 
  filter(timestamp_month < "2020-03-01") %>% 
  group_by(organization_uuid) %>%
  summarise(pan_sales = mean(valid_volume))

pan_users <- monthly %>% 
  filter(start_date >= "2020-03-01")
pan_users_avg <- pan_users %>% 
  filter(timestamp_month == "2020-08-01") %>%
  group_by(organization_uuid) %>%
  summarise(pan_sales = valid_volume)

pan <- bind_rows(prepan_users_avg, pan_users_avg) #%>% 
# left_join(., monthly, by = "organization_uuid") %>% 
#filter(timestamp_month == "2020-08-01" & group1 == 1) %>%
#select(organization_uuid, pan_sales)

# (1.6): Update datasets.
monthly <- left_join(monthly, wp, by = "organization_uuid")
monthly <- left_join(monthly, aug, by = "organization_uuid")
monthly <- left_join(monthly, july, by = "organization_uuid")
monthly <- left_join(monthly, pan, by = "organization_uuid")

# (1.7): Get observations of interest.
all <- monthly
monthly %<>% filter(timestamp_month == "2020-08-01" & group1 == 1)

# (1.8): Create necessary variables.
monthly %<>% mutate(fee_reduction = case_when(commission_model == "SMART_RATE" ~ .5*(.0375-.0275) + .5*(.0375-.03), 
                                              commission_model == "FIXED_RATE" ~ .5*(.035-.0275) + .5*(.035-.03)), 
                    length = 6, 
                    fee_current = case_when(commission_model == "SMART_RATE" ~ 0.0375, 
                                            commission_model == "FIXED_RATE" ~ 0.035), 
                    elasticity = exp(0.137) - 1,
                    takeup_rate = (125+191)/(638+630), 
                    fee_new = (0.030 + 0.0275)/2)

# (1.9): Calculate expected cost, revenues and profits.
monthly %<>% 
  mutate(wholeperiod_cost = wholeperiod_sales * (fee_current - fee_new) * takeup_rate * length, 
         wholeperiod_cost_p = ntile(wholeperiod_cost, 100), 
         wholeperiod_revenue = (wholeperiod_sales * elasticity * fee_new * length ), 
         wholeperiod_profit = wholeperiod_revenue - wholeperiod_cost,
         
         wholeperiod_counterfactual = wholeperiod_sales * fee_current * takeup_rate * length, 
         wholeperiod_actual = wholeperiod_sales * fee_new * takeup_rate * length, 
         
         julyvol_cost = july_sales * (fee_current - fee_new) * takeup_rate * length,
         julyvol_cost_p = ntile(julyvol_cost, 100),
         julyvol_revenue = (july_sales * elasticity * fee_new * length), 
         julyvol_profit = julyvol_revenue - julyvol_cost,
         
         julyvol_counterfactual = july_sales * fee_current * takeup_rate * length, 
         julyvol_actual = july_sales * fee_new * takeup_rate * length, 
         
         augustvol_cost = august_sales * (fee_current - fee_new) * takeup_rate * length,
         augustvol_cost_p = ntile(augustvol_cost, 100),
         augustvol_revenue = (august_sales * elasticity * fee_new * length), 
         augustvol_profit = augustvol_revenue - augustvol_cost,
         
         augustvol_counterfactual = august_sales * fee_current * takeup_rate * length, 
         augustvol_actual = august_sales * fee_new * takeup_rate * length, 
         
         panvol_cost = pan_sales * (fee_current - fee_new) * takeup_rate * length,
         panvol_cost_p = ntile(panvol_cost, 100),
         panvol_revenue = (pan_sales * elasticity * fee_new * length), 
         panvol_profit = panvol_revenue - panvol_cost, 
         
         panvol_counterfactual = pan_sales * fee_current * takeup_rate * length, 
         panvol_actual = pan_sales * fee_new * takeup_rate * length) 

# (1.10): Calculation using group 1 and group 2 fixed rate.
all <- all %>% 
  filter( (timestamp_month == "2020-08-01" & group1 == 1) |
            (timestamp_month == "2020-08-01" & group2_fr == 1))

all %<>% 
  mutate(fee_reduction = case_when(commission_model == "SMART_RATE" ~ .5*(.0375-.0275) + .5*(.0375-.03), 
                                   commission_model == "FIXED_RATE" ~ .5*(.035-.0275) + .5*(.035-.03)), 
         length = 6, 
         fee_current = case_when(commission_model == "SMART_RATE" ~ 0.0375, 
                                 commission_model == "FIXED_RATE" ~ 0.035), 
         elasticity = exp(0.137) - 1,
         takeup_rate = (125+191)/(638+630), 
         fee_new = (0.030 + 0.0275)/2)

all %<>% 
  mutate(wholeperiod_cost = wholeperiod_sales * (fee_current - fee_new) * takeup_rate * length, 
         wholeperiod_cost_p = ntile(wholeperiod_cost, 100), 
         wholeperiod_revenue = (wholeperiod_sales * elasticity * fee_new * length ), 
         wholeperiod_profit = wholeperiod_revenue - wholeperiod_cost,
         
         wholeperiod_counterfactual = wholeperiod_sales * fee_current * takeup_rate * length, 
         wholeperiod_actual = wholeperiod_sales * fee_new * takeup_rate * length, 
         
         julyvol_cost = july_sales * (fee_current - fee_new) * takeup_rate * length,
         julyvol_cost_p = ntile(julyvol_cost, 100),
         julyvol_revenue = (july_sales * elasticity * fee_new * length), 
         julyvol_profit = julyvol_revenue - julyvol_cost,
         
         julyvol_counterfactual = july_sales * fee_current * takeup_rate * length, 
         julyvol_actual = july_sales * fee_new * takeup_rate * length, 
         
         augustvol_cost = august_sales * (fee_current - fee_new) * takeup_rate * length,
         augustvol_cost_p = ntile(augustvol_cost, 100),
         augustvol_revenue = (august_sales * elasticity * fee_new * length), 
         augustvol_profit = augustvol_revenue - augustvol_cost,
         
         augustvol_counterfactual = august_sales * fee_current * takeup_rate * length, 
         augustvol_actual = august_sales * fee_new * takeup_rate * length, 
         
         panvol_cost = pan_sales * (fee_current - fee_new) * takeup_rate * length,
         panvol_cost_p = ntile(panvol_cost, 100),
         panvol_revenue = (pan_sales * elasticity * fee_new * length), 
         panvol_profit = panvol_revenue - panvol_cost, 
         
         panvol_counterfactual = august_sales * fee_current * takeup_rate * length, 
         panvol_actual = august_sales * fee_new * takeup_rate * length) 

# (1.11): Export data.
write_rds(monthly, here("proc", "benefits_data.rds"))
write_rds(all, here("proc", "benefits_data_all.rds"))

