#--------------------------------------------------------------------------------------------
# File name: 		      08b_panelregressions.R
# Creation date:      2021-02-15
# Author:          		Noah Forougi and César Landín
# Files used:
#  - here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
# Files created:
#   - here("proc", "20_panel_regressions.qs")
# Purpose:
# 	- Run the panel regressions on a monthly and weekly level.
#--------------------------------------------------------------------------------------------

#################### Import packages ####################
library(here)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(lubridate)
library(fixest)
library(modelsummary)
library(data.table)
library(lubridate)
library(qs)
library(tabulator)
library(assertthat)
source(here("scripts", "programs", "strip_fixest.R"))
#########################################################

##########################################################################
##    (1): Import data and define regression functions and parameters.  ##
##########################################################################
# (1.1): Read in daily to monthly card data with adjusted dates.
fintech_fee_dtm_adjusteddates <- qread(here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")) %>% 
  select(postXtreated, postXadopted,
         postXadoptedXfee_2.75,postXtreatedXfee_2.75, organization_uuid, timestamp_month, 
         log_valid_volume, log_valid_volume_w5, log_nr_payments, log_nr_payments_w5, make_sale, 
         treated, adopted, fee_2.75,
         treatedXfee_2.75, adoptedXfee_2.75, start_date) %>% 
  filter(timestamp_month <= ymd("2021-03-01"))  
# 2022-12-02: Keep data until 2021-03-01 (not 2021-04-01), since we told firms that they'd have lower fee for only 6 months
# 2025-11-28: Xinyu added start_date to the data b/c we want to know which company started business relationship with Zettle.
# (1.4): Generate dummy: firm made any number of transactions on or after current month.
fintech_fee_dtm_adjusteddates %<>%
  arrange(organization_uuid, desc(timestamp_month)) %>% 
  group_by(organization_uuid) %>% 
  mutate(made_sale_after = cumsum(make_sale)) %>% 
  arrange(organization_uuid, timestamp_month) %>% 
  mutate(made_sale_after = ifelse(made_sale_after > 0, 1, 0)) %>% 
  ungroup()

# (1.5): Define parameters for regression objects.
outcomes <- c("log_valid_volume_w5", "log_nr_payments_w5", "make_sale", "made_sale_after") #"arcsinh_valid_volume", , "asinh_nr_payments" "nr_valid_payments_w5", 
clean_outcomes <- c("Log(sales + 1)", "Log(\\# transactions + 1)", "Made at least 1 sale", "Continued using technology") # "arcsinh(sales)", ,"arcsinh(\\# transactions)" #"Winsorized payments",
months <- unique(fintech_fee_dtm_adjusteddates$timestamp_month)
clean_months <- paste(lubridate::month(months, label = T), lubridate::year(months))

###########################################################
##    (2): Run monthly panel regressions (ITT and TOT).  ##
###########################################################
# (2.1): Intent-to-treat (full sample).
reg_monthly_itt_dtm_adjusteddates <- lapply(outcomes, function(x){ 
  model <- paste(x, "~", "postXtreated  | organization_uuid + timestamp_month ")
  reg <- feols(
    fml = as.formula(model) , 
    data = fintech_fee_dtm_adjusteddates, 
    cluster = "organization_uuid"
  )
  return(strip_feols(reg))
}
)
names(reg_monthly_itt_dtm_adjusteddates) <- clean_outcomes

# (2.3): Treatment-on-the-treated (full sample).
reg_monthly_tot_dtm_adjusteddates <- lapply(outcomes, function(x){
  model <- paste(x, "~ 0 | organization_uuid + timestamp_month | postXadopted ~ postXtreated")
  reg <- feols(
    fml = as.formula(model), 
    data = fintech_fee_dtm_adjusteddates, 
    cluster = "organization_uuid"
  )
  return(strip_feols_iv(reg))
}
)
names(reg_monthly_tot_dtm_adjusteddates) <- clean_outcomes


# (2.5): Export regression objects.
stuff_to_save <-  ls(.GlobalEnv)[grep("reg", ls())]
all_regressions <- list()
for (i in 1:length(stuff_to_save)) {
  all_regressions[[i]] <- get(stuff_to_save[i])
}
names(all_regressions) <- stuff_to_save
qsave(all_regressions, here("proc", "20_panel_regressions.qs"))
