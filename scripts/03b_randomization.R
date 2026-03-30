#--------------------------------------------------------------------------------------------
# File name: 		      03b_randomization.R
# Creation date:      2022-01-03
# Author:          		Noah Forougi & César Landín
# Files used:
#   - here("proc", "temp", "07_prep_randomization.rds")
#   - here("proc", "temp", "optimal_ss.rds")
# Files created:
#   - here("proc", "08_randomization.csv")
#   - here("proc", "fintech_fee_randomization.xlsx")
# Purpose:
# 	- This script creates the randomization for the fintech Merchant Fee experiment.
#--------------------------------------------------------------------------------------------

# Packages
library(here)
library(tidyverse)
library(writexl)
library(magrittr)
library(randomizr)
library(assertthat)
library(here)
library(tidyverse)
library(writexl)
library(magrittr)
library(randomizr)
library(assertthat)

###################################
##  (1): Conduct randomization.  ##
###################################
# (1.1): Read in data.
data_random <- read_rds(here("proc", "temp", "07_prep_randomization.rds"))
optimal_ss <- read_rds(here("proc", "temp", "optimal_ss.rds"))

# (1.2): Define randomization blocks. 
# data_random is a data frame where each row is a unique customer. 
# We will randomize into 8 treatment arms, stratified by sales quartile and business type.
blocks_overall <- with(data_random, paste(sales_quartiles, new_buss_type, sep = "_"))
as.data.frame(table(blocks_overall))

# We want to create a group of 4,010 firms that are randomly assigned to T1. The rest of the firms are assigned to one of the treatment groups. 
# Use the different blocks to assign approx 4k firms to control. Then, if they were not assigned to control, we assign them to one of the treatment groups.

# (1.3): Randomize treatment.
seed <- 70016777
set.seed(seed)
p_control <- 4010 / 34010
data_random %<>%
  mutate(control = block_ra(blocks = blocks_overall, prob = p_control),
         treatment_group_random = if_else(control == 0,
                                          block_ra(blocks = blocks_overall, prob_each = optimal_ss$prob),
                                          as.factor("Control")))
# WARNING MESSAGE: creates NA if control == 1 for treatment group random (which is what we want - control group are not assigned a treatment type).

# (1.4): Randomization check 1.
set.seed(seed)
data_random %<>%
  mutate(control = block_ra(blocks = blocks_overall, prob = p_control),
         treatment_group_random_CHECK = if_else(control == 0,
                                                block_ra(blocks = blocks_overall, prob_each = optimal_ss$prob),
                                                as.factor("Control")))
assert_that(all(data_random$treatment_group_random[] == data_random$treament_group_random_CHECK)) # Works

# (1.5): Calculate some quick descriptive stats.
table(data_random$control) # 4010 assigned to control
table(data_random$treatment_group_random)
# Actual
c(table(data_random$treatment_group_random) / 30000) # Actual assignment probabilities look similar
# Optimal
optimal_ss$prob

# (1.6): Randomize the fee reduction.
blocks_overall2 <- with(data_random, paste(treatment_group_random, sales_quartiles, new_buss_type, sep = "_"))
set.seed(seed)
data_random %<>%
  mutate(fee_assign = if_else(control == 0,
                              block_ra(blocks = blocks_overall2, num_arms = 2),
                              as.factor(NA))) %>%
  mutate(fee_group_random = if_else(fee_assign == "T1", "0.03", "0.0275"))

##################################
##  (2): Export randomization.  ##
##################################
# (2.1): Create output.
# data_random_alt <- data_random %>% 
data_random %<>%
  mutate(treatment_group_random = if_else(is.na(treatment_group_random) | treatment_group_random == "Control", 
                                          "T1", as.character(treatment_group_random)),
         treatment_description = case_when(treatment_group_random == "T1" ~ "Control",
                                           treatment_group_random == "T2" ~ "No deadline, no reminder",
                                           treatment_group_random == "T3" ~ "No deadline, anticipated reminder",
                                           treatment_group_random == "T4" ~ "No deadline, unanticipated reminder",
                                           treatment_group_random == "T5" ~ "Deadline, no reminder",
                                           treatment_group_random == "T6" ~ "Deadline, anticipated reminder",
                                           treatment_group_random == "T7" ~ "Deadline, unanticipated reminder",
                                           treatment_group_random == "T8" ~ "24-hour deadline, no reminder"),
         fee_description = case_when(fee_group_random == "0.03" ~ "3% offer",
                                     fee_group_random == "0.0275" ~ "2.75% offer",
                                     is.na(fee_group_random) ~ ""),
         group_description = if_else(treatment_group_random != "T1", 
                                     paste(treatment_description, fee_description, sep = " with "), 
                                     paste(treatment_description)),
         group_description = as.factor(group_description),
         treatment_description = as.factor(treatment_description))

# (2.2): Define treatment group variable.
data_random %<>%
  mutate(group_id = case_when(group_description == "Control" ~ 1,
                              group_description == "No deadline, no reminder with 3% offer" ~ 2,
                              group_description == "No deadline, anticipated reminder with 3% offer" ~ 3,
                              group_description == "No deadline, unanticipated reminder with 3% offer" ~ 4,
                              group_description == "Deadline, no reminder with 3% offer" ~ 5,
                              group_description == "Deadline, anticipated reminder with 3% offer" ~ 6,
                              group_description == "Deadline, unanticipated reminder with 3% offer" ~ 7,
                              group_description == "24-hour deadline, no reminder with 3% offer" ~ 8,
                              group_description == "No deadline, no reminder with 2.75% offer" ~ 9,
                              group_description == "No deadline, anticipated reminder with 2.75% offer" ~ 10,
                              group_description == "No deadline, unanticipated reminder with 2.75% offer" ~ 11,
                              group_description == "Deadline, no reminder with 2.75% offer" ~ 12,
                              group_description == "Deadline, anticipated reminder with 2.75% offer" ~ 13,
                              group_description == "Deadline, unanticipated reminder with 2.75% offer" ~ 14,
                              group_description == "24-hour deadline, no reminder with 2.75% offer" ~ 15))

# (2.3): Subset dataset and rename variables.
output <- data_random %>% select(organization_uuid,
                                 start_date,
                                 commission_model,
                                 last_valid_payment,
                                 birth_date, customer_type,
                                 sales_quartiles,
                                 new_buss_type,
                                 treat_type = treatment_group_random,
                                 treat_description = treatment_description,
                                 fee_type = fee_description,
                                 group_id,
                                 group_description,
                                 gender = genre,
                                 length,
                                 old_customer = usage_length_binary, 
                                 covid_shock_quartile)

# (2.4): Export output.
write_csv(output, here("proc", "08_randomization.csv"))

# (2.5): Export output for fintech in excel format
fintech_output <- list()
treatments <- levels(as.factor(output$group_id))
for (i in 1:length(treatments)) {
  temp <- output %>%
    filter(group_id == i) %>%
    select(organization_uuid)
  fintech_output[[i]] <- temp
  names(fintech_output)[i] <- paste("Base", treatments[i])
}
write_xlsx(fintech_output, here("proc", "fintech_fee_randomization.xlsx"))
