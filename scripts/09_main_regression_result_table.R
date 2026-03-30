#--------------------------------------------------------------------------------------------
# File name: 		      09_main_regression_result_table.R
# Creation date:      2021-10-19
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("results", "tables", "main_results_full.tex")
# Purpose:
# 	- Table C.2: Main Regression Results: Average Treatment Effects: Generate main results table regressing take-up on treatment dummies.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(fixest)
library(modelsummary)
library(kableExtra)
library(fastDummies)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
#########################################################

######################################################
##    (1): Generate panel A: treatment assignment.  ##
######################################################
# (1.1): Import e1 treatment assignment.
exp1_data <- read_csv(here("proc", "fintech_fee_light.csv"))

# (1.2): Generate treatment dummies.
exp1_data %<>%
  select(organization_uuid, accepted_offer_ontime, treat_type, treat_description, strata_fe, fee_2.75, control) %>% 
  dummy_cols("treat_type") %>% 
  rename_all(~str_to_lower(.)) %>% 
  mutate(treat_description = as.factor(str_c("Group ", str_sub(treat_type, 2, 2), ": ", treat_description)),
         treat_type = as.factor(treat_type))

# (1.3): Generate vector with long names.
treat_dist <- exp1_data %>% 
  tab(treat_description) %>% 
  arrange(treat_description) %>% 
  pull(treat_description) %>% 
  as.character() %>% 
  str_replace(pattern = "24-hour", 
              replacement = "Same-day") %>% 
  str_replace(pattern = "anticipated", 
              replacement = "announced")
# treat_dist <- treat_dist[2:8]
# names(treat_dist) <- str_c("treat_type_t", 2:8)
treat_dist <- treat_dist[c(2, 4, 3, 5, 7, 6, 8)]
names(treat_dist) <- str_c("treat_type_t", c(2, 4, 3, 5, 7, 6, 8))


# (1.4): Estimate model.
m1 <- feols(accepted_offer_ontime ~ treat_type_t2 + treat_type_t4 + treat_type_t3 + treat_type_t5 + 
              treat_type_t7 + treat_type_t6 + treat_type_t8,
            fixef = "strata_fe",
            se = "hetero",
            data = exp1_data) # Not clustered since randomization unit is the individual firm.
m2 <- feols(accepted_offer_ontime ~ fee_2.75_control + fee_3_control,
            fixef = "strata_fe",
            se = "hetero",
            data = exp1_data %>%
              mutate(fee_2.75_control = ifelse(fee_2.75 == 1, 1, 0)) %>% 
              mutate(fee_2.75_control = ifelse(control == 1, 0, fee_2.75_control)) %>%
              mutate(fee_3_control = ifelse(fee_2.75 == 0, 1, 0)) %>%
              mutate(fee_3_control = ifelse(control == 1, 0, fee_3_control)))

# Combined Panel A and Panel B
# Panel A
panel_a <- modelsummary(
  list("Panel A: Fees" = m2),
  coef_rename = c(
    "fee_2.75_control" = "2.75% Fee",
    "fee_3_control" = "3.00% Fee"
  ),
  stars = TRUE,
  output = "data.frame"
)

# Panel B
panel_b <- modelsummary(
  list("Panel B: Treatments" = m1),
  coef_rename = c(
    "treat_type_t2" = "No deadline, no reminder",
    "treat_type_t4" = "No deadline, unannounced reminder",
    "treat_type_t3" = "No deadline, announced reminder",
    "treat_type_t5" = "Deadline, no reminder",
    "treat_type_t7" = "Deadline, unannounced reminder",
    "treat_type_t6" = "Deadline, announced reminder",
    "treat_type_t8" = "Same-day deadline, no reminder"
  ),
  stars = TRUE,
  output = "data.frame"
)

# Align column names to avoid mismatch
colnames(panel_b) <- colnames(panel_a)

# Combine panels
combined_table <- rbind(
  cbind(" " = "Panel A", panel_a),
  cbind(" " = "Panel B", panel_b)
) |>
  filter(part == "estimates" | term == "Num.Obs.") |>
  select(-part)

variable_labels <- c(
  "treat_type_t2" = "No deadline, no reminder",
  "treat_type_t4" = "No deadline, unannounced reminder",
  "treat_type_t3" = "No deadline, announced reminder",
  "treat_type_t5" = "Deadline, no reminder",
  "treat_type_t7" = "Deadline, unannounced reminder",
  "treat_type_t6" = "Deadline, announced reminder",
  "treat_type_t8" = "Same-day deadline, no reminder"
)
# Get summary of the regressions
m1_summary <- summary(m1)
m2_summary <- summary(m2)

# Extract coefficients and standard errors for m2 (Panel A)
m2_coeff <- m2_summary$coefficients
m2_se <- m2_summary$se

# Extract coefficients and standard errors for m1 (Panel B)
m1_coeff <- m1_summary$coefficients
m1_se <- m1_summary$se

# Create LaTeX formatted output for Panel A (m2)
# Create LaTeX formatted output for Panel A (m2) without bold formatting
latex_m2 <- paste0(
  "2.75\\% fee & ", format(round(m2_coeff[1], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m2_se[1], 3), nsmall = 3), ") \\\\ \n",
  "3.00\\% fee & ", format(round(m2_coeff[2], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m2_se[2], 3), nsmall = 3), ") \\\\ \n"
)

# Create LaTeX formatted output for Panel B (m1) without bold formatting
latex_m1 <- paste0(
  variable_labels["treat_type_t2"], " & ", format(round(m1_coeff[1], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[1], 3), nsmall = 3), ") \\\\ \n",
  variable_labels["treat_type_t4"], " & ", format(round(m1_coeff[2], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[2], 3), nsmall = 3), ") \\\\ \n",
  variable_labels["treat_type_t3"], " & ", format(round(m1_coeff[3], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[3], 3), nsmall = 3), ") \\\\ \n",
  variable_labels["treat_type_t5"], " & ", format(round(m1_coeff[4], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[4], 3), nsmall = 3), ") \\\\ \n",
  variable_labels["treat_type_t7"], " & ", format(round(m1_coeff[5], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[5], 3), nsmall = 3), ") \\\\ \n",
  variable_labels["treat_type_t6"], " & ", format(round(m1_coeff[6], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[6], 3), nsmall = 3), ") \\\\ \n",
  variable_labels["treat_type_t8"], " & ", format(round(m1_coeff[7], 3), nsmall = 3), "*** \\\\ \n",
  " & (", format(round(m1_se[7], 3), nsmall = 3), ") \\\\ \n"
)
n_obs_m2 <- length(m2$fitted.values) 
n_obs_m2<- formatC(n_obs_m2, format = "d", big.mark = ",")
# Combine the results and add table structure
latex_table <- paste0(
  "\\begin{tabular}[t]{lc} \n",
  "\\toprule \n",
  "& Firm accepted offer \\\\ \n",
  "& (1) \\\\ \n",
  "\\midrule \n",
  " \\underline{Panel A: Offer value} & \\\\ \n",
  latex_m2, # Panel A results
  " \\underline{Panel B: Treatment arm} & \\\\ \n",
  latex_m1, # Panel B results
  "\\midrule \n",
  "Number of firms & ", n_obs_m2, " \\\\ \n",
  "\\bottomrule \n",
  "\\end{tabular}"
)

writeLines(latex_table, here("results", "tables", "main_results_full.tex"))

# Print the LaTeX code
cat(latex_table)

