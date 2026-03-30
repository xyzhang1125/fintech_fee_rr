#--------------------------------------------------------------------------------------------
# File name: 		      12_het_survey_pct_sales.R
# Creation date:      2022-10-01
# Authors:          	César Landín & Mohammad Atif Haidry
# Files used:
#   - here("proc", "survey_successful.csv")
#   - here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("results", "numbers", "survey_num_value_transactions_abovezero.tex")
#   - here("results", "numbers", "survey_num_value_transactions_missing.tex")
#   - here("results", "numbers", "survey_num_value_transactions_zero.tex")
#   - here("results", "numbers", "survey_num_value_transactions_does_not_know.tex")
#   - here("results", "numbers", "survey_num_value_transactions_refuses_to_answer.tex")
#   - here("results", "numbers", "survey_num_percent_sales_refuses_to_answer.tex")
#   - here("results", "numbers", "survey_num_percent_sales_does_not_know.tex")
#   - here("results", "numbers", "survey_num_percent_sales_valid_answer.tex")
#   - here("results", "numbers", "survey_num_percent_sales_using_tech_full.tex")
#   - here("results", "tables", "het_pct_sales_owner_takeup_ontime.tex")
# Purpose:
#   - Creates a table with version that combines table C.6 and table C.7 from within the NBER paper
# 	- Estimate treatment heterogeneity by percentage sales through Zettle.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(kableExtra)
library(assertthat)
library(spatstat)
library(modelsummary)
library(scales)
library(fixest)
library(cowplot)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
#########################################################

###################################
##    (1): Process survey data.  ##
###################################
# 1.8. What is the approximate value of all transactions you have made through fintech in the past week? 
# 1.9. What share of your total pesos of sales did you make through fintech in the past week? 

# Filter for question 1.8: all firms
# Filter for question 1.9: value_transactions > 0 & !is.na(value_transactions)

# (1.1): Import survey data and merge with treatment assignment.
data_percent_sales <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, req_ans_q1.8, req_ans_q1.9, value_transactions, percent_sales, 
         reminder, deadline, anticipated_reminder, unanticipated_reminder) %>% 
  filter(req_ans_q1.8 == 1)
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, accepted_offer_late, accepted_offer_ontime, open_email_late, takeup_date, 
         treat_type, treat_description,
         no_reminder, no_deadline, control, strata_fe, fee_2.75)
data_percent_sales %<>% left_join(fintech_fee, by = "organization_uuid")


# (1.2): Plot raw survey responses.
# Process survey data
data_percent_sales %<>%
  mutate(answer_status_value = case_when(value_transactions == 0 ~ "Provided a zero valid answer",
                                         value_transactions == -777 ~ "Does not know",
                                         value_transactions == -888 ~ "Refuses to answer",
                                         TRUE ~ "Provided a non-zero valid answer"),
         answer_status_percent = case_when((value_transactions %in% c(-777, -888)) | value_transactions == 0 ~ NA_character_,
                                           percent_sales == -777 ~ "Does not know",
                                           percent_sales == -888 ~ "Refuses to answer",
                                           TRUE ~ "Provided a valid answer")) %>% 
  mutate_at(vars(contains("answer_status")), ~factor(., levels = c("Refuses to answer", "Does not know", "Provided a valid answer", 
                                                                   "Provided a zero valid answer", "Provided a non-zero valid answer")))
data_percent_sales %>% tab(answer_status_value)
data_percent_sales %>% tab(answer_status_percent)

data_percent_sales %>% 
  filter(value_transactions != 0, value_transactions != -777, value_transactions != -888, !is.na(value_transactions)) %>%
  nrow() %>%
  print_n("survey_num_value_transactions_abovezero.tex", "Above zero answer to the question - What is the approximate value of all transactions you have made through the technology in the past week?", dig = 1)

data_percent_sales %>% 
  filter(is.na(value_transactions)) %>%
  nrow() %>%
  print_n("survey_num_value_transactions_missing.tex", "Missing answer to the question - What is the approximate value of all transactions you have made through the technology in the past week?", dig = 1)

data_percent_sales %>% 
  filter(value_transactions == 0) %>%
  nrow() %>%
  print_n("survey_num_value_transactions_zero.tex", "Provided a zero valid answer to the question - What is the approximate value of all transactions you have made through the technology in the past week?", dig = 1)


data_percent_sales %>% 
  filter(value_transactions == -777) %>%
  nrow() %>%
  print_n("survey_num_value_transactions_does_not_know.tex", "Provided a 'does not know' answer to the question - What is the approximate value of all transactions you have made through the technology in the past week?", dig = 1)

data_percent_sales %>% 
  filter(value_transactions == -888) %>%
  nrow() %>%
  print_n("survey_num_value_transactions_refuses_to_answer.tex", "Provided a 'refuses to answer' to the question - What is the approximate value of all transactions you have made through the technology in the past week?", dig = 1)

data_percent_sales %>% 
  filter(percent_sales == -888) %>%
  nrow() %>%
  print_n("survey_num_percent_sales_refuses_to_answer.tex", "Provided a 'refuses to answer' to the question - “What share of your total pesos of sales did you make through the technology in the past week?", dig = 1)

data_percent_sales %>% 
  filter(percent_sales == -777) %>%
  nrow() %>%
  print_n("survey_num_percent_sales_does_not_know.tex", "Provided a 'does not know' to the question - “What share of your total pesos of sales did you make through the technology in the past week?", dig = 1)

data_percent_sales %>% 
  filter(percent_sales != -777, percent_sales != -888, !is.na(percent_sales)) %>%
  nrow() %>%
  print_n("survey_num_percent_sales_valid_answer.tex", "Provided a valid answer to the question - “What share of your total pesos of sales did you make through the technology in the past week?", dig = 1)

# (1.3): Drop don't know / did not answer and replace zeros in percent sales.
# Firms that reported 0 sales were not asked percent sales question.
data_percent_sales %<>%
  filter(value_transactions >= 0 / !(percent_sales %in% c(-777, -888))) %>% 
  mutate(percent_sales = ifelse(value_transactions == 0, 0, percent_sales),
         percent_sales = percent_sales / 100)
data_percent_sales %>% 
  nrow() %>%
  print_n("survey_num_percent_sales_using_tech_full.tex", "Variable constructed from the questions - What is the approximate value of all transactions you have made through the technology in the past week? and What share of your total pesos of sales did you make through the technology in the past week?", dig = 1)

# (1.4): Confirm there are no control and same-day deadline, no reminder groups.
data_percent_sales %>% tab(treat_description)
# Drop 1 observation from control group
data_percent_sales %<>% filter(treat_description != "Control")
assert_that(data_percent_sales %>% tab(treat_description) %>% arrange(treat_description) %>% nrow == 6)

# (1.4): Define above/below median percentage sales using technology.
data_percent_sales %>% pull(percent_sales) %>% median(na.rm = TRUE)
data_percent_sales %<>% 
  mutate(above_median_percent_sales = ifelse(percent_sales >= median(percent_sales, na.rm = TRUE), 1, 0)) %>%
  mutate(below_median_percent_sales = ifelse(percent_sales < median(percent_sales, na.rm = TRUE), 1, 0))
# Confirm correct variable definition
data_percent_sales %>% 
  tab(above_median_percent_sales, percent_sales) %>% 
  arrange(percent_sales) %>% 
  mutate(cum_prop = cumsum(prop))

rm(fintech_fee)

####################################################
##    (2): Define table function and parameters.  ##
####################################################
# (2.1): Define coefficient names.
long_treat_names <- c("Reminder", "Announced reminder", "Deadline")
treat_vars <- c("reminder", "anticipated_reminder", "deadline")
coef_names <- c("Intercept",
                "Above median \\% sales using technology",
                str_c(rep(c("", "Above median \\% sales using technology times "), times = 3), rep(long_treat_names, each = 2)))
names(coef_names) <- c("(Intercept)",
                       "above_median_percent_sales",
                       str_c(rep(c("", "above_median_percent_sales:"), times = 3), rep(treat_vars, each = 2)))

# (2.2): Define parameters for different tables.
params <- tribble(~treatment, ~data,
                  "reminder", data_percent_sales %>% filter(reminder == 1 | no_reminder == 1),
                  "anticipated_reminder", data_percent_sales %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1),
                  "deadline", data_percent_sales %>% filter(deadline == 1 | no_deadline == 1))

# (2.3): Define regression function.
reg_basic <- function(var, data = NULL, het_var = "", treat = "", wt = "") {
  if (is.null(data)) {
    data <- params %>% filter(treatment == treat) %>% pull(data) %>% .[[1]]
  }
  model <- paste(var, "~", het_var, "*", treat)
  if (wt != "") {current_weight <- ~ipw_takeup}
  else {current_weight <- 1}
  reg <- feols(fml = as.formula(model),
               data = data,
               se = "hetero",
               weights = current_weight,
               lean = FALSE)
  return(reg)
}


# Treatment (ontime version)
survey_data <- read_csv(here("proc", "survey_successful.csv")) %>% 
  replace_na(list(owner_receives_emails = 0)) %>% 
  left_join(read_csv(here("proc", "fintech_fee_light.csv")) %>% 
              select(organization_uuid, accepted_offer_late, accepted_offer_ontime, no_reminder, no_deadline, fast_deadline),
            by = "organization_uuid")
survey_data %>% tab(owner_receives_emails)


reg_basic_rem_above_med_pct_sales <- feols(fml = accepted_offer_ontime ~ above_median_percent_sales*Treatment,
                                           se = "hetero",
                                           data =  data_percent_sales |>
                                             filter(reminder == 1 | no_reminder == 1) |>
                                             rename(Treatment = reminder))
reg_basic_antrem_above_med_pct_sales <- feols(fml = accepted_offer_ontime ~ above_median_percent_sales*Treatment,
                                              se = "hetero",
                                              data =  data_percent_sales |>
                                                filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |>
                                                rename(Treatment = anticipated_reminder))
reg_basic_deadline_above_med_pct_sales <- feols(fml = accepted_offer_ontime ~ above_median_percent_sales*Treatment,
                                                se = "hetero",
                                                data =  data_percent_sales |>
                                                  filter(deadline == 1 | no_deadline == 1) |>
                                                  rename(Treatment = deadline))

reg_basic_rem_owner_receives <- feols(fml = accepted_offer_ontime ~ owner_receives_emails*Treatment,
                                      se = "hetero",
                                      data =  survey_data |>
                                        filter(reminder == 1 | no_reminder == 1)|>
                                        rename(Treatment = reminder))
reg_basic_antrem_owner_receives <- feols(fml = accepted_offer_ontime ~ owner_receives_emails*Treatment,
                                         se = "hetero",
                                         data =  survey_data |>
                                           filter(anticipated_reminder == 1 | unanticipated_reminder == 1)|>
                                           rename(Treatment = anticipated_reminder))
reg_basic_deadline_owner_receives <- feols(fml = accepted_offer_ontime ~ owner_receives_emails*Treatment,
                                           se = "hetero",
                                           data =  survey_data |>
                                             filter(deadline == 1 | no_deadline == 1)|>
                                             rename(Treatment = deadline))

extract_coeffs_se_pvalue_dataset <- function(model, model_name) {
  # Extract coefficients, standard errors, and p-values
  coef_data <- model$coefficients
  se_data <- model$se
  p_values <- model$coeftable[, "Pr(>|t|)"]
  num_firms <- nobs(model)  # Number of observations
  
  # Add stars based on p-values
  stars <- ifelse(
    p_values < 0.01, "***",
    ifelse(p_values < 0.05, "**",
           ifelse(p_values < 0.1, "*", ""))
  )
  coef_starred <- paste0(sprintf("%.3f", coef_data), stars)
  
  # Combine into a data frame
  df <- data.frame(
    Variable = names(coef_data),
    Coefficient = coef_data,
    Coefficient_Starred = coef_starred,
    Std_Error = se_data,
    P_Value = p_values,
    Num_Firms = num_firms,
    Model = model_name
  )
  
  return(df)
}

rename_treatments <- c(
  "(Intercept)" = "Intercept",
  "Treatment" = "Treatment",
  "above_median_percent_sales" = "Above-median \\% sales using technology",
  "above_median_percent_sales:Treatment" = "Above-median \\% sales using technology $\\times$ Treatment",
  "owner_receives_emails" = "Owner was recipient of emails",
  "owner_receives_emails:Treatment" = "Owner was recipient of emails $\\times$ Treatment"
)



results_list <- list(
  reg_basic_rem_above_med_pct_sales,
  reg_basic_rem_owner_receives,
  reg_basic_antrem_above_med_pct_sales,
  reg_basic_antrem_owner_receives,
  reg_basic_deadline_above_med_pct_sales,
  reg_basic_deadline_owner_receives
)

model_names <- c(
  "(1)", "(2)", "(3)", "(4)", "(5)", "(6)"
)

# Use the function to extract data for all models
all_results <- purrr::map2_dfr(results_list, model_names, extract_coeffs_se_pvalue_dataset)

all_results <- all_results %>%
  mutate(Variable = recode(Variable, !!!rename_treatments),
         Variable = sprintf("%s", Variable))

table_data <- all_results %>%
  select(Model, Variable, Coefficient_Starred, Std_Error) %>%
  mutate(Coefficient_Starred = sprintf("%s", Coefficient_Starred),
         Std_Error = sprintf("(%.3f)", Std_Error)) %>%
  pivot_wider(
    names_from = Model,
    values_from = c(Coefficient_Starred, Std_Error),
    names_glue = "{Model}_{.value}"
  ) %>%
  slice(c(1, 3, 2, 4:n()))


num_firms <- purrr::map_chr(results_list, ~ comma(nobs(.)))

# remove headers until tabular
latex_code <- paste0(
  "\\begin{tabular}{l", paste(rep("c", 6), collapse = ""), "}\n",
  "\\toprule\n",
  " & \\multicolumn{6}{c}{Firm accepted offer} \\\\\n",
  "\\cmidrule(lr){2-7}\n",
  "\\multicolumn{1}{r}{Treatment:} & \\multicolumn{2}{c}{Reminder} & \\multicolumn{2}{c}{Announced reminder} & \\multicolumn{2}{c}{Deadline} \\\\\n",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}\n",
  " & ", paste(model_names, collapse = " & "), " \\\\\n",
  "\\midrule\n",
  paste(
    apply(
      table_data %>%
        mutate(across(-Variable, ~replace_na(.x, ""))) %>%
        select(Variable, ends_with("Coefficient_Starred"), ends_with("Std_Error")),
      1,
      function(row) {
        n <- 7
        row_content <- paste(row, collapse = " & ")
        parts <- strsplit(row_content, " & ")
        for (i in seq(n, length(parts[[1]]), by = n + 1)) {
          parts[[1]][i] <- paste(parts[[1]][i], "\\\\")
        }
        paste(parts[[1]], collapse = " & ")
      }
    ),
    collapse = " \\\\\n"
  ),
  " \\\\\n",
  "\\midrule\n",
  "Number of firms & ", paste(num_firms, collapse = " & "), " \\\\\n",
  "\\bottomrule\n",
  "\\end{tabular}\n"
)
cat(latex_code)



# Split the LaTeX code into individual lines
lines <- strsplit(latex_code, "\n")[[1]]

# Initialize a vector to store the modified lines
modified_lines <- c()

# Iterate through each line
for (line in lines) {
  
  # Check if the line contains "$\\times$ Treatment"
  if (grepl("\\$\\\\times\\$ Treatment", line)) {
    
    # Split the line into coefficient and standard error parts at "\\\\"
    parts <- strsplit(line, "\\\\\\\\")[[1]]
    
    # Ensure that the line has both coefficient and standard error parts
    if (length(parts) >= 2) {
      coef_part <- parts[1]
      se_part <- parts[2]
      
      # Remove "$\\times$ Treatment" from the coefficient part
      coef_part_new <- sub("\\s*\\$\\\\times\\$ Treatment", "", coef_part)
      
      # Add "$\\times$ Treatment" with spacing to the standard error part
      se_part_new <- sub("^\\s*&", "\\\\hspace{1em} $\\\\times$ Treatment &", se_part)
      
      # Append the modified coefficient line with "\\\\"
      modified_coef_line <- paste0(coef_part_new, " \\\\")
      
      # Append the modified standard error line with "\\\\"
      modified_se_line <- paste0(se_part_new, " \\\\")
      
      # Add the modified lines to the list
      modified_lines <- c(modified_lines, modified_coef_line, modified_se_line)
      
    } else {
      # If the line doesn't have a standard error part, retain it as is
      modified_lines <- c(modified_lines, line)
    }
    
  } else {
    # If "$\\times$ Treatment" is not in the line, retain the line as is
    modified_lines <- c(modified_lines, line)
  }
}

# Combine the modified lines back into a single LaTeX string
modified_latex_code <- paste(modified_lines, collapse = "\n")



# Optionally, print the modified LaTeX code
cat(modified_latex_code)

writeLines(modified_latex_code, here("results", "tables", "het_pct_sales_owner_takeup_ontime.tex"))
