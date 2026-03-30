#--------------------------------------------------------------------------------------------
# File name: 		      13_het_num_employee.R
# Creation date:      2022-06-09
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "survey_successful.csv"))
#   - here("proc", "fintech_fee_light.csv"))
# Files created:
#   - here("results", "tables", "het_num_employee_all_inc_deadline_treat_ontime.tex")
# Purpose:
# 	- Table 2: Heterogeneous Effects by Number of Employees: Generate tables estimating treatment heterogeneity by number of employees.
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
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
#########################################################

###################################
##    (1): Process survey data.  ##
###################################
# (1.1): Import survey data and merge with treatment assignment.
data_nb_employees <- read_csv(here("proc", "survey_successful.csv"), show_col_types = FALSE) %>% 
  select(organization_uuid, req_ans_q1.1, nb_employees, reminder, deadline, anticipated_reminder, unanticipated_reminder)
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv"), show_col_types = FALSE) %>% 
  select(organization_uuid, accepted_offer_late, accepted_offer_ontime, accepted_offer_firstday, takeup_date, 
         treat_type, treat_description,
         no_reminder, no_deadline, strata_fe)
data_nb_employees %<>% left_join(fintech_fee, by = "organization_uuid")

# (1.2): Keep firms that answered median number of employees question.
data_nb_employees %<>% 
  filter(nb_employees != -888 & !is.na(nb_employees))  
data_nb_employees %>% tab(nb_employees)

# (1.3): Confirm there are no control and same-day deadline, no reminder groups.
data_nb_employees %>% tab(treat_description)
# Drop 1 observation from control group
data_nb_employees %<>% filter(treat_description != "Control")
assert_that(data_nb_employees %>% tab(treat_description) %>% arrange(treat_description) %>% nrow == 6)

# (1.4): Define above/below median number of employees.
# Get median number of employees (no weights)
data_nb_employees %>% pull(nb_employees) %>% median(na.rm = TRUE)
data_nb_employees %<>% 
  mutate(above_median_employees = ifelse(nb_employees >= median(nb_employees, na.rm = TRUE), 1, 0),
         more_one_employee = ifelse(nb_employees > 1, 1, 0))
# Confirm correct variable definition
data_nb_employees %>% 
  tab(above_median_employees, nb_employees) %>% 
  arrange(nb_employees) %>% 
  mutate(cum_prop = cumsum(prop))

## Ontime
reg_check_above_median_emp_reminder <- feols(fml = accepted_offer_ontime ~ above_median_employees*Treatment, 
                                             data = data_nb_employees |>
                                               filter(reminder == 1 | no_reminder == 1) |> rename(Treatment = reminder),
                                             se = "hetero")

reg_check_more_one_emp_reminder <- feols(fml = accepted_offer_ontime ~ more_one_employee*Treatment, 
                                         data = data_nb_employees |>
                                           filter(reminder == 1 | no_reminder == 1) |> rename(Treatment = reminder),
                                         se = "hetero")

reg_check_above_median_emp_antrem <- feols(fml = accepted_offer_ontime ~ above_median_employees*Treatment, 
                                           data = data_nb_employees |>
                                             filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> rename(Treatment = anticipated_reminder),
                                           se = "hetero")
reg_check_more_one_emp_antrem <- feols(fml = accepted_offer_ontime ~ more_one_employee*Treatment, 
                                       data = data_nb_employees |>
                                         filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> rename(Treatment = anticipated_reminder),
                                       se = "hetero")

reg_check_above_median_emp_deadline <- feols(fml = accepted_offer_ontime ~ above_median_employees*Treatment, 
                                             data = data_nb_employees |>
                                               filter(deadline == 1 | no_deadline == 1) |> rename(Treatment = deadline),
                                             se = "hetero")
reg_check_more_one_emp_deadline <- feols(fml = accepted_offer_ontime ~ more_one_employee*Treatment, 
                                         data = data_nb_employees |>
                                           filter(deadline == 1 | no_deadline == 1) |> rename(Treatment = deadline),
                                         se = "hetero")



mean_values <- list(
  mean_above_median_emp_reminder = data_nb_employees |> 
    filter(reminder == 1 | no_reminder == 1) |> 
    summarise(mean = mean(above_median_employees, na.rm = TRUE)) |> 
    pull(mean),
  mean_more_one_emp_reminder = data_nb_employees |> 
    filter(reminder == 1 | no_reminder == 1) |> 
    summarise(mean = mean(more_one_employee, na.rm = TRUE)) |> 
    pull(mean),
  mean_above_median_emp_antrem = data_nb_employees |> 
    filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> 
    summarise(mean = mean(above_median_employees, na.rm = TRUE)) |> 
    pull(mean),
  mean_more_one_emp_antrem = data_nb_employees |> 
    filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> 
    summarise(mean = mean(more_one_employee, na.rm = TRUE)) |> 
    pull(mean),
  mean_above_median_emp_deadline = data_nb_employees |> 
    filter(deadline == 1 | no_deadline == 1) |> 
    summarise(mean = mean(above_median_employees, na.rm = TRUE)) |> 
    pull(mean),
  mean_more_one_emp_deadline = data_nb_employees |> 
    filter(deadline == 1 | no_deadline == 1) |>
    summarise(mean = mean(more_one_employee, na.rm = TRUE)) |> 
    pull(mean)
)
formatted_means <- sprintf("%.3f", unlist(mean_values))



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
  "above_median_employees" = "Above-median \\# of employees",
  "more_one_employee" = "More than 1 employee",
  "above_median_employees:Treatment" = "Above-median \\# of employees $\\times$ Treatment",
  "more_one_employee:Treatment" = "More than 1 employee $\\times$ Treatment"
)

results_list <- list(
  reg_check_above_median_emp_reminder,
  reg_check_more_one_emp_reminder,
  
  reg_check_above_median_emp_antrem,
  reg_check_more_one_emp_antrem,
  
  reg_check_above_median_emp_deadline,
  reg_check_more_one_emp_deadline
)

model_names <- c(
  "(1)", "(2)", "(3)",
  "(4)", "(5)", "(6)"
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


num_firms <- purrr::map_chr(results_list, ~ sprintf("%d", nobs(.)))
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
  "Mean heterogeneity variable & ", paste(formatted_means, collapse = " & "), " \\\\\n",
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

writeLines(modified_latex_code, here("results", "tables", "het_num_employee_all_inc_deadline_treat_ontime.tex"))

