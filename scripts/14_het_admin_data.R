#--------------------------------------------------------------------------------------------
# File name: 		      14_het_admin_data.R
# Creation date:      2023-01-15
# Author:          		César Landín
# Files used:
#   - here("proc", "balance_table_data.csv")
#   - here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
#   - here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("results", "numbers", "stat_pct_change_sale_median_rem.tex")
#   - here("results", "numbers", "stat_pct_change_sale_median_antrem.tex")
#   - here("results", "numbers", "stat_pct_change_sale_median_dl.tex")
#   - here("results", "tables", "het_admin_data_buss_type_all.tex")
#   - here("results", "tables", "het_admin_data_main_rem_antrem_dl_ontime.tex")
# Purpose:
#   - Table C.8: Heterogeneous Effects by Firm Business Type
#   - Creates a table with version that combines table C.9, table C.10, and table C.11 from within the NBER paper
# 	- Process administrative data and generate administrative data heterogeneity tables.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(fixest)
library(qs)
library(tabulator)
library(lubridate)
library(data.table)
library(modelsummary)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "winsorize.R"))
source(here("scripts", "programs", "myfunctions.R"))
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
# group_colors <- read_csv(here("proc", "colors.csv"))
#########################################################

##########################################################
##    (1): Process admin data for heterogeneity table.  ##
##########################################################
# (1.1): Import balance table data. 
balance_data <- read_csv(here("proc", "balance_table_data.csv"))

# (1.2): Calculate percentage change in sales from August to September 2020 (with adjusted dates).
change_sales <- qread(here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")) %>% 
  select(organization_uuid, timestamp_month, valid_volume, treat_type) %>%  # Use non-winsorized variable for change in sales
  filter(timestamp_month >= "2020-08-01" & timestamp_month <= "2020-09-01") %>% 
  pivot_wider(names_from = timestamp_month, 
              values_from = valid_volume) %>% 
  set_names(c("organization_uuid", "treat_type", "sales_aug", "sales_sep")) %>% 
  mutate(change_sales = (sales_sep - sales_aug) / sales_aug)

# (1.3): Get winsorized change in sales.
change_sales %>% 
  summarise(min_change = min(change_sales),
            max_change = max(change_sales))
change_sales %<>%
  as.data.table() %>% 
  winsorize(outcome = "change_sales",
            newvar = "change_sales_w5", 
            by = c("treat_type"),
            highonly = TRUE) %>% 
  as_tibble()
change_sales %>% 
  ggplot() +
  geom_histogram(aes(x = change_sales_w5)) + 
  theme_minimal()

# (1.4): Calculate above/below median change in sales.
median_change_sales <- change_sales %>% pull(change_sales_w5) %>% median()
change_sales %<>%
  mutate(above_median_change_sales = ifelse(change_sales_w5 >= median_change_sales, 1, 0))
change_sales %>% tab(above_median_change_sales)


# (1.5): Import offer acceptance variable and merge all datasets.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv"), show_col_types = FALSE) %>% 
  select(organization_uuid, accepted_offer_late, accepted_offer_ontime, reminder, no_reminder, no_deadline, strata_fe)
het_admin_data <- balance_data %>% 
  left_join(fintech_fee, by = "organization_uuid") %>% 
  left_join(change_sales %>% 
              select(organization_uuid, above_median_change_sales),
            by = "organization_uuid")

rm(balance_data, fintech_fee)

# (1.7): Redefine owner sex variable.
het_admin_data %<>% mutate(owner_sex_female = ifelse(owner_sex_unknown == 1, NA, owner_sex_female))

# (1.8): Define above median owner age variable.
median_age <- het_admin_data %>% filter(!is.na(owner_age)) %>% pull(owner_age) %>% median()
het_admin_data %<>% mutate(above_median_owner_age = ifelse(owner_age >= median_age, 1, 0))
het_admin_data %>% tab(above_median_owner_age)

change_sales %>%
  left_join(het_admin_data, by = "organization_uuid") |>
  filter(reminder == 1 | no_reminder == 1) %>%
  summarise(median_change_in_sales = median(change_sales_w5, na.rm = TRUE)) %>% 
  print_pct("stat_pct_change_sale_median_rem.tex", "Percentage (Median Change in Sales), reminder and no reminder group", dig = 0)

change_sales %>%
  left_join(het_admin_data, by = "organization_uuid") |>
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>%
  summarise(median_change_in_sales = median(change_sales_w5, na.rm = TRUE)) %>% 
  print_pct("stat_pct_change_sale_median_antrem.tex", "Percentage (Median Change in Sales), anticipated and unanticipated reminder group", dig = 0)

change_sales %>%
  left_join(het_admin_data, by = "organization_uuid") |>
  filter(deadline == 1 | no_deadline == 1) %>%
  summarise(median_change_in_sales = median(change_sales_w5, na.rm = TRUE)) %>% 
  print_pct("stat_pct_change_sale_median_dl.tex", "Percentage (Median Change in Sales), deadline and no deadline group", dig = 0)


##########################################################################
##    (2): Generate baseline firm characteristics heterogeneity table.  ##
##########################################################################
# (2.1): Define regression function.
reg_basic <- function(ind_var, model, data) {
  model <- paste("accepted_offer_late", "~", model, "+", ind_var)
  reg <- feols(fml = as.formula(model),
               data = data,
               se = "hetero")
  return(reg)
}

# (2.2): Define regression parameters.
# * Heterogeneity by age of owner (above/below median age of owner)
# * Heterogeneity by gender (female vs. male)
# * Heterogeneity by pre-experiment growth in sales (compare vs. month previous to experiment: september to august, since firms won't all have months)
# * Heterogeneity by industry (one industry vs. others, do a full table)

# General parameters
params <- tribble(~var_name,  ~var_label, ~var_group,
                  "above_median_owner_age", "Above-median manager age", "main",
                  "owner_sex_female", "Female manager", "main", 
                  "above_median_change_sales", "Above-median baseline change in sales", "main",
                  "new_buss_type_small_retailers", "Small retailers", "buss_type",
                  "new_buss_type_professionals", "Professionals", "buss_type",
                  "new_buss_type_beauty", "Beauty", "buss_type",
                  "new_buss_type_clothing", "Clothing", "buss_type", 
                  "new_buss_type_restaurants", "Restaurants", "buss_type", 
                  "new_buss_type_other", "Other", "buss_type")
# Add interactions with treatment
params %<>% 
  bind_rows(params %>% 
              mutate(var_name = str_c("treat_var_name:", var_name),
                     var_label = paste(var_label, "times treat_var_label")))

# (2.3): Calculate means for footnote.
mean_male <- het_admin_data %>% pull(owner_sex_male) %>% mean()

# (2.4): Functions for defining short and long variable names.
short_name <- function(treat) {
  case_when(treat == "reminder" ~ "rem",
            treat == "anticipated_reminder" ~ "antrem",
            treat == "deadline" ~ "dl")
}
long_name <- function(treat) {
  case_when(treat == "reminder" ~ "Reminder",
            treat == "anticipated_reminder" ~ "Announced reminder", 
            treat == "deadline" ~ "Deadline")
}

# (2.5): Loop over three treatments.
# Define treatment variable names and data used for each treatment
table_params <- tribble(~treatment, ~data,
                        "reminder", het_admin_data %>% filter(reminder == 1 | no_reminder == 1),
                        "anticipated_reminder", het_admin_data %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1),
                        "deadline", het_admin_data %>% filter(deadline == 1 | no_deadline == 1))
# # Loop over treatments
# for (i in 1:nrow(table_params)) {
#   # Define current treatment
#   current_treatment <- table_params$treatment[i]
# 
#   # Define current set of variables
#   current_params <- params %>% 
#     mutate_all(~str_replace(., "treat_var_name", current_treatment)) %>% 
#     mutate_all(~str_replace(., "treat_var_label", long_name(current_treatment)))
#   
#   # Generate vector with coefficient names
#   coef_names <- c("Reminder", "Announced reminder", "Deadline") %>% 
#     append(current_params$var_label)
#   names(coef_names) <- c("reminder", "anticipated_reminder", "deadline") %>% 
#     append(current_params$var_name)
#   
#   
#   
#   if (i == 3) {
#     custom_order <- c(
#       "Deadline",
#       "Reminder",
#       "Announced reminder",
#       "Above-median manager age",
#       "Above-median manager age times Deadline",
#       "Female manager",
#       "Female manager times Deadline",
#       "Above-median baseline change in sales",
#       "Above-median baseline change in sales times Deadline",
#       "Small retailers",
#       "Small retailers times Deadline",
#       "Professionals",
#       "Professionals times Deadline",
#       "Beauty",
#       "Beauty times Deadline",
#       "Clothing",
#       "Clothing times Deadline",
#       "Restaurants",
#       "Restaurants times Deadline",
#       "Other",
#       "Other times Deadline"
#     )
#   } else if (i == 1) {
#     custom_order <- c(
#       "Deadline",
#       "Reminder",
#       "Announced reminder",
#       "Above-median manager age",
#       "Above-median manager age times Reminder",
#       "Female manager",
#       "Female manager times Reminder",
#       "Above-median baseline change in sales",
#       "Above-median baseline change in sales times Reminder",
#       "Small retailers",
#       "Small retailers times Reminder",
#       "Professionals",
#       "Professionals times Reminder",
#       "Beauty",
#       "Beauty times Reminder",
#       "Clothing",
#       "Clothing times Reminder",
#       "Restaurants",
#       "Restaurants times Reminder",
#       "Other",
#       "Other times Reminder"
#     )
#   } else if (i == 2) {
#     custom_order <- c(
#       "Deadline",
#       "Reminder",
#       "Announced reminder",
#       "Above-median manager age",
#       "Above-median manager age times Announced reminder",
#       "Female manager",
#       "Female manager times Announced reminder",
#       "Above-median baseline change in sales",
#       "Above-median baseline change in sales times Announced reminder",
#       "Small retailers",
#       "Small retailers times Announced reminder",
#       "Professionals",
#       "Professionals times Announced reminder",
#       "Beauty",
#       "Beauty times Announced reminder",
#       "Clothing",
#       "Clothing times Announced reminder",
#       "Restaurants",
#       "Restaurants times Announced reminder",
#       "Other",
#       "Other times Announced reminder"
#     )
#   }  
#   # Convert coef_names to a factor with the specified order
#   coef_names_ordered <- factor(coef_names, levels = custom_order)
#   
#   # Sort coef_names based on the ordered factor
#   coef_names_sorted <- coef_names[order(coef_names_ordered)]
#   
#   # Output the sorted coef_names
#   print(coef_names_sorted)
#   
#   
#   
#   
#   # Collapse business types for model
#   buss_type_vars <- current_params %>% 
#     filter(var_group == "buss_type" & !str_detect(var_name, "other")) %>% 
#     pull(var_name) %>% 
#     str_c(collapse = " + ")
#   
#   # First model: baseline variables
#   model_baseline <- lapply(current_params %>% filter(var_group != "buss_type" & !str_detect(var_label, "times")) %>% pull(var_name), reg_basic, 
#                            model = paste(current_treatment, "*"),
#                            data = table_params$data[i][[1]])
#   
#   # Define label and file name
#   current_filename <- str_c("het_admin_data_main_", short_name(current_treatment))
#   
#   # Save table
#   gen_base_table(models = model_baseline, 
#                  caption = paste("Heterogeneous Effects of", str_to_title(long_name(current_treatment)), "by Baseline Characteristics"),
#                  label = current_filename,
#                  title = "Firm accepted offer",
#                  coef_names = coef_names_sorted) %>% 
#     coef_two_rows() %>% 
#     add_line_space(0.2) %>% 
#     adjust_col_spacing(8) %>% 
#     adjust_height(0.6) %>% 
#     write_(here("results", "tables", str_c(current_filename, ".tex")))
# }

##########################################################
##    (3): Generate business type heterogeneity table.  ##
##########################################################
# (3.1): Define current set of variables.
current_params <- params %>% 
  mutate_all(~str_replace(., "treat_var_name", "reminder")) %>% 
  bind_rows(params %>% 
              mutate_all(~str_replace(., "treat_var_name", "anticipated_reminder"))) %>%
  bind_rows(params %>% 
              mutate_all(~str_replace(., "treat_var_name", "deadline"))) %>% 
  mutate_all(~str_replace(., "treat_var_label", "Treatment")) %>% 
  unique()

# (3.2): Collapse business types for model.
buss_type_vars <- current_params %>% 
  filter(var_group == "buss_type" & !str_detect(var_name, "other") & !str_detect(var_name, "reminder|deadline|anticipated_reminder")) %>% 
  pull(var_name)

# (3.3): Generate vector with coefficient names.
coef_names <- c("Treatment", "Treatment", "Treatment") %>% 
  append(current_params$var_label)
names(coef_names) <- c("reminder", "anticipated_reminder", "deadline") %>% 
  append(current_params$var_name)

current_filename <- "het_admin_data_buss_type_all"

# (3.4): Second model: business type variables.
reg_basic_fixef <- function(ind_var, model, data) {
  model <- paste("accepted_offer_ontime", "~", model, "+", ind_var)
  reg <- feols(fml = as.formula(model),
               data = data,
               fixef = "strata_fe",
               se = "hetero")
  return(reg)
}

bus_type_regs <- function(treat) {
  reg_basic_fixef(ind_var = buss_type_vars %>% str_c(collapse = " + "),
            model = paste(treat, "+", 
                          str_c(treat, ":", buss_type_vars, collapse = " + ")),
            data = table_params %>% filter(treatment == treat) %>% pull(data) %>% .[[1]])
}
model_buss_type <- lapply(table_params$treatment, bus_type_regs)

# (3.5): Save table.
names(model_buss_type) <- c("Reminder", "Announced reminder", "Deadline")
gen_base_table(models = model_buss_type, 
               caption = "Heterogeneous Effects by Firm Business Type",
               label = current_filename,
               title = "Firm accepted offer",
               cline = TRUE,
               coef_names = coef_names) %>% 
  coef_two_rows() %>% 
  #add_line_space(0.2) %>% 
  #adjust_height(0.7) %>% 
  write_(here("results", "tables", str_c(current_filename, ".tex")))



# Custom function to modify the LaTeX file
add_vspace_after_cline <- function(file_path) {
  # Read the file content
  table_content <- readLines(file_path)
  
  # Use gsub to insert \\[.5ex] after the multicolumn line
  #table_content <- gsub("(\\\\multicolumn\\{3\\}\\{c\\}\\{Firm accepted offer\\} \\\\\\\\)", "\\\\multicolumn\\{3\\}\\{c\\}\\{Firm accepted offer\\} \\\\\\\\ \\[.5ex]", table_content)
  
  table_content <- gsub("\\\\cline\\{2-4\\}", "\\\\cmidrule(lr)\\{2-4\\}" , table_content)
  table_content <- gsub("\\\\vphantom\\{1\\}", "" , table_content)
  table_content <- gsub("\\\\adjustbox\\{max width=\\\\textwidth\\}", "\\\\adjustbox\\{max width=1.2\\\\textwidth, center\\}", table_content)
  table_content <- gsub("& Reminder & Announced reminder & Deadline", "\\\\multicolumn\\{1\\}\\{r\\}\\{Treatment:\\} & Reminder & Announced reminder & Deadline", table_content)
  
  # Write the modified content back to the file
  writeLines(table_content, file_path)
}


# Specify the file path
file_path <- here("results", "tables", "het_admin_data_buss_type_all.tex")

# Call the function to modify the .tex file
add_vspace_after_cline(file_path)

































## Merging Heterogeneity Treatment Effects by Baseline Characteristics to have nine columns

data_check <- het_admin_data |>
  select(reminder, no_reminder, unanticipated_reminder, anticipated_reminder, deadline,
         no_deadline, owner_sex_female, accepted_offer_ontime, above_median_owner_age, 
         above_median_change_sales, strata_fe)

reg_check_owner_age_reminder <- feols(fml = accepted_offer_ontime ~ above_median_owner_age*Treatment, 
                                      data = data_check |>
                                        filter(reminder == 1 | no_reminder == 1) |> rename(Treatment = reminder),
                                      fixef = "strata_fe",
                                      se = "hetero")
reg_check_owner_sex_reminder <- feols(fml = accepted_offer_ontime ~ owner_sex_female*Treatment, 
                                      data = data_check |>
                                        filter(reminder == 1 | no_reminder == 1) |> rename(Treatment = reminder),
                                      fixef = "strata_fe",
                                      se = "hetero")
reg_check_change_sales_reminder <- feols(fml = accepted_offer_ontime ~ above_median_change_sales*Treatment, 
                                         data = data_check |>
                                           filter(reminder == 1 | no_reminder == 1) |> rename(Treatment = reminder),
                                         fixef = "strata_fe",
                                         se = "hetero")

reg_check_owner_age_deadline <- feols(fml = accepted_offer_ontime ~ above_median_owner_age*Treatment, 
                                      data = data_check |>
                                        filter(deadline == 1 | no_deadline == 1) |> rename(Treatment = deadline),
                                      fixef = "strata_fe",
                                      se = "hetero")
reg_check_owner_sex_deadline <- feols(fml = accepted_offer_ontime ~ owner_sex_female*Treatment, 
                                      data = data_check |>
                                        filter(deadline == 1 | no_deadline == 1) |> rename(Treatment = deadline),
                                      fixef = "strata_fe",
                                      se = "hetero")
reg_check_change_sales_deadline <- feols(fml = accepted_offer_ontime ~ above_median_change_sales*Treatment, 
                                         data = data_check |>
                                           filter(deadline == 1 | no_deadline == 1) |> rename(Treatment = deadline),
                                         fixef = "strata_fe",
                                         se = "hetero")

reg_check_owner_age_antrem <- feols(fml = accepted_offer_ontime ~ above_median_owner_age*Treatment, 
                                    data = data_check |>
                                      filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> rename(Treatment = anticipated_reminder),
                                    fixef = "strata_fe",
                                    se = "hetero")
reg_check_owner_sex_antrem <- feols(fml = accepted_offer_ontime ~ owner_sex_female*Treatment, 
                                    data = data_check |>
                                      filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> rename(Treatment = anticipated_reminder),
                                    fixef = "strata_fe",
                                    se = "hetero")
reg_check_change_sales_antrem <- feols(fml = accepted_offer_ontime ~ above_median_change_sales*Treatment, 
                                       data = data_check |>
                                         filter(anticipated_reminder == 1 | unanticipated_reminder == 1) |> rename(Treatment = anticipated_reminder),
                                       fixef = "strata_fe",
                                       se = "hetero")


# Function to extract regression results
extract_coeffs_se_pvalue_dataset <- function(model, model_name) {
  # Extract coefficients, standard errors, and p-values
  coef_data <- model$coefficients
  se_data <- model$se
  p_values <- model$coeftable[, "Pr(>|t|)"]
  num_firms <- nobs(model)  # Number of observations
  
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
  
  # Exclude intercept if necessary
  df <- df[!grepl("\\(Intercept\\)", df$Variable), ]
  return(df)
}

rename_treatments <- c(
  "Treatment" = "Treatment",
  "above_median_owner_age" = "Age",
  "owner_sex_female" = "Female",
  "above_median_change_sales" = "Growth",
  "above_median_owner_age:Treatment" = "Age $\\times$ Treatment",
  "owner_sex_female:Treatment" = "Female $\\times$ Treatment",
  "above_median_change_sales:Treatment" = "Growth $\\times$ Treatment"
)


results_list <- list(
  reg_check_owner_age_reminder,
  reg_check_owner_sex_reminder,
  reg_check_change_sales_reminder,
  reg_check_owner_age_antrem,
  reg_check_owner_sex_antrem,
  reg_check_change_sales_antrem,
  reg_check_owner_age_deadline,
  reg_check_owner_sex_deadline,
  reg_check_change_sales_deadline
)

model_names <- c(
  "(1)", "(2)", "(3)",
  "(4)", "(5)", "(6)",
  "(7)", "(8)", "(9)"
)

# Use the function to extract data for all models
all_results <- purrr::map2_dfr(results_list, model_names, extract_coeffs_se_pvalue_dataset)

# all_results <- all_results %>%
#   mutate(Variable = recode(Variable, !!!rename_treatments))
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
  )%>%
  slice(c(2, 1, 3:n()))

num_firms <- purrr::map_chr(results_list, ~ comma(nobs(.)))
#num_firms <- purrr::map_chr(results_list, ~ sprintf("%d", nobs(.)))
latex_code <- paste0(
  "\\begin{tabular}{l", paste(rep("c", 9), collapse = ""), "}\n",
  "\\toprule\n",
  " & \\multicolumn{9}{c}{Firm accepted offer} \\\\\n",
  "\\cmidrule(lr){2-10}\n",
  "\\multicolumn{1}{r}{Treatment:} & \\multicolumn{3}{c}{Reminder} & \\multicolumn{3}{c}{Announced reminder} & \\multicolumn{3}{c}{Deadline} \\\\\n",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-10}\n", 
  " & ", paste(model_names, collapse = " & "), " \\\\\n",
  "\\midrule\n",
  paste(
    apply(
      table_data %>% 
        mutate(across(-Variable, ~replace_na(.x, ""))) %>%
        select(Variable, ends_with("Coefficient_Starred"), ends_with("Std_Error")),
      1,
      function(row) {
        n <- 10
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

writeLines(modified_latex_code, here("results", "tables", "het_admin_data_main_rem_antrem_dl_ontime.tex"))







