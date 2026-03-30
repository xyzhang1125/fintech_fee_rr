#--------------------------------------------------------------------------------------------
# File name: 		      15c_survey_measure_tables.R
# Creation date:      2023-03-28
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "survey_successful.csv")
# 	- here("proc", "fintech_fee_light.csv")
# 	- here("results", "tables", "romano_p_antrem_unantrem.tex")
# 	- here("results", "tables", "romano_p_unantrem_norem.tex")
# 	- here("results", "tables", "het_survey_measures_antrem_late.tex")
# 	- here("results", "tables", "het_survey_measures_unantrem_late.tex")
# Files created:
# 	- here("proc", "het_survey_measures_unantrem_late.csv")
# 	- here("proc", "het_survey_measures_dl_late.csv")
# 	- here("proc", "het_survey_measures_antrem_late.csv")
# 	- here("results", "tables", "het_survey_measures_unantrem_late.tex")
# 	- here("results", "tables", "het_survey_measures_dl_late.tex")
# 	- here("results", "tables", "het_survey_measures_antrem_late.tex")
# 	- here("results", "tables", "het_survey_measures_antrem_late_romano.tex")
# 	- here("results", "tables", "het_survey_measures_unantrem_late_romano.tex")
# Purpose:
#   - Table C.12: Treatment Effect of Announced Reminder Concentrated Among Low Trust Firms.
#   - Table C.13: Heterogeneous Treatment Effects of Unannounced Reminder by Survey Measures
# 	- Estimate heterogenous treatment effects by survey measures.
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
library(readxl)
library(lubridate)
library(data.table)
library(multcomp)
library(haven)
library(conflicted)
options(modelsummary_format_numeric_latex = "plain",
        "modelsummary_stars_note" = FALSE)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "table_functions.R"))
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("last", "dplyr")
conflict_prefer("set_names", "magrittr")
#########################################################

####################################################
##  (1): Define functions for generating tables.  ##
####################################################
# (1.1): Basic regression function.
reg_basic <- function(data, var = "", model, wt = 1) {
  if (var != "") {
    model <- paste(var, model)
  }
  reg <- feols(fml = as.formula(model),
               data = data,
               se = "hetero",
               weights = wt)
  return(reg)
}

# (1.2): Regress offer acceptance on survey measure.
reg_accepted_offer <- function(data, measure, late = 0, wt = 1) {
  accept_var <- ifelse(late == 0, "accepted_offer_ontime", "accepted_offer_late")
  reg_basic(data = data, model = str_c(accept_var, " ~ ", measure), wt = wt)
}

# (1.3): Extract coefficients and standard errors.
extract_coef_se <- function(model) {
  output <- tribble(~survey_measure, ~group, ~coef, ~se,
                    as.character(model$fml[[3]]), "low", model$coefficients[[1]], model$se[[1]],
                    as.character(model$fml[[3]]), "high", sum(model$coefficients), model$se[[2]],
                    as.character(model$fml[[3]]), "treat_only", model$coefficients[[2]], model$se[[2]])
  return(output)
}

# (1.4): Extract coefficient and confidence intervals from combinations of variables.
all_coef_cis <- function(model) {
  treat_var <- as.character(model[["fml"]][[3]][[2]][[3]])
  svy_var <- as.character(model[["fml"]][[3]][[2]][[2]])
  svy_treat_var <- str_c(svy_var, ":", treat_var)
  current_params <- tribble(~coef_var, ~se_var, ~group, ~svy_status, 
                            # Low survey measure, no treatment = Intercept
                            "(Intercept)", "(Intercept)", "control", 0,       
                            # Low survey measure, treated = Intercept + treatment
                            c("(Intercept)", treat_var), treat_var, "treatment", 0,       
                            # High survey measure, no treatment = Intercept + survey measure  
                            c("(Intercept)", svy_var), svy_var, "control", 1, 
                            # High survey measure, treatment = Intercept + survey measure + treatment
                            c("(Intercept)", treat_var, svy_var,
                              svy_treat_var), 
                            svy_treat_var, "treatment", 1) %>% 
    as.data.frame()
  
  extract_coef_cis <- function(coef_var, se_var, model) {
    df <- tibble(coef = model$coefficients[coef_var] %>% sum(),
                 treat_coef = model$coefficients[se_var],
                 se = model$se[se_var],
                 se_or = se)
    if (str_detect(se_var, ":")) {
      res <- model %>% 
        glht(linfct = str_c(treat_var, "+", svy_treat_var, "= 0")) %>% 
        confint() %>% 
        summary()
      df %<>% mutate(se = res$test$sigma)
    }
    return(df)
  }
  
  current_results <- tibble()
  for (r in 1:nrow(current_params)) {
    current_results %<>%
      bind_rows(extract_coef_cis(unlist(current_params$coef_var[r]), current_params$se_var[r], model) %>% 
                  mutate(group = current_params$group[r],
                         svy_status = current_params$svy_status[r]))
  }
  current_results %<>%
    mutate(svy_measure = rep(svy_var, each = 4)) %>% 
    relocate(svy_measure, .before = coef)
}

# (1.4): Save coefficients and standard errors for graphs
save_coef_se <- function(model, fun, filename) {
  lapply(model, fun) %>%
    bind_rows() %>%
    write_csv_(here("proc", filename))
}

# (1.5): Run full regressions.
all_models_data <- function(data, variable, time, wt) {
  accept_var <- case_when(time == "firstday" ~ "accepted_offer_firstday",
                          time == "ontime" ~ "accepted_offer_ontime", 
                          time == "late" ~ "accepted_offer_late")
  models <- c("(1)" = lapply(data = data, accept_var, reg_basic, model = str_c("~ trust_scale_1_binary + ", variable, " + trust_scale_1_binary:", variable), wt = wt),
              "(2)" = lapply(data = data, accept_var, reg_basic, model = str_c("~ reciprocity_scale_binary + ", variable, " + reciprocity_scale_binary:", variable), wt = wt),
              "(3)" = lapply(data = data, accept_var, reg_basic, model = str_c("~ procrastination_scale_binary + ", variable, " + procrastination_scale_binary:", variable), wt = wt),
              "(4)" = lapply(data = data, accept_var, reg_basic, model = str_c("~ memory_scale_1_binary + ", variable, " + memory_scale_1_binary:", variable), wt = wt),
              "(5)" = lapply(data = data, accept_var, reg_basic, model = str_c("~ memory_scale_2_binary + ", variable, " + memory_scale_2_binary:", variable), wt = wt),
              "(6)" = lapply(data = data, accept_var, reg_basic, model = str_c("~ attention_scale_binary + ", variable, " + attention_scale_binary:", variable), wt = wt))
}

######################################################################################
##  (2): Regression tables: test correlation between take-up and survey measures.   ##
######################################################################################
# (2.1): Import survey and take-up data.
survey_successful <- read_csv(here("proc", "survey_successful.csv"))
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>% 
  select(organization_uuid, accepted_offer_ontime, accepted_offer_firstday, accepted_offer_late, treat_type, treat_description,
         reminder, deadline, anticipated_reminder, unanticipated_reminder, no_deadline, no_reminder)

# (2.2): Compare survey vs. full sample takeup.
survey_successful %>% summarise_at(vars(contains("accepted")), mean)
fintech_fee %>%
  filter(!str_detect(treat_description, "Control|24")) %>%
  summarise_at(vars(contains("accepted")), mean)

# (2.3): Merge offer acceptance variable.
survey_successful %<>%
  select(organization_uuid, contains("binary"), req_ans_q12.11, ipw_takeup) %>% 
  left_join(fintech_fee, by = "organization_uuid")

# (2.4): Drop missing values.
survey_successful %<>%
  filter(!is.na(trust_scale_1_binary)) %>%
  filter(!is.na(trust_scale_2_binary)) %>%
  filter(!is.na(reciprocity_scale_binary)) %>%
  filter(!is.na(procrastination_scale_binary)) %>%
  filter(!is.na(memory_scale_1_binary)) %>%
  filter(!is.na(memory_scale_2_binary)) %>%
  filter(!is.na(attention_scale_binary))  

##################################################################################
##  (3): Regression tables: heterogeneous treatment effects by survey measure.  ##
##################################################################################
# (3.1): Define coefficient names.
# Coefficient names.
survey_measures_varnames <- c("trust_scale_1_binary", "reciprocity_scale_binary", "procrastination_scale_binary", "memory_scale_1_binary",  "memory_scale_2_binary", "attention_scale_binary")
survey_measures_fullnames <- c("Trust", "Reciprocity", "Procrastination", "Memory", "Overconfidence", "Attention")
subgroups_fullnames <- c("Unannounced reminder", "Deadline", "Announced reminder")
subgroups_varnames <- c("unanticipated_reminder", "deadline", "anticipated_reminder")
# subgroups_fullnames <- c("Unannounced reminder", "Announced reminder")
# subgroups_varnames <- c("unanticipated_reminder", "anticipated_reminder")
coef_names <- c("Intercept",
                rep("High survey measure", 6),
                subgroups_fullnames,
                paste(rep("High survey measure", 6), "times", rep(subgroups_fullnames, each = 6)))
names(coef_names) <- c("(Intercept)",
                       survey_measures_varnames,
                       subgroups_varnames,
                       str_c(rep(survey_measures_varnames, 3), ":", rep(subgroups_varnames, each = 6)))



# (3.2): Define regression parameters.
regression_parameters <- tribble(
  ~data, ~subgroup, ~caption, ~label, ~varname,
  # Unannounced reminder
  survey_successful %>% filter(unanticipated_reminder == 1 | no_reminder == 1), 
  "unanticipated_reminder", "Heterogeneous Effects of Unannounced Reminder by GSS Measures", "unantrem", "Unannounced reminder",
  # Deadline
  survey_successful, 
  "deadline", "Heterogeneous Treatment Effects of Deadline by Survey Measures", "dl", "Deadline",
  # Announced reminder
  survey_successful %>% filter(anticipated_reminder == 1 | unanticipated_reminder == 1),
  "anticipated_reminder", "Heterogeneous Effects of Announced Reminder by GSS Measures", "antrem", "Announced reminder") %>% 
  as.data.frame()

# (3.3): Generate all tables and export regression coefficients and standard errors.

# Define function to add significance stars
add_stars <- function(p) {
  if (p <= 0.01) return("^{+++}")
  if (p <= 0.05) return("^{++}")
  if (p <= 0.1) return("^{+}")
  return("")
}

# Define function: add Romano-Wolf p-values to table
add_romano_to_table <- function(table_content, romano_pvalues) {
  if (is.null(romano_pvalues)) return(table_content)
  
  # Build Romano-Wolf row
  rw_cells <- sapply(romano_pvalues, function(p) {
    sprintf("&$[%.3f]%s$", p, add_stars(p))
  })
  rw_line <- paste0("Romano-Wolf $p$-values ", paste(rw_cells, collapse = " "), " \\\\")
  
  # Simple and reliable method: find the last occurrence of interaction term
  # and insert the Romano-Wolf line right after it (before the \midrule)
  
  # Split the table content into lines
  lines <- strsplit(table_content, "\n")[[1]]
  
  # Find the line that contains "\times" and comes before a \midrule line
  romano_inserted <- FALSE
  new_lines <- character(0)
  
  for (j in 1:length(lines)) {
    new_lines <- c(new_lines, lines[j])
    
    # If this line contains "\times" and the next line is \midrule, insert Romano line
    if (!romano_inserted && 
        j < length(lines) && 
        grepl("\\\\times", lines[j]) && 
        grepl("\\\\midrule", lines[j+1])) {
      new_lines <- c(new_lines, rw_line)
      romano_inserted <- TRUE
    }
  }
  
  # If cannot find the pattern, insert before the last \midrule before "Number of firms"
  if (!romano_inserted) {
    for (j in 1:length(lines)) {
      if (grepl("\\\\midrule", lines[j]) && 
          j < length(lines) && 
          grepl("^Number of firms", lines[j+1])) {
        new_lines <- c(new_lines[1:j], rw_line, lines[j:length(lines)])
        break
      }
    }
  }
  
  return(paste(new_lines, collapse = "\n"))
}

# Read Romano-Wolf files first
antrem_unantrem_romano <- readLines(here("results", "tables", "romano_p_antrem_unantrem.tex"))
unantrem_norem_romano <- readLines(here("results", "tables", "romano_p_unantrem_norem.tex"))

t = "late"
for (current_weight in 1) {  # Only run for unweighted case
  for (i in 1:nrow(regression_parameters)) {
    # Define parameters for titles and functions.
    ot_l_title <- case_when(t == "firstday" ~ "on First Day of Treatment", 
                            t == "ontime" ~ "by Deadline", 
                            t == "late" ~ "Beyond Deadline")
    wt <- ""  # No _wt suffix since we removed weighted case
    mean_weight <- "dum"
    
    # Run regressions (unant, deadline, ant).
    mod <- all_models_data(data = regression_parameters$data[i][[1]],
                           variable = regression_parameters$subgroup[i],
                           time = t,
                           wt = current_weight)
    
    # Calculate mean survey measures.
    mean_row <- survey_successful %>% 
      mutate(dum = 1) %>% 
      summarise_at(all_of(survey_measures_varnames), ~weighted.mean(., w = eval(as.name(mean_weight)))) %>% 
      lapply(comma_format)
    
    # Calculate mean takeup rate.
    takeup_row <- survey_successful %>% 
      mutate(dum = 1) %>% 
      summarise(takeup_rate = weighted.mean(eval(as.name(str_c("accepted_offer_", t))), w = eval(as.name(mean_weight)))) %>% 
      rep(6) %>%  
      lapply(comma_format)
    
    # Define additional row
    add_row <- data.frame(rbind(c("Prop. survey measure = 1", mean_row)))
    
    # Define label and file name
    label_name <- str_c("het_survey_measures_", regression_parameters$label[i], "_", t, wt)
    file_path <- here("results", "tables", str_c(label_name, ".tex"))
    
    # Extract corresponding Romano-Wolf p-values based on table type
    if (regression_parameters$label[i] == "antrem") {
      romano_pvalues <- str_trim(antrem_unantrem_romano[6:11]) %>% 
        str_extract_all("\\d*\\.\\d+") %>% 
        sapply(`[`, 3) %>% 
        as.numeric()
    } else if (regression_parameters$label[i] == "unantrem") {
      romano_pvalues <- str_trim(unantrem_norem_romano[6:11]) %>% 
        str_extract_all("\\d*\\.\\d+") %>% 
        sapply(`[`, 3) %>% 
        as.numeric()
    } else {
      romano_pvalues <- NULL
    }
    
    # Generate and export table.
    table_content <- mod %>% 
      set_names(c("Trust", "Reciprocity", "Procrastination", "Memory", "Overconfidence", "Attention")) %>% 
      gen_base_table(caption = regression_parameters$caption[i],
                     label = label_name,
                     title = str_c("Firm accepted offer"),
                     cline = TRUE,
                     coef_names = coef_names,
                     add_rows = add_row) %>% 
      coef_two_rows()
    
    # Add Romano-Wolf p-values to table
    table_content <- add_romano_to_table(table_content, romano_pvalues)
    
    # Replace cline with cmidrule
    table_content <- gsub("\\\\cline\\{2-7\\}", "\\\\cmidrule(lr)\\{2-7\\}", table_content)
    
    # Write to file (original version without Romano in filename)
    writeLines(table_content, con = file_path)
    
    # Also create *_romano.tex version for backward compatibility
    if (regression_parameters$label[i] == "antrem" || regression_parameters$label[i] == "unantrem") {
      romano_file_path <- here("results", "tables", str_c(label_name, "_romano.tex"))
      writeLines(table_content, con = romano_file_path)
    }
    
    # Save coefficients and standard errors.
    save_coef_se(mod, all_coef_cis, str_c(label_name, ".csv"))
    
  }
}

# Also create the specific files you mentioned for backward compatibility
# Create het_survey_measures_antrem_late_romano.tex
antrem_table_content <- readLines(here("results", "tables", "het_survey_measures_antrem_late.tex"))
antrem_romano_file_path <- here("results", "tables", "het_survey_measures_antrem_late_romano.tex")
writeLines(antrem_table_content, con = antrem_romano_file_path)

# Create het_survey_measures_unantrem_late_romano.tex
unantrem_table_content <- readLines(here("results", "tables", "het_survey_measures_unantrem_late.tex"))
unantrem_romano_file_path <- here("results", "tables", "het_survey_measures_unantrem_late_romano.tex")
writeLines(unantrem_table_content, con = unantrem_romano_file_path)
