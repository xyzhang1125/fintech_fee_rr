#--------------------------------------------------------------------------------------------
# File name: 		      05a_survey_functions.R
# Creation date:      2023-01-31
# Author:          		César Landín
# Files used:
#   - (None)
# Files created:
#   - (None)
# Purpose:
# 	- Define functions for survey processing.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(assertthat)
library(stringr)
# source(here("scripts", "programs", "number_functions.R"))
# original number function
num_int <- function(x) x == round(x)

comma_format <- function(x, override_right_digits = NA) {
  if (x <= 0) num <- 1 else num <- x
  right_digits <- 3 - floor(log10(abs(num)))
  if (right_digits < 0) right_digits <- 0
  if (right_digits > 3) right_digits <- 3
  if (!is.na(override_right_digits)) right_digits <- override_right_digits
  format(round(x, right_digits), nsmall = right_digits, big.mark = ",")
}

write_ <- function(content, file_path) {
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(content, file_path)
}

print_n <- function(x, file, note = "", dig = 1) {
  current_num <- as.numeric(x) %>% round(dig)
  current_num <- ifelse(num_int(current_num),
                        comma_format(current_num, override_right_digits = 0),
                        comma_format(current_num, override_right_digits = dig))
  output_text <- paste0(current_num, "%", ifelse(note != "", paste0("\n% ", note), ""))
  write_(output_text, here("results", "numbers", file))
  invisible(current_num)
}

print_pct <- function(x, file, note = "", dig = 1) {
  current_num <- as.numeric(x) * 100 %>% round(dig)
  current_num <- ifelse(num_int(current_num),
                        comma_format(current_num, override_right_digits = 0),
                        comma_format(current_num, override_right_digits = dig))
  output_text <- paste0(current_num, "\\\\%%", ifelse(note != "", paste0("\n% ", note), ""))
  write_(output_text, here("results", "numbers", file))
  invisible(current_num)
}
#########################################################

###########################################
##    (1): Initial non-response tabbing.  #
###########################################
# (1.1): Define mapping for required answer variables.
var_mapping <- tribble(~var_name, ~var_question,
                       "nb_employees", "1.1",
                       "perform_review", "1.2",
                       "monthly_profits", "1.4",
                       "tech_importance", "1.7",
                       "value_transactions", "1.8",
                       "percent_sales", "1.9",
                       "recall_old_fee", "1.10",
                       "compare_fee", "1.11",
                       "amount_paid_fee", "1.12",
                       "firstemail_recall", "2.1",
                       "recall_sms", "2.2",
                       "read_sms", "2.3",
                       "click_recall", "2.4",
                       "accept_offer_recall", "2.5",
                       "form_time_cat", "6.1",
                       "form_time_cat_alt", "6.12",
                       "exp_form_time_cat", "6.2",
                       "exp_form_time_cat_alt", "6.22",
                       "offer_effect_time", "6.3",
                       "noticed_deadline", "7.1",
                       "why_deadline", "7.2",
                       "why_activate_firstday", "8.1",
                       "why_activate_later", "9.1",
                       "reminder_recall", "10.1",
                       "open_reminder_recall", "10.2",
                       "why_reminder", "10.3",
                       "reminder_feel_cat", "10.4",
                       "offer_value_change", "10.5",
                       "a_reminder_takeup", "10.6",
                       "offer_impact_cat", "11.1",
                       "trust_scale_1", "12.11",
                       "trust_scale_2", "12.12",
                       "reciprocity_scale", "12.13",
                       "procrastination_scale", "12.14",
                       "memory_scale_1", "12.15",
                       "memory_scale_2", "12.16",
                       "attention_scale", "12.17") %>% 
  mutate(var_question = stringr::str_c("req_ans_q", var_question))

# (1.2): Tab non-responses.
tab_nas <- function(variable, data) {
  # Set successful as 1 if not using survey_proc
  proc_data <- data
  if (deparse(substitute(data)) != "survey_proc") {
    proc_data %<>% mutate(successful = 1)
  }
  # Define required answer variable name
  req_ans_var <- var_mapping %>% filter(var_name == variable) %>% pull(var_question) 
  # Filter firms that should have been asked question
  proc_data %<>%
    filter(successful == 1) %>%             # Keep successful surveys
    filter(eval(as.name(req_ans_var)) == 1) # Keep firms that should have answered this question
  # Get number of "other" answers
  ans_other <- proc_data %>% filter(eval(as.name(variable)) == -666) %>% nrow()
  # Get number of firms that did not know answer to question
  ans_dont_know <- proc_data %>% filter(eval(as.name(variable)) == -777) %>% nrow()
  # Get number of firms that refused to answer this question
  ans_no_answer <- proc_data %>% filter(is.na(eval(as.name(variable))) | eval(as.name(variable)) == -888) %>% nrow()
  # Print all results
  cat(str_c("(VARIABLE = ", variable, ")\n",
            "...", ans_other, " businesses answered 'other',\n", 
            "...", ans_dont_know, " did not know answer,\n",
            "...", ans_no_answer, " refused to answer,\n",
            "...out of ", nrow(proc_data), " firms that should have answered this question.\n"))
}
tabna <- function(varlist, data = survey_proc) {
  sapply(varlist, tab_nas, USE.NAMES = FALSE, data) %>% invisible()
}

# (1.3): Save number of firms that answered "other" (-666).
print_other <- function(variable, data = survey_proc) {
  # Define required answer variable name
  req_ans_var <- var_mapping %>% filter(var_name == variable) %>% pull(var_question) 
  # Set successful as 1 if not using survey_proc
  proc_data <- data
  if (deparse(substitute(data)) != "survey_proc") {
    proc_data %<>% mutate(successful = 1)
  }
  # Count number of don't knows
  proc_data %>%
    select(all_of(variable), all_of(req_ans_var), successful) %>%
    filter(successful == 1) %>%                    # Keep successful surveys
    filter(eval(as.name(req_ans_var)) == 1) %>%    # Keep firms that should have answered this question
    filter(eval(as.name(variable)) == -666) %>%    # Keep firms that answered "other"
    nrow() %>%                                     # Count firms
    print_n(str_c("survey_other_", variable, ".tex"))
}

# (1.4): Save number of firms that don't know answer to question (-777).
print_dont_know <- function(variable, data = survey_proc) {
  # Define required answer variable name
  req_ans_var <- var_mapping %>% filter(var_name == variable) %>% pull(var_question) 
  # Set successful as 1 if not using survey_proc
  proc_data <- data
  if (deparse(substitute(data)) != "survey_proc") {
    proc_data %<>% mutate(successful = 1)
  }
  # Count number of don't knows
  proc_data %>%
    select(all_of(variable), all_of(req_ans_var), successful) %>%
    filter(successful == 1) %>%                    # Keep successful surveys
    filter(eval(as.name(req_ans_var)) == 1) %>%    # Keep firms that should have answered this question
    filter(eval(as.name(variable)) == -777) %>%    # Keep firms that did not know the answer to this question
    nrow() %>%                                     # Count firms
    print_n(str_c("survey_dont_know_", variable, ".tex"))
}

# (1.5): Save number of firms that should have answered the question and did not answer (NAs and -888s).
print_no_answer <- function(variable, data = survey_proc) {
  # Define required answer variable name
  req_ans_var <- var_mapping %>% filter(var_name == variable) %>% pull(var_question) 
  # Set successful as 1 if not using survey_proc
  proc_data <- data
  if (deparse(substitute(data)) != "survey_proc") {
    proc_data %<>% mutate(successful = 1)
  }
  # Get number of firms that should have answered question and did not answer
  proc_data %>% 
    select(all_of(variable), all_of(req_ans_var), successful) %>%
    filter(successful == 1) %>%                    # Keep successful surveys
    filter(eval(as.name(req_ans_var)) == 1) %>%    # Keep firms that should have answered this question
    filter(is.na(eval(as.name(variable))) |        # Keep firms that have an NA for this question (prior to recoding)
             eval(as.name(variable)) == -888) %>%  # Keep firms that refuse to answer the question
    nrow() %>%                                     # Count firms
    print_n(str_c("survey_no_answer_", variable, ".tex"))
}

# (1.6): Get number of omitted firms.
get_n_omitted <- function(variable, stats = c("other", "dk", "na"), data = survey_proc) {
  current_num <- tabna(variable, data) %>% 
    capture.output() %>% 
    str_c(collapse = "") %>% 
    str_match_all("[0-9]+") %>% 
    unlist() %>% 
    as.numeric() %>% 
    .[1:3] %>% 
    as_tibble() %>% 
    transmute(n = value,
              non_response = c("other", "dk", "na"))
  current_num %>% 
    filter(non_response %in% stats) %>% 
    pull(n) %>% 
    sum()
}

##########################################################################
##    (2): Functions for recoding sections 3-5 (non-adoption questions.  #
##########################################################################
#(2.1): Convert responses to numeric.
var_to_num <- function(df, var, recode_df = responses) {
  recode_var <- str_remove(var, "_other") %>% str_c("_recode")
  df %>%
    left_join(recode_df %>%
                rename(!!var := response) %>%
                select(all_of(var), all_of(recode_var)),
              by = var) %>%
    mutate(!!var := eval(as.name(recode_var))) %>%
    select(-all_of(recode_var))
}


##################################
##    (3): Recoding functions.  ##
##################################
# (3.1): Recode "other" responses in reminder feel and offer impact questions.
recode_other <- function(df, var, recode_df) {
  print(paste("Current number of 'other' responses", 
              df %>% filter(str_detect(eval(as.name(var)), "-666")) %>% nrow()))
  proc_df <- df %>%
    left_join(recode_df %>%
                transmute(organization_uuid,
                          new_categorical = eval(as.name(var))),
              by = "organization_uuid") %>%
    mutate(!!var := ifelse(is.na(new_categorical), eval(as.name(var)), new_categorical)) %>%
    select(!new_categorical)
  print(paste("New number of 'other' responses",
              proc_df %>% filter(str_detect(eval(as.name(var)), "-666")) %>% count()))
  return(proc_df)
}

