#--------------------------------------------------------------------------------------------
# File name: 		      20c_form_time.R
# Creation date:      2023-02-04
# Author:          		César Landín
# Files used:
# 	- here("proc", "survey_successful.csv")
# 	- here("data", "Survey response mappings", "mapping_6.12_form_time_cat_alt.csv")
# 	- here("data", "Survey response mappings", "mapping_6.22_exp_form_time_cat_alt.csv")
# 	- here("proc", "survey_non_adoption_pct.csv")
# Files created:
# 	- here("proc", "survey_form_time_cat2_changed.qs")
# 	- here("results", "figures", "survey_form_time_cat2_changed.eps")
# 	- here("results", "numbers", "survey_num_form_time_cat_alt.tex")
# 	- here("proc", "survey_exp_form_time_cat_alt_changed.qs")
# 	- here("results", "figures", "survey_exp_form_time_cat_alt_changed.eps")
# 	- here("results", "numbers", "survey_num_exp_form_time_cat_alt.tex")
# 	- here("results", "numbers", "survey_non_adoption_too_long.tex")
# Purpose:
#   - Figure C.3a: Self-Reported Time Cost of Accepting the Offer (Expected Time graph)
#   - Figure C.3b: Self-Reported Time Cost of Accepting the Offer (Actual Time graph)
# 	- Graph Section 6 questions 6.1 and 6.2: how long it took firms to complete and how long
#     firms expected it would take them to complete the offer.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(qs)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf", bold = "timesbd.ttf") # changed to times and timesbd 
showtext_auto()
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "05b_graph_colors.R"))
source(here("scripts", "programs", "05c_barplot_functions.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

##################################################################################################
##    (1): Generate bar graph for question 6.1 How long did it take you to fill out the offer?  ##
##################################################################################################
# 6.	[If merchant DID complete the form]
# 6.1.	How long did it take you to fill out the offer? 
# a.	1 minute or less
# b.	1-5 minutes
# c.	6-10 minutes
# d.	10 + minutes
# e.	Does not know
# f.	Refuses to answer

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 6: accepted offer and recalls accepting survey or clicking on link.

# (1.1): Import survey data.
data_form_time_cat_alt <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, form_time_cat_alt, req_ans_q6.12) %>% 
  filter(req_ans_q6.12 == 1)
data_form_time_cat_alt %>% tab(form_time_cat_alt)

# (1.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_6.12_form_time_cat_alt.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_"))
# new_row <-  tibble(
#   response = "Did not answer",
#   form_time_cat_alt = 111,
#   response_var = "did_not_answer"
# )
# current_mapping_change <- bind_rows(current_mapping, new_row)

# (1.3): Replace missings (NA) with -888.
data_form_time_cat_alt %<>% mutate_at(vars(form_time_cat_alt), ~ifelse(is.na(.), "-888", .))
# This line of code was created by Mohammad -> 
data_form_time_cat_alt_change <- data_form_time_cat_alt |>
  mutate(form_time_cat_alt = ifelse(form_time_cat_alt == "-777" | form_time_cat_alt == "-888", "111", form_time_cat_alt))
                      

# (1.4): Generate dummy response variables and process data for graph.
current_graph_data <- data_form_time_cat_alt %>% 
  gen_all_response_dummies(current_mapping, "form_time_cat_alt") %>% 
  proc_graph_data()

current_graph_data_change <- data_form_time_cat_alt_change |>
  count(form_time_cat_alt) |>                   
  rename(n = n) |>                          
  mutate(coef = n / sum(n),
         term = case_when(
           form_time_cat_alt == 1 ~ "1 minute or less",
           form_time_cat_alt == 2 ~ "1-5 minutes",
           form_time_cat_alt == 3 ~ "6-10 minutes",
           form_time_cat_alt == 4 ~ "10 + minutes",
           form_time_cat_alt == 111 ~ "Did not answer"))

# (1.5): Relevel responses.
current_graph_data %<>% relevel_data("form_time_cat_alt")

# (1.6): Define bar plot colors from standardized palette.
current_col <- lapply(seq(-7, 1, 2), modify_col, col = group_colors$col1[3]) %>% as_vector()
# names(current_col) <- c(1:5)
show_col(current_col)

# (1.7): Generate and export graph.
form_time_levels <- c("1 minute or less", "1-5 minutes", "6-10 minutes", "10 + minutes", "Does not know", "Refuses to answer")

# (1.7) Changed
form_time_levels_change <- c("1 minute or less", "1-5 minutes", "6-10 minutes", "10 + minutes", "Did not answer")
graph <- current_graph_data_change %>% 
  mutate(term = factor(term, levels = form_time_levels_change)) %>%
  graph_barplot() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 24),
        axis.text.x = element_text(size = 24)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = current_col) +
  scale_y_continuous(limits = c(0, 0.7), labels = percent_format(accuracy = 1))
# Save paper version
graph
qsave(graph, here("proc", "survey_form_time_cat2_changed.qs"))
ggsave_(here("results", "figures", "survey_form_time_cat2_changed.eps"), width = 8, height = 6)

data_form_time_cat_alt_change %>% 
  nrow() %>%
  print_n("survey_num_form_time_cat_alt.tex", "Number of firms asked the question - How long did it take you to fill out the offer?", dig = 1)


# (1.8): Define and save footnote.
# Check extent of non response
tabna("form_time_cat_alt", data_form_time_cat_alt)

rm(data_form_time_cat_alt, data_form_time_cat_alt_change, graph, current_graph_data, current_graph_data_change, current_mapping)

########################################################################################################################################
##    (2): Generate bar graph for question 6.2 How long did you expect completing the form to activate the lower fee would take you?  ##
########################################################################################################################################
# 6.2.	How long did you expect completing the form to activate the lower fee would take you?
# a.	1 minute or less
# b.	Between 1 and 5 minutes
# c.	Between 6 and 10 minutes
# d.	More than 10 minutes
# e.	Does not know
# f.	Refuses to answer

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 6: accepted offer and recalls accepting survey or clicking on link.

# (2.1): Import survey data.
data_exp_form_time_cat_alt <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, exp_form_time_cat_alt, req_ans_q6.22) %>% 
  filter(req_ans_q6.22 == 1)
data_exp_form_time_cat_alt %>% tab(exp_form_time_cat_alt)

# (2.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_6.22_exp_form_time_cat_alt.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_"))

# (2.3): Replace missings (NA) with -888.
data_exp_form_time_cat_alt %<>% mutate_at(vars(exp_form_time_cat_alt), ~ifelse(is.na(.), "-888", .))
# This line of code was created by Mohammad -> 
data_exp_form_time_cat_alt_change <- data_exp_form_time_cat_alt |>
  mutate(exp_form_time_cat_alt = ifelse(exp_form_time_cat_alt == "-777" | exp_form_time_cat_alt == "-888", "111", exp_form_time_cat_alt))

# (2.4): Generate dummy response variables and process data for graph.
current_graph_data <- data_exp_form_time_cat_alt %>% 
  gen_all_response_dummies(current_mapping, "exp_form_time_cat_alt") %>% 
  proc_graph_data()

current_graph_data_change <- data_exp_form_time_cat_alt_change |>
  count(exp_form_time_cat_alt) |>                   
  rename(n = n) |>                          
  mutate(coef = n / sum(n),
         term = case_when(
           exp_form_time_cat_alt == 1 ~ "1 minute or less",
           exp_form_time_cat_alt == 2 ~ "1-5 minutes",
           exp_form_time_cat_alt == 3 ~ "6-10 minutes",
           exp_form_time_cat_alt == 4 ~ "10 + minutes",
           exp_form_time_cat_alt == 111 ~ "Did not answer"))


# (2.5): changed.
graph <- current_graph_data_change %>% 
  mutate(term = factor(term, levels = form_time_levels_change)) %>%
  graph_barplot() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.y = element_text(size = 24),
        axis.text.x = element_text(size = 24)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_fill_manual(values = current_col) +
  scale_y_continuous(limits = c(0, 0.7), labels = percent_format(accuracy = 1))
# Save paper version
graph
qsave(graph, here("proc", "survey_exp_form_time_cat_alt_changed.qs"))
ggsave_(here("results", "figures", "survey_exp_form_time_cat_alt_changed.eps"), width = 8, height = 6)

data_exp_form_time_cat_alt_change %>% 
  nrow() %>%
  print_n("survey_num_exp_form_time_cat_alt.tex", "Number of firms asked the question - How long did you expect completing the form to activate the lower fee would take you?", dig = 1)

# (2.6): Define and save footnote.
# Check extent of non response
tabna("exp_form_time_cat_alt", data_exp_form_time_cat_alt)

rm(data_exp_form_time_cat_alt, graph, current_graph_data, current_mapping)

##############################################################################################################
##  (3): Get percentage survey respondents that did not accept because they thought it would take too long.  # 
##############################################################################################################
# All recall questions should have been asked to firms that recalled sms (recall_sms == 1) or recalled opening the first email (firstemail_recall == 1)
# (3.1): Import non-adoption data and tab percentage firms that did not accept because they thought it would take too long.
read_csv(here("proc", "survey_non_adoption_pct.csv")) %>% 
  filter(term == "Thought it would take too much time") %>% 
  pull(coef) %>% 
  print_pct("survey_non_adoption_too_long.tex",
            "Percentage firms that did not accept because they thought it would take too long")
