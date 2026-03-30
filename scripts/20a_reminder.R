#--------------------------------------------------------------------------------------------
# File name: 		      20_reminder.R
# Creation date:      2023-01-31
# Author:          		César Landín
# Files used:
# 	- here("proc", "survey_successful.csv")
#   - here("data", "Survey response mappings", "mapping_10.3_why_reminder.csv")
#   - here("data", "Survey response mappings", "mapping_10.4_reminder_feel_cat.csv") 
# Files created:
# 	- here("proc", "survey_why_reminder_changed.qs")
# 	- here("results", "figures", "survey_why_reminder_changed.eps")
# 	- here("results", "numbers", "survey_num_why_reminder.tex")
# 	- here("results", "numbers", "survey_pct_why_reminder_normal.tex")
# 	- here("results", "numbers", "survey_pct_why_reminder_suspicious.tex")
# 	- here("results", "figures", "survey_reminder_feel_changed.eps")
# 	- here("results", "numbers", "survey_num_reminder_feel.tex")
# 	- here("results", "numbers", "survey_pct_reminder_feel_cat_irritated.tex")
# 	- here("results", "numbers", "pres_survey_pct_reminder_feel_cat_irritated.tex")
# 	- here("results", "numbers", "survey_pct_reminder_feel_cat_irritated_antrem.tex")
# 	- here("results", "numbers", "survey_pct_reminder_feel_cat_irritated_unantrem.tex")
# 	- here("results", "numbers", "survey_pct_reminder_feel_cat_irritated_diff.tex")
# 	- here("results", "figures", "survey_offer_value_change.eps")
# 	- here("results", "numbers", "offer_perception_change.tex")
# 	- here("results", "numbers", "coef_offer_value_change.tex")
# 	- here("results", "numbers", "mean_offer_value_change_antrem.tex")
# 	- here("results", "numbers", "mean_offer_value_change_unantrem.tex")
# Purpose:
#   - Figure C.13a: Why Firms Thought the Offer Had a Deadline and Reminder (Only reminder graph)
#   - Figure C.18: How Firms Felt About Receiving Reminder
# 	- Figure 9: Effect of Announced Reminder on Perceived Offer Value
#     Graph questions 10.3 (why firms think they received the reminder), 10.4 (how firms feel
#     about receiving reminder)
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(fixest)
library(qs)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # changed to times
showtext_auto()
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "programs", "05c_barplot_functions.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

################################################################################################
##    (1): Generate bar graph for question 10.3 (why firms think they received the reminder).  #
################################################################################################
# 10.3.	Why do you think iZettle sent you a reminder? [Open ended. Surveyor: let the respondent answer fully before selecting all choices that align with their response. If none of the options closely aligns with their response, select Other and write out their response]
# a.	Usual business practice
# b.	They knew I would forget
# c.	Make sure I wouldn’t forgo a valuable offer
# d.	Trying to get me to fall for a scam or deceptive ad
# e.	Trying to get me to sign up for something that would increase their profits
# f.	Other (Specify: _____________)

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.3: reminder_recall == 1

# (1.1): Import survey data.
data_why_reminder <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, why_reminder, req_ans_q10.3, anticipated_reminder, unanticipated_reminder) %>% 
  filter(req_ans_q10.3 == 1)
data_why_reminder %>% tab(why_reminder)

# (1.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_10.3_why_reminder.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_"))

# (1.3): Generate dummy response variables and process data for graph.
current_graph_data <- data_why_reminder %>% 
  gen_all_response_dummies(current_mapping, "why_reminder") %>% 
  proc_graph_data()

sub <- current_graph_data |>
  filter(term == "Refuses to answer" | term == "Does not know")

new_observation <- tibble(
  term = 'Did not answer',
  n = sum(sub$n),
  coef = sum(sub$coef),
  lci = sum(sub$lci),
  uci = sum(sub$uci)
)

current_graph_data_changed <- bind_rows(current_graph_data, new_observation)
current_graph_data_changed <- current_graph_data_changed |>
  filter(term != 'Refuses to answer' & term != 'Does not know')

# (1.4): changed.
form_levels <- c("Did not answer",
                 "Other",
                 "Trying to get me to fall for a scam or deceptive ad",
                 "To remind customer to accept the form",
                 "Trying to get me to sign up for something that would increase their profits",
                 "Usual business practice",
                 "They knew I would forget",
                 "Make sure I wouldn't forgo a valuable offer")

#added now
graph <- current_graph_data_changed %>%
  mutate(term = fct_reorder(term, coef)) %>%
  graph_barplot() +
  coord_flip() +
  expand_limits(y = 0.6) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40))
# Save paper version
graph
qsave(graph, here("proc", "survey_why_reminder_changed.qs"))
ggsave_(here("results", "figures", "survey_why_reminder_changed.eps"), width = 14, height = 14)

data_why_reminder %>% 
  nrow() %>%
  print_n("survey_num_why_reminder.tex", "Number of firms asked the question - Why do you think we sent you a reminder?", dig = 1)

# (1.5): Define and save footnote.
# Check extent of non response
tabna("why_reminder", data_why_reminder)

# (1.7): Print percentage firms responding to top 3 reasons or being suspicious of reminder.
data_why_reminder %>% 
  rowwise() %>% 
  filter(str_detect_numeric(why_reminder, "1") |
           str_detect_numeric(why_reminder, "2") |
           str_detect_numeric(why_reminder, "3")) %>% 
  nrow() %>% 
  "/"(nrow(data_why_reminder)) %>% 
  print_pct("survey_pct_why_reminder_normal.tex",
            "Percentage firms reporting thinking they received reminder because it was a usual business practice so that firms wouldn't forget to adopt a valuable offer.")

data_why_reminder %>% 
  rowwise() %>% 
  filter(str_detect_numeric(why_reminder, "4") |
           str_detect_numeric(why_reminder, "5")) %>% 
  nrow() %>% 
  "/"(nrow(data_why_reminder)) %>% 
  print_pct("survey_pct_why_reminder_suspicious.tex",
            "Percentage firms reporting thinking they received reminder to increase the FinTech's profits or to make firms fall for a scam.")
  
rm(data_why_reminder, current_mapping, current_graph_data, graph, data_test)

#############################################################################################
##    (2): Generate bar graph for question 10.4 (how firms felt about receiving reminder).  #
#############################################################################################
# 10.4.	How did you feel about receiving a reminder? [Surveyor: select all that apply]
# a.	Makes me feel important as a client
# b.	Indifferent, without much interest or importance
# c.	Motivated to not pass up on the offer
# d.	Would have forgotten the offer
# e.	Other (Specify: ___________)
# f.	Does not know
# g.	Refuses to answer

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.4: reminder_recall == 1

# (2.1): Import survey data.
data_reminder_feel_cat <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, reminder_feel_cat, req_ans_q10.4, anticipated_reminder, unanticipated_reminder) %>% 
  filter(req_ans_q10.4 == 1)
data_reminder_feel_cat %>% tab(reminder_feel_cat)

# (2.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_10.4_reminder_feel_cat.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_")) %>% 
  filter(reminder_feel_cat != "5")

# (2.3): Generate dummy response variables and process data for graph.
current_graph_data <- data_reminder_feel_cat %>% 
  gen_all_response_dummies(current_mapping, "reminder_feel_cat") %>% 
  proc_graph_data()

sub <- current_graph_data |>
  filter(term == "Refuses to answer" | term == "Does not know")

new_observation <- tibble(
  term = 'Did not answer',
  n = sum(sub$n),
  coef = sum(sub$coef),
  lci = sum(sub$lci),
  uci = sum(sub$uci)
)

current_graph_data_changed <- bind_rows(current_graph_data, new_observation)
current_graph_data_changed <- current_graph_data_changed |>
  filter(term != 'Refuses to answer' & term != 'Does not know')


data_reminder_feel_cat %>% 
  nrow() %>%
  print_n("survey_num_reminder_feel.tex", "Number of firms asked the question - How did receiving the reminder make you feel?", dig = 1)

# (2.4) changed
form_levels <- c("Did not answer",
                 "Other",
                 "Irritated or pressured by the reminders",
                 "Would have forgotten the offer",
                 "Motivated to not pass up on the offer",
                 "Indifferent, without much interest or importance",
                 "Makes me feel important as a client")

graph <- current_graph_data_changed %>%
  mutate(term = fct_reorder(term, coef)) %>%
  graph_barplot(str_width = 22) +
  coord_flip() + 
  expand_limits(y = 0.6) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))
# Save paper version
graph
ggsave_(here("results", "figures", "survey_reminder_feel_changed.eps"), width = 10, height = 8)


data_reminder_feel_cat %>% 
  nrow() %>%
  print_n("survey_num_reminder_feel.tex", "Number of firms asked the question - How did receiving the reminder make you feel?", dig = 1)

# (2.5): Define and save footnote.
# Check extent of non response
tabna("reminder_feel_cat", data_reminder_feel_cat)

# (2.6): Print percentage firms reporting they felt irritated.
temp_n <- current_graph_data %>% 
  filter(str_detect(term, "Irritated")) %>% 
  pull(coef)
print_pct(temp_n,
          "survey_pct_reminder_feel_cat_irritated.tex",
          "Percentage firms reporting that the reminder made them feel irritated or pressured.")
print_pct(temp_n,
          "pres_survey_pct_reminder_feel_cat_irritated.tex",
          "Percentage firms reporting that the reminder made them feel irritated or pressured.",
          dig = 0)
irritated_split <- data_reminder_feel_cat %>%
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>%
  rowwise() %>% 
  mutate(irritated = str_detect_numeric(reminder_feel_cat, "6") %>% as.numeric()) %>%
  group_by(anticipated_reminder, unanticipated_reminder) %>%
  summarise(irritated = mean(irritated))
irritated_split %>% 
  filter(anticipated_reminder == 1) %>% 
  pull(irritated) %>% 
  print_pct("survey_pct_reminder_feel_cat_irritated_antrem.tex",
            "Percentage announced reminder firms reporting that the reminder made them feel irritated or pressured.")
irritated_split %>% 
  filter(unanticipated_reminder == 1) %>% 
  pull(irritated) %>% 
  print_pct("survey_pct_reminder_feel_cat_irritated_unantrem.tex",
            "Percentage unannounced reminder firms reporting that the reminder made them feel irritated or pressured.")
irritated_split %>% 
  pull(irritated) %>% 
  dist() %>% 
  "*"(100) %>% 
  print_n("survey_pct_reminder_feel_cat_irritated_diff.tex",
          "P.p. difference between % announced and unannounced reminder firms reporting that the reminder made them feel irritated or pressured.")

rm(data_reminder_feel_cat, current_mapping, current_graph_data, graph, data_test)

###########################################################################################
##    (3): Generate bar graph for question 10.5 (change in perception of offer value).   ##
###########################################################################################
# 10.5.	Did the reminder change your perception of the offer’s value? 
# a.	Yes (Why? ____________)
# b.	No

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 10: (anticipated_reminder == 1 | unanticipated_reminder == 1) & (accepted_offer_date  >= ymd("2020-10-05") | accepted_offer == 0)
# Filter for question 10.5: reminder_recall == 1

# (3.1): Import survey results and process data.
data_offer_value_change <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, req_ans_q10.5, anticipated_reminder, unanticipated_reminder, offer_value_change) %>% 
  mutate(rem_type = ifelse(anticipated_reminder == 1, "Announced Reminder", "Unannounced Reminder"),
         ant_rem = ifelse(rem_type == "Anticipated Reminder", 1, 0)) %>% 
  filter(req_ans_q10.5 == 1)

nrow(data_offer_value_change %>% 
       filter(!is.na(offer_value_change) & offer_value_change != -777))%>% 
  print_n("offer_perception_change.tex", "Firms that were asked the question -- “Did the reminder change your perception of the offer’s value?”, separately by reminder type. These are firms that knew the answer")

# (3.2): Run regression and get coefficients and standard errors.
model <- feols(data = data_offer_value_change %>% 
                   filter(!is.na(offer_value_change) & offer_value_change != -777),
               fml = offer_value_change ~ anticipated_reminder, 
               se = "hetero")
reg_results <- get_coef_cis(model, "Unanticipated reminder", "Anticipated reminder") 

# (3.3): Generate and export graph.
graph <- reg_results %>% 
  # mutate(term = case_when(term == "Anticipated reminder" ~ "Announced reminder",
                          # term == "Unnanounced reminder" ~
  # filter(!(term %in% c("Other", "Does not know", "Refuses to answer"))) %>%
  graph_barplot() +
  geom_hline(yintercept = 0) +
  scale_x_discrete(labels = function(x) str_wrap(c("Unannounced reminder", "Announced reminder"), width = 10), expand = expansion(add = 0.5)) +
  set_theme(size = 16) + 
  theme(text = element_text(family = "Times New Roman"))
# Save paper version
graph
ggsave_(here("results", "figures", "survey_offer_value_change.eps"), width = 6, height = 6)

# (3.5): Print coefficient and means for paper.
# Get offer value change regression coefficient
model[["coefficients"]][["anticipated_reminder"]] %>% 
  "*"(100) %>% 
  print_n("coef_offer_value_change.tex", "Offer value change coefficient")
# Print mean announced reminder offer value change
reg_results %>% 
  filter(term == "Anticipated reminder") %>% 
  pull(coef) %>% 
  print_pct("mean_offer_value_change_antrem.tex", "Mean offer value change for announced reminder")
# Print mean unanticipated reminder offer value change
reg_results %>% 
  filter(term == "Unanticipated reminder") %>% 
  pull(coef) %>% 
  print_pct("mean_offer_value_change_unantrem.tex", "Mean offer value change for unannounced reminder")

rm(model, data_offer_value_change, graph, reg_results)
