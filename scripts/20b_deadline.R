#--------------------------------------------------------------------------------------------
# File name: 		      22f_s7_s8_s9_deadline.R
# Creation date:      2023-01-31
# Author:          		César Landín
# Files used:
# 	- here("proc", "survey_successful.csv")
#   - here("data", "Survey response mappings", "mapping_7.2_why_deadline.csv")
#   - here("data", "Survey response mappings", "mapping_8.1_why_activate_firstday.csv")
#   - here("data", "Survey response mappings", "mapping_9.1_why_activate_later.csv")
# Files created:
#   - here("proc", "survey_why_deadline_changed.qs")
#   - here("results", "figures", "survey_why_deadline_changed.eps")
#   - here("results", "numbers", "survey_num_why_deadline.tex")
#   - here("results", "numbers", "survey_pct_why_deadline_scarcity.tex")
#   - here("proc", "survey_why_activate_firstday_changed.qs")
#   - here("results", "figures", "survey_why_activate_firstday_changed.eps")
#   - here("results", "numbers", "survey_num_why_activate_firstday.tex")
#   - here("results", "numbers", "survey_pct_why_activate_firstday_had_time.tex")
#   - here("proc", "survey_why_activate_later_changed.qs")
#   - here("results", "figures", "survey_why_activate_later_changed.eps")
#   - here("results", "numbers", "survey_num_why_activate_later.tex")
#   - here("results", "numbers", "survey_pct_why_activate_later_too_busy.tex")
#   - here("results", "numbers", "survey_pct_why_activate_later_discuss_think_offer.tex")
#   - here("results", "numbers", "survey_pct_why_activate_later_discuss_offer_antrem.tex")
#   - here("results", "numbers", "survey_pct_why_activate_later_discuss_offer_unantrem.tex")
#   - here("results", "numbers", "survey_p_why_activate_later_discuss_offer.tex")
# Purpose:
#   - Figure C.13b: Why Firms Thought the Offer Had a Deadline and Reminder (Only deadline graph)
#   - Figure C.6a: Why Firms Accepted the Offer On or After the First Day
#   - Figure C.6b: Why Firms Accepted the Offer On or After the First Day
# 	- Generate figures for survey sections 7-9 (deadline sections): reasons for deadline
#     existence, why activate offer on the first day, and why wait to activate the lower fee.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
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

###################################################################################
##    (1): Generate bar graph for question 7.2 (reasons for deadline existence).  #
###################################################################################
# 7.2.	Why do you think the offer had a deadline? [open-ended; Surveyor: let the respondent answer fully before selecting the choice that most closely aligns with their response. If none of the options closely aligns with their response, select Other and write out their response]
# a.	It is a usual business practice
# b.	It is a common marketing tool
# c.	Fintech can only lower the fee for a limited number of users 
# d.	Other (Please specify: ________________)
# e.	Don’t know
# f.	Refuses to answer

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 7: deadline == 1
# Filter for question 7.2: noticed_deadline == 1

# (1.1): Import survey data.
data_why_deadline <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, why_deadline, req_ans_q7.2) %>% 
  filter(req_ans_q7.2 == 1)
data_why_deadline %>% tab(why_deadline)

# (1.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_7.2_why_deadline.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_"))
current_mapping <- current_mapping |>
  mutate(response = ifelse(response == "It is a commmon marketing tool", "It is a common marketing tool", response))

# (1.3): Generate dummy response variables and process data for graph.
current_graph_data <- data_why_deadline %>% 
  gen_all_response_dummies(current_mapping, "why_deadline") %>% 
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


# (1.4): Relevel responses.
current_graph_data %<>% relevel_data("why_deadline")

# (1.5): changed
form_levels <- c("Did not answer",
                 "Other",
                 "Because it is a temporary offer",
                 "To help businesses cope with the pandemic",
                 "Because opportunities were not available for all users",
                 "It is a common marketing tool",
                 "Usual business practice")


graph <- current_graph_data_changed %>%
  mutate(term = fct_reorder(term, coef)) %>% 
  graph_barplot() +
  coord_flip() +
  expand_limits(y = 0.6) +
  #ylim(0, 1) +
  #scale_y_continuous(breaks = seq(0, 1, by = 0.2), 
  #                   labels = percent_format(accuracy = 1)) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40))

# Save paper version
graph
qsave(graph, here("proc", "survey_why_deadline_changed.qs"))
ggsave_(here("results", "figures", "survey_why_deadline_changed.eps"), width = 14, height = 14)

data_why_deadline %>% 
  nrow() %>%
  print_n("survey_num_why_deadline.tex", "Number of firms asked the question - Why do you think the offer had a deadline?", dig = 1)


# (1.6): Define and save footnote.
# Check extent of non response
tabna("why_deadline", data_why_deadline)

# (1.7): Save numbers for paper.
current_graph_data %>% 
  filter(term == "Because opportunities were not available for all users") %>% 
  pull(coef) %>% 
  print_pct("survey_pct_why_deadline_scarcity.tex",
            "Percentage firms reporting that they thought the deadline existed because opportunities were not available for all users.")

rm(data_why_deadline, graph, current_graph_data, current_graph_data_changed, current_mapping)

########################################################################################
##    (2): Generate bar graph for question 8.2 (why activate offer on the first day).  #
########################################################################################
# 8.1.	Our records show that you activated the offer on September 29, even though your deadline 
#       to activate the offer was not until October 6. Why did you activate the offer on September 29? 
# a.	I was worried that I would forget if I waited
# b.	Offer was too good to delay activating
# c.	I had time today
# d.	Other (Please specify: ________________)

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 8: deadline == 1 & accepted_offer_date == ymd("2020-09-29")

# (2.1): Import survey data.
data_why_activate_firstday <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, why_activate_firstday, req_ans_q8.1, anticipated_reminder, unanticipated_reminder) %>% 
  filter(req_ans_q8.1 == 1)
data_why_activate_firstday %>% tab(why_activate_firstday)

# (2.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_8.1_why_activate_firstday.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_"))

# (2.3): Generate dummy response variables and process data for graph.
current_graph_data <- data_why_activate_firstday %>% 
  gen_all_response_dummies(current_mapping, "why_activate_firstday") %>% 
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


# (2.4): Relevel responses.
current_graph_data %<>% relevel_data("why_activate_firstday")

# (2.5): changed
form_levels <- c("Did not answer",
                 "Other",
                 "I was worried that I would forget if I waited",
                 "Offer was too good to delay activating",
                 "I had time today")


graph <- current_graph_data_changed %>%
  mutate(term = fct_reorder(term, coef)) %>%  
  graph_barplot(str_width = 24) +
  coord_flip() +
  expand_limits(y = 0.6) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22))
# Save paper version
graph
qsave(graph, here("proc", "survey_why_activate_firstday_changed.qs"))
ggsave_(here("results", "figures", "survey_why_activate_firstday_changed.eps"), width = 8, height = 6)

data_why_activate_firstday %>% 
  nrow() %>%
  print_n("survey_num_why_activate_firstday.tex", "Number of firms asked the question - Our records show that you activated the offer on September 29, even though your deadline to activate the offer was not until October 6. Why did you activate the offer on September 29?", dig = 1)

# (2.6): Define and save footnote.
# Check extent of non response
tabna("why_activate_firstday", data_why_activate_firstday)

# (2.7): Save numbers for paper.
current_graph_data %>% 
  filter(term == "I had time today") %>% 
  pull(coef) %>% 
  print_pct("survey_pct_why_activate_firstday_had_time.tex",
            "Percentage firms reporting that they accepted the offer on the first day because they had time that day.")

rm(data_why_activate_firstday, graph, current_graph_data, current_graph_data_changed, current_mapping)


#########################################################################################
##    (3): Generate bar graph for question 9.1 (why wait to activate lower fee offer).  #
#########################################################################################
# 9.1.	We sent you the emails and SMS to let you know about this offer on September 29, 
#       but we see that you filled the form on ${activation_date}. 
#       Why did you wait until ___ day(s) later?
# a.	I was too busy when I initially received the offer to activate it then
# b.	I had to discuss the offer with someone else before accepting it
# c.	I was putting it off
# d.	I knew I would receive a reminder
# e.	Other (Please specify: ________________)

# Filter for sections 3-10: recall_sms == 1 | firstemail_recall == 1
# Filter for section 9: deadline == 1 & accepted_offer_date > ymd("2020-09-29") & (accept_offer_recall == 1 | click_recall == 1)

# (3.1): Import survey data.
data_why_activate_later <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, why_activate_later, why_activate_later_other, req_ans_q9.1, anticipated_reminder, unanticipated_reminder) %>% 
  filter(req_ans_q9.1 == 1)
data_why_activate_later %>% tab(why_activate_later)

# (3.2): Import response mapping.
current_mapping <- read_csv(here("data", "Survey response mappings", "mapping_9.1_why_activate_later.csv")) %>% 
  mutate(response_var = str_to_lower(response) %>% str_replace_all(" ", "_"))

# (3.3): Merge two categories: "Had to discuss or think about offer first" and "Had to review the offer".
data_why_activate_later %>% tab(why_activate_later)
data_why_activate_later %<>% mutate(why_activate_later = ifelse(why_activate_later == "6", "2", why_activate_later))

# (3.4): Replace missings (NA) with -888.
data_why_activate_later %<>% mutate_at(vars(why_activate_later), ~ifelse(is.na(.), "-888", .))

# (3.5): Generate dummy response variables and process data for graph.
current_graph_data <- data_why_activate_later %>% 
  gen_all_response_dummies(current_mapping, "why_activate_later") %>% 
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

# (3.6): Relevel responses.
current_graph_data %<>% relevel_data("why_activate_later")


# (3.7): changed.
form_levels <- c("Did not answer",
                 "Other",
                 "Had to discuss or think about offer first",
                 "Had not seen the email",
                 "Was procrastinating",
                 "Was too busy to accept",
                 "Had to review the offer")

graph <- current_graph_data_changed %>%
  mutate(term = fct_reorder(term, coef)) %>%  
  filter(!(term %in% c("Had to review the offer"))) %>%
  graph_barplot() +
  coord_flip() +
  expand_limits(y = 0.6) +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 22))
# Save paper version
graph
qsave(graph, here("proc", "survey_why_activate_later_changed.qs"))
ggsave_(here("results", "figures", "survey_why_activate_later_changed.eps"), width = 8, height = 6)

data_why_activate_later %>% 
  nrow() %>%
  print_n("survey_num_why_activate_later.tex", "Number of firms asked the question - We sent you the emails and SMS to let you know about this offer on September 29, but we see that you filled the form on {activation date}. Why did you wait until {days to accept} day(s) later?", dig = 1)

# (3.7): Define and save footnote.
# Check extent of non response
tabna("why_activate_later", data = data_why_activate_later)

# (3.8): Save numbers for paper.
current_graph_data %>% 
  filter(term == "Was too busy to accept") %>% 
  pull(coef) %>% 
  print_pct("survey_pct_why_activate_later_too_busy.tex",
            "Percentage firms reporting that they did not accept the offer on the first day because they were too busy to accept.")
current_graph_data %>% 
  filter(term == "Had to discuss or think about offer first") %>% 
  pull(coef) %>% 
  print_pct("survey_pct_why_activate_later_discuss_think_offer.tex",
            "Percentage firms reporting that they did not accept the offer on the first day because they had to discuss or think about the offer first")
discuss_offer_split <- data_why_activate_later %>%
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>%
  rowwise() %>%
  mutate(discuss_offer = str_detect_numeric(why_activate_later, "2") %>% as.numeric()) %>%
  group_by(anticipated_reminder, unanticipated_reminder) %>%
  summarise(discuss_offer = mean(discuss_offer))
discuss_offer_split %>% 
  filter(anticipated_reminder == 1) %>% 
  pull(discuss_offer) %>% 
  print_pct("survey_pct_why_activate_later_discuss_offer_antrem.tex",
            "Percentage anticipated reminder firms reporting that they activated later to discuss offer.")
discuss_offer_split %>% 
  filter(unanticipated_reminder == 1) %>% 
  pull(discuss_offer) %>% 
  print_pct("survey_pct_why_activate_later_discuss_offer_unantrem.tex",
            "Percentage unanticipated reminder firms reporting that they activated later to discuss offer.")

# (3.9): Test difference in firms accepting because offer was too good to delay activating.
data_test <- data_why_activate_later %>%
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>%
  rowwise() %>%
  mutate(discuss_offer = str_detect_numeric(why_activate_later, "2") %>% as.numeric()) %>%
  group_by(anticipated_reminder, unanticipated_reminder) %>%
  summarise(discuss_offer = sum(discuss_offer),
            n = n()) %>%
  ungroup() %>%
  select(discuss_offer, n)

# Use binomial proportion test because can't use prop.test function
# https://stats.stackexchange.com/questions/155523/r-prop-test-chi-squared-approximation-may-be-incorrect
binom_test(n1 = data_test$discuss_offer[1],
           N1 = data_test$n[1],
           n2 = data_test$discuss_offer[2],
           N2 = data_test$n[2]) %>% 
  print_n("survey_p_why_activate_later_discuss_offer.tex",
          "Test difference between prop. firms reporting that they activated later to discuss offer in ant. and unant. reminder groups.",
          dig = 3)

rm(data_why_activate_later, graph, current_graph_data, current_mapping, data_test, discuss_offer_split)

