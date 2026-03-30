#--------------------------------------------------------------------------------------------
# File name: 		      19_non_adoption_bar_plot.R
# Creation date:      2021-01-01
# Author:          		César Landín
# Files used:
#   - here("proc", "colors.csv")
#   - here("proc", "survey_successful.csv")
# Files created:
#   - here("proc", "why_firm_did_not_adopt_data.csv")
#   - here("proc", "survey_non_adoption_pct.csv")
#   - here("results", "figures", "survey_non_adoption_all_pct.eps")
#   - here("results", "numbers", "survey_non_adoption_other.tex")
#   - here("results", "numbers", "survey_non_adoption_dna.tex")
#   - here("results", "numbers", "survey_non_adoption_n.tex")
#   - here("results", "numbers", "survey_non_adoption_forgot.tex")
#   - here("results", "numbers", "survey_non_adoption_open_email_dont_remember.tex")
# Purpose:
# 	- Figure C.4: Reasons Why Firms Did Not Adopt Offer: Generate bar graph with reasons for not adopting the offer.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(assertthat)
library(rcartocolor)
library(tabulator)
library(scales)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # changed to times for running on the server
showtext_auto()
group_colors <- read_csv(here("proc", "colors.csv"))
source(here("scripts", "programs", "05c_barplot_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

#######################################
##    (1): Initial data processing.  ##
#######################################
# (1.1): Import successful survey data.
survey_data <- read_csv(here("proc", "survey_successful.csv"), show_col_types = FALSE)
  # filter(successful == 1)
  # mutate(rem_type = ifelse(anticipated_reminder == 1, "Anticipated Reminder", "Unanticipated Reminder"))

# (1.2): Keep firms that did not accept offer by the time they were surveyed.
non_adoption_data <- survey_data %>% filter(accepted_offer_survey == 0)

# (1.3): Tab different reasons for not accepting offer.
non_adoption_data %>% 
  tab(open_email_reminder_survey, clicked_email_reminder_link_survey) %>% 
  arrange(open_email_reminder_survey)

# (1.4): Generate reason for non-adoption variables.
non_adoption_data %<>%
  mutate(reason_no_adopt = case_when(open_email_reminder_survey == 0 ~ "Did not open email or reminder",
                                     open_email_reminder_survey == 1 & clicked_email_reminder_link_survey == 0 ~ "Did not click link",
                                     open_email_reminder_survey == 1 & clicked_email_reminder_link_survey == 1 ~ "Clicked link but did not accept offer"),
         reason_no_adopt = factor(reason_no_adopt, levels = c("Did not open email or reminder", "Did not click link", "Clicked link but did not accept offer")))
non_adoption_data %>% tab(reason_no_adopt) %>% arrange(reason_no_adopt)

# (1.5): Review skip patterns. 
# All recall questions should have been asked to firms that recalled sms (recall_sms == 1) or recalled opening the first email (firstemail_recall == 1)
# Question 3 (Why not open email?) -> Firm did not open email
# Question 4 (Why not click on link?) -> Firm opened email but did not click on link
# Question 5 (Why not complete form?) -> Firm opened email, clicked on link but did not accept offer
non_adoption_data  %<>%
  rowwise() %>% 
  mutate(ask_why_no_open = ifelse(!is.na(why_no_open), 1, 0),
         ask_why_no_click = ifelse(!is.na(why_no_click), 1, 0),
         ask_why_no_accept = ifelse(!is.na(why_no_accept), 1, 0),
         recall_firstemail_sms = sum(recall_sms, firstemail_recall, na.rm = TRUE),
         recall_firstemail_sms = ifelse(recall_firstemail_sms >= 1, 1, 0))
non_adoption_data %>% tab(recall_firstemail_sms)
tab_arrange <- function(df) {
  df %>% 
    tab(recall_firstemail_sms, ask_why_no_open, ask_why_no_click, ask_why_no_accept) %>% 
    arrange(recall_firstemail_sms, ask_why_no_open, ask_why_no_click, ask_why_no_accept)
}
non_adoption_data %>% filter(reason_no_adopt == "Did not open email or reminder") %>% tab_arrange()
non_adoption_data %>% filter(reason_no_adopt == "Did not click link") %>% tab_arrange()
non_adoption_data %>% filter(reason_no_adopt == "Clicked link but did not accept offer") %>% tab_arrange()
# These questions were never asked to firms that shouldn't have been asked recall questions,
# but were sometimes asked to firms that should've been asked a different question.

#########################################################
##    (2): Process firms that did not open the email.  ##
#########################################################

# Filter for this section: (opened_email == 0 & opened_reminder == 0 & click_recall <= 0 & accepted_offer == 0)

# (2.1): Tab admin reason for non-adoption and whether firm was asked why did not open email question and recalls.
non_adoption_data %>% tab(reason_no_adopt, ask_why_no_open) %>% arrange(reason_no_adopt)
# Now explore firms that did not open the email. Was this because they opened SMS directly?
non_adoption_data %>% 
  filter(reason_no_adopt == "Did not open email or reminder") %>% 
  tab(ask_why_no_open, recall_firstemail_sms, click_recall) # Click recall is only asked if recall first email or SMS.
# Another way of tabbing this:
non_adoption_data %>% 
  mutate(why_no_open_filter = (open_email_reminder_survey == 0 & (click_recall <= 0 | is.na(click_recall)) & accepted_offer_survey == 0) %>% as.numeric()) %>% 
  tab(why_no_open_filter, open_email_reminder_survey, ask_why_no_open) %>% 
  arrange(why_no_open_filter, open_email_reminder_survey)

# (2.2): Set reason for non adoption if did not open email and does not recall email or SMS.
ans_proc <- non_adoption_data %>% 
  filter(reason_no_adopt == "Did not open email or reminder" & ask_why_no_open == 0 & recall_firstemail_sms == 0) %>% 
  # mutate(reason_non_adoption = "Did not open email")
  mutate(why_no_open = "10")
filter_ans_proc <- function(df) {
  df %>% filter(!(organization_uuid %in% ans_proc$organization_uuid))
}
ans_rem <- non_adoption_data %>% filter_ans_proc()

# (2.3): Review firms that were asked question.
current_ans <- ans_rem %>% filter(reason_no_adopt == "Did not open email or reminder" & ask_why_no_open == 1)
current_ans %>% tab(why_no_open, why_no_open_alt)

# (2.4): Recode answers.
current_ans %<>% mutate(why_no_open = why_no_open_alt) # Leave remaining -666 (1 obs) as other
ans_proc %<>% bind_rows(current_ans)
ans_rem %<>% filter_ans_proc()

# (2.5): Check firms that where not asked question.
current_ans <- ans_rem %>% filter(reason_no_adopt == "Did not open email or reminder" & ask_why_no_open == 0)

# (2.6): Confirm firms were filtered correctly.
# Filter for ALL following sections: recall_sms == 1 or firstemail_recall == 1
current_ans %>% tab(recall_firstemail_sms, click_recall) # All these firms recall clicking on link
current_ans %>% tab(recall_firstemail_sms, click_recall, ask_why_no_click, ask_why_no_accept)

# (2.7): All of these firms recall clicking on link: send to why no accept.
# These are firms that could've clicked on the SMS but not on the email
current_ans %<>% mutate(reason_no_adopt = "Clicked link but did not accept offer")
current_ans %>% tab(reason_no_adopt)
ans_rem %<>%
  filter(!(organization_uuid %in% current_ans$organization_uuid)) %>% 
  bind_rows(current_ans)

# (2.8): Confirm we don't have duplicates.
assert_that(nrow(ans_rem) + nrow(ans_proc) == nrow(non_adoption_data))

############################################################
##    (3): Process firms that did not click on the link.  ##
############################################################

# Filter for this section: ((opened_email == 1 | opened_reminder == 1) & click_recall <= 0 & accepted_offer = 0)

# (3.1): Work on firms that did not click on link.
ans_rem %>% tab(reason_no_adopt)
current_ans <- ans_rem %>% filter(reason_no_adopt == "Did not click link")

# (3.2): Tab admin reason for non-adoption and whether firm was asked why did not click question.
current_ans %>% tab(reason_no_adopt, ask_why_no_click, click_recall, recall_firstemail_sms, ask_why_no_accept) %>% arrange(ask_why_no_click)
# Three types of fims:
# 1. Firms that recall clicking and should have been asked why no accept (send to why no accept)
# 2. Firms that do not recall first email or sms (even though they did open it)
# 3. Firms that opened email and did not click on link

# (3.3): Process firms that recall clicking on link.
current_ans %<>% mutate(reason_no_adopt = ifelse(!is.na(click_recall) & click_recall == 1, "Clicked link but did not accept offer", reason_no_adopt))
recall_click <- current_ans %>% filter(reason_no_adopt == "Clicked link but did not accept offer")
ans_rem %<>% 
  filter(!(organization_uuid %in% recall_click$organization_uuid)) %>% 
  bind_rows(recall_click)
current_ans %<>% filter(reason_no_adopt == "Did not click link")
rm(recall_click)

# (3.4): Process firms that do not recall first email or SMS (even though they did open it).
no_recall <- current_ans %>% 
  filter(recall_firstemail_sms == 0) %>% 
  mutate(reason_no_adopt = "Did not open email or reminder",
         why_no_click = "11") # "Opened email but did not remember doing so"
ans_proc %<>% bind_rows(no_recall)
ans_rem %<>% filter_ans_proc()
current_ans %<>% filter_ans_proc()
rm(no_recall)

# (3.5): Process firms that opened email and did not click on link.
current_ans %>% tab(why_no_click, why_no_click_alt)
current_ans %>% filter(str_detect(why_no_click_alt, "666|777")) %>% tab(why_no_click_other)
# Recode answers
current_ans %<>%
  mutate(ask_why_no_click = 1, # Send 1 firm that hadn't been asked question to "did not answer"
         why_no_click = why_no_click_alt,
         why_no_click = case_when(why_no_click_other == "Menciona tener un plan de comisiones mas bajos tipo dinamico entre mas ventas menos comision para el negocio" ~ "7",
                                  why_no_click_other == "si lleno el formato pero no tuvo respuesta confirmo correo" ~ "8",
                                  click_recall == -777 ~ "12", #"Does not remember clicking on link"
                                  is.na(why_no_click_alt) ~ "-777",
                                  TRUE ~ why_no_click))
current_ans %>% tab(why_no_click)
ans_proc %<>% bind_rows(current_ans)
ans_rem %<>% filter_ans_proc() 

# (3.6): Confirm we don't have duplicates.
assert_that(nrow(ans_rem) + nrow(ans_proc) == nrow(non_adoption_data))

#######################################################
##    (4): Process firms that did not accept offer.  ##
#######################################################

# Filter for this section: (click_recall == 1 & accepted_offer = 0)

# (4.1): Work on firms that did not accept the offer.
ans_rem %>% tab(reason_no_adopt)
current_ans <- ans_rem

# (4.2): Tab admin reason for non-adoption and whether firm was asked why did not click question.
current_ans %>% tab(click_recall, ask_why_no_open, ask_why_no_click, ask_why_no_accept, open_email_reminder_survey) %>% arrange(click_recall)
# Three types of fims:
# 1. Firms that did click but do not recall clicking.
# 2. Firms that recall clicking but weren't asked this question.
# 4. Firms that were asked question correctly.

# (4.3): Process firms that clicked on link but don't recall clicking.
clicked_link <- current_ans %>% 
  filter(is.na(click_recall) | click_recall == 0) %>% 
  mutate(why_no_accept = "13")
ans_proc %<>% bind_rows(clicked_link)
ans_rem %<>% filter_ans_proc()
current_ans %<>% filter_ans_proc()
rm(clicked_link)

# (4.4): Process firms that recall clicking but weren't asked this question.
current_ans %>% 
  filter(ask_why_no_accept == 0) %>% 
  tab(recall_firstemail_sms, click_recall, open_email_reminder_survey, clicked_email_reminder_link_survey)
# All of these firms recall the first email, clicking on the link but weren't asked the question. Send to no answer.
no_answer <- current_ans %>% 
  filter(ask_why_no_accept == 0) %>% 
  mutate(why_no_accept = "-777")
ans_proc %<>% bind_rows(no_answer)
ans_rem %<>% filter_ans_proc()
current_ans %<>% filter_ans_proc()
rm(no_answer)

# (4.5): Process firms that were asked this question correctly.
current_ans %>% filter(str_detect(why_no_accept_alt, "666|777")) %>% tab(why_no_accept_other)
current_ans %<>% mutate(why_no_accept = why_no_accept_alt)
ans_proc %<>% bind_rows(current_ans)
ans_rem %<>% filter_ans_proc()
current_ans %<>% filter_ans_proc()   

# (4.6): Confirm we don't have duplicates.
assert_that(nrow(ans_rem) + nrow(ans_proc) == nrow(non_adoption_data))
rm(ans_rem, current_ans)

#####################################
##    (5): Process final dataset.  ##
#####################################
# (5.1): Recode dummy question variables.
final_data <- ans_proc %>%
  select(organization_uuid, open_email_reminder_survey, recall_firstemail_sms,
         why_no_open, why_no_click, why_no_accept) %>%
  mutate(ask_why_no_open = ifelse(!is.na(why_no_open), 1, 0),
         ask_why_no_click = ifelse(!is.na(why_no_click), 1, 0),
         ask_why_no_accept = ifelse(!is.na(why_no_accept), 1, 0))

# (5.2): Keep latest answer as correct.
final_data %>% 
  tab(ask_why_no_open, ask_why_no_click, ask_why_no_accept) %>% 
  arrange(ask_why_no_open, ask_why_no_click, ask_why_no_accept)
final_data %<>%
  mutate(why_no_open = ifelse(!is.na(why_no_click), NA, why_no_open),
         why_no_click = ifelse(!is.na(why_no_accept), NA, why_no_click),
         ask_why_no_open = ifelse(!is.na(why_no_open), 1, 0),
         ask_why_no_click = ifelse(!is.na(why_no_click), 1, 0),
         ask_why_no_accept = ifelse(!is.na(why_no_accept), 1, 0))
final_data %>% tab(ask_why_no_open, ask_why_no_click, ask_why_no_accept)

# (5.3): Define non-adoption variable.
final_data %<>%
  mutate(why_no_adopt = case_when(ask_why_no_open == 1 ~ why_no_open,
                                  ask_why_no_click == 1 ~ why_no_click,
                                  ask_why_no_accept == 1 ~ why_no_accept))
final_data %>% tab(why_no_adopt)

# (5.4): Define parameters for variable processing.
responses <- tribble(~response, ~why_no_open, ~why_no_click, ~why_no_accept, ~short_response,
                     "Did not see email",                    1,  -888,  -888, "did_not_see_email", 
                     "Did not see where to activate offer",  -888,  1,  -888, "did_not_see_link",
                     "Forgot",                               2,  2,  1,  "distracted",
                     "Ran out of time",                      3,  3,  2,  "no_time",
                     "Thought it would take too much time",  4,  4,  3,  "too_much_time",
                     "Didn't consider important",            5,  5,  4,  "not_important",
                     "Thought it was scam or fake",          6,  6,  5,  "scam_fake",
                     "Wasn't sure if it would benefit",      7,  7,  6,  "no_benefit",
                     "Had already clicked on SMS link",      8,  -888,  -888,  "click_sms",
                     "Didn't fill out form correctly",       9,  8,  8,  "no_form",
                     "Does not remember receiving emails or messages", 10, 10, 10, "no_email_no_recall",
                     "Opened email but did not remember doing so", 11, 11, 11, "open_email_no_recall",
                     "Does not remember clicking on link", 12, 12, 12, "no_link_no_recall",
                     "Clicked on link but did not remember doing so", 13, 13, 13, "click_link_no_recall",
                     # "Does not recall clicking on link", 11, 11, 11, "no_recall_click", # previously 11
                     "Did not answer", -777, -777, -777, "no_answer",
                     "Other",          -666, -666, -666, "other")

# (5.5): Generate dummy variables.
str_detect_numeric <- function(string, number) {
  if (string %in% c("-777", "-666")) {
    ifelse(string == as.character(number), 1, 0)
  } else {
    ((str_replace_all(string, " ", "x") %>% 
      str_split("x") %>% 
      .[[1]] %>% 
       as.numeric()) == number) %>% 
      sum()
  }
}
for (r in 1:nrow(responses)) {
  final_data %<>%
    mutate_at(vars(contains("why_no_")), ~ifelse(is.na(.), "", .)) %>% 
    mutate(!!str_c("r_no_open_", responses$short_response[r]) := str_detect_numeric(why_no_open, responses$why_no_open[r]),
           !!str_c("r_no_click_", responses$short_response[r]) := str_detect_numeric(why_no_click, responses$why_no_click[r]),
           !!str_c("r_no_accept_", responses$short_response[r]) := str_detect_numeric(why_no_accept, responses$why_no_accept[r])) %>% 
    mutate_at(vars(starts_with("r_")), ~ifelse(is.na(.), 0, ifelse(. > 1, 1, .))) %>% 
    mutate(!!str_c("r_no_adopt_", responses$short_response[r]) := 
             eval(as.name(str_c("r_no_open_", responses$short_response[r]))) +
             eval(as.name(str_c("r_no_click_", responses$short_response[r])))+ 
             eval(as.name(str_c("r_no_accept_", responses$short_response[r]))))
}
final_data %>% tab(r_no_open_no_benefit, r_no_click_no_benefit, r_no_accept_no_benefit, r_no_adopt_no_benefit)

why_firm_did_not_adopt_data <- final_data
write.csv(final_data, file = here("proc", "why_firm_did_not_adopt_data.csv"))
# (5.6): Generate dataset for graph.
graph_data <- final_data %>% 
  select(contains("r_no_adopt")) %>% 
  ungroup() %>% 
  summarise_all(sum) %>% 
  pivot_longer(cols = everything(), names_to = "short_response", values_to = "n") %>% 
  mutate(coef = n / nrow(final_data)) %>% 
  left_join(responses %>%
              mutate(short_response = str_c("r_no_adopt_", short_response)),
            by = "short_response") %>% 
  mutate(lci = 0,
         uci = 0) %>%
  select(response, n, coef, lci, uci) %>%
  rename(term = response) %>% 
  filter(coef >= 0.0001) %>% 
  arrange(desc(coef))
write_csv(graph_data, here("proc", "survey_non_adoption_pct.csv"))

###########################################
##    (6): Generate non-adoption graph.  ##
###########################################
# (6.1): Generate final graph.
graph <- graph_data %>% 
  #filter(!(term %in% c("Other", "Did not answer"))) %>% 
  mutate(term = factor(term,
                       levels = rev(graph_data %>% pull(term)))) %>% 
  graph_barplot() +
  theme(axis.text.x = element_text(family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title.y = element_text(family = "Times New Roman"),
        axis.title.x = element_text(family = "Times New Roman")) +
  coord_flip()

# Save paper version
graph
ggsave_(here("results", "figures", "survey_non_adoption_all_pct.eps"), width = 10, height = 8)

# (6.2): Define numbers for footnotes.
n_dna <- graph_data %>% filter(term == "Did not answer") %>% pull(n) %>% sum()
n_other <- graph_data %>% filter(term == "Other") %>% pull(n) %>% sum()
n_other %>%
  print_n("survey_non_adoption_other.tex",
          "Number of firms answered ''Other''")
n_dna <- graph_data %>% filter(term == "Did not answer") %>% pull(n) %>% sum()
n_dna %>%
  print_n("survey_non_adoption_dna.tex",
          "Number of firms did not answer the question")
n_did_not_adopt <- nrow(final_data) - n_dna - n_other
n_did_not_adopt %>%
  print_n("survey_non_adoption_n.tex",
          "Number of firms that did not adopt offer")
graph_data %>% 
  filter(term == "Forgot") %>% 
  pull(coef) %>% 
  print_pct("survey_non_adoption_forgot.tex",
            "% of firms did not adopt because they forgot")
graph_data %>% 
  filter(term == "Opened email but did not remember doing so") %>% 
  pull(coef) %>% 
  print_pct("survey_non_adoption_open_email_dont_remember.tex",
            "% of firms did not adopt because they opened the email but did not remember doing so")
