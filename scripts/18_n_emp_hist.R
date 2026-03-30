#--------------------------------------------------------------------------------------------
# File name: 		      18_n_emp_hist.R
# Creation date:      2023-01-17
# Author:          		César Landín
# Files used:
#   - here("data", "INEGI", "LM 833 - 174  Revisión 17 Enero 2023", "n_emp_dist_inc_2023-01-17.xlsx")
#   - here("data", "INEGI", "LM 833 - 174  Revisión 17 Enero 2023", "n_emp_ini_2023-01-17.csv")
#   - here("data", "INEGI", "LM 833 - 174  Revisión 17 Enero 2023", "n_firms.tex")
#   - here("proc", "survey_successful.csv")
# Files created:
#   - here("results", "figures", "hist_survey_nemployees_comp.pdf")
#   - here("results", "numbers", "n_emp_n_max.tex")
#   - here("results", "numbers", "n_emp_pct_census_leq_5.tex")
#   - here("results", "numbers", "n_emp_pct_survey_leq_5.tex")
#   - here("results", "numbers", "n_emp_pct_census_leq_20.tex")
#   - here("results", "numbers", "n_emp_pct_survey_leq_20.tex")
#   - here("results", "numbers", "n_emp_pct_census_leq_150.tex")
#   - here("results", "numbers", "n_emp_census_n_firms.tex")
#   - here("results", "numbers", "n_emp_n_omit.tex")
#   - here("results", "numbers", "n_q_n_omit.tex")
# Purpose:
# 	- # Creates Figure 4: Number of Employees: Process data from INEGI and survey on the distribution of number of employees by firm.
#     Generate a histogram comparing the number of employees by firm in these two datasets.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(readxl)
library(assertthat)
library(scales)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # changed to times for runnning on the server
showtext_auto()
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05a_survey_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
#########################################################

###############################################################################
##    (1): Process number of employee data from survey and economic census.  ##
###############################################################################
# (1.1): Import all data generated at the microdata lab.
inegi_folder <- "LM 833 - 174  Revisión 17 Enero 2023"
inegi_date <- "_2023-01-17"
n_emp_dist_inc <- read_excel(here("data", "INEGI", inegi_folder, str_c("n_emp_dist_inc", inegi_date, ".xlsx")), col_names = c("lb", "ub", "n_firms"))
n_emp_ini <- read_csv(here("data", "INEGI", inegi_folder, str_c("n_emp_ini", inegi_date, ".csv")))
n_firms <- read_file(here("data", "INEGI", inegi_folder, "n_firms.tex")) %>% 
  str_remove(fixed("%")) %>% 
  str_remove_all(",") %>% 
  as.numeric()

# (1.2): Check total number of firms.
# Survey question we use is H001A Personal ocupado total: 
# Comprende a todas las personas que trabajaron durante el 
# periodo de referencia dependiendo contractualmente o no 
# de la unidad económica, sujetas a su dirección y control.
# This only includes employees, because we find firms with 0 employees.

# Number of firms with at least 1 employee
n_emp_dist_inc %>% pull(n_firms) %>% sum()
# Number of firms firms with 0 employees
n_emp_ini %>% filter(n_employees == 0) %>% pull(n_emp_count)
# Check that we have the correct number of firms
assert_that((n_emp_dist_inc %>% pull(n_firms) %>% sum()) +
              (n_emp_ini %>% filter(n_employees == 0) %>% pull(n_emp_count)) ==
              n_firms)
# Update number of employees
n_emp_ini %<>% mutate(n_employees = n_employees + 1) # Add 1 because of question phrasing

# (1.3): Import number of firms from survey.
# Question asked: How many employees work in your business, including yourself?
data_nb_employees <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, nb_employees, req_ans_q1.1) %>% 
  filter(req_ans_q1.1 == 1)
data_nb_employees %>% tab(nb_employees)

# (1.4): Process survey number of employees.
survey_graph_data <- data_nb_employees %>% 
  filter(!is.na(nb_employees) & nb_employees != -888) %>% 
  tab(nb_employees) %>% 
  mutate(pct_firms = N / sum(N)) %>% 
  transmute(n_employees = nb_employees,
            pct_firms) %>% 
  arrange(n_employees)
max_svy_n_emp <- survey_graph_data %>% pull(n_employees) %>% max()

# (1.5): Process economic census number of employees.
inegi_graph_data <- n_emp_ini %>% 
  mutate(pct_firms = n_emp_count / n_firms) %>% 
  select(-n_emp_count)

# (1.6): Prepare data for figure.
inegi_graph_data_final <- inegi_graph_data %>% 
  filter(n_employees < 20) %>% 
  mutate(n_employees = as.character(n_employees)) %>% 
  bind_rows(inegi_graph_data %>%
              filter(n_employees >= 20) %>% 
              summarise_all(sum) %>% 
              mutate(n_employees = "20+"))
inegi_graph_data_final %<>% mutate(n_employees = factor(n_employees, levels = inegi_graph_data_final$n_employees))


# Create a sequence of n_employees from 0 to 19
complete_employees <- tibble(n_employees = as.character(1:19))

# Prepare your survey data
survey_graph_data_final <- survey_graph_data %>%
  filter(n_employees < 20) %>%
  mutate(n_employees = as.character(n_employees)) %>%
  full_join(complete_employees, by = "n_employees") %>%
  replace(is.na(.), 0) %>%
  bind_rows(
    survey_graph_data %>%
      filter(n_employees >= 20) %>%
      summarise_all(sum) %>%
      mutate(n_employees = "20+")
  )

survey_graph_data_final %<>% mutate(n_employees = factor(n_employees, levels = survey_graph_data_final$n_employees))

# final_survey_inegi <- survey_graph_data_final_test |>
#   left_join(inegi_graph_data_final, by = "n_employees")

rm(inegi_date, inegi_folder, n_emp_dist_inc)

#####################################################
##    (2): Print numbers for paper and footnotes.  ##
#####################################################
# (2.1): Number of employees in firm with most employees in survey sample.
max_svy_n_emp %>% 
  print_n("n_emp_n_max.tex",
          "Number of employees in firm with most employees in survey sample")

# (2.2): Percentage of firms in sample with <= N employees.
print_ntile <- function(df, ntile, data) {
  df %>% 
    filter(n_employees <= ntile) %>% 
    pull(pct_firms) %>% 
    sum() %>% 
    print_pct(str_c("n_emp_pct_", data, "_leq_", ntile, ".tex"),
              str_c("Percentage of firms in ", data, " with <= ", ntile, " employees"))
}
inegi_graph_data %>% print_ntile(5, "census")
survey_graph_data %>% print_ntile(5, "survey")
inegi_graph_data %>% print_ntile(20, "census")
survey_graph_data %>% print_ntile(20, "survey")
inegi_graph_data %>% print_ntile(150, "census")
inegi_graph_data %>% 
  filter(n_employees <= 150) %>% 
  pull(pct_firms) %>% 
  sum() %>%
  `*`(100) %>% 
  round(2) %>% 
  print_n("n_emp_pct_census_leq_150.tex",
          "Percentage of firms in INEGI census with <= 150 employees")

###########################################################
##    (3): Generate histogram comparing to survey data.  ##
###########################################################
# (3.1): Generate histogram.
ggplot() +
  geom_col(data = survey_graph_data_final, 
           aes(x = n_employees, y = pct_firms, fill = "RCT sample"),
           alpha = 1) +
  geom_col(data = inegi_graph_data_final,
           aes(x = n_employees, y = pct_firms, fill = "INEGI Economic Census"), color = "black", alpha = 0) +
  geom_hline(yintercept = 0, linewidth = 0.425, color = "black") +
  set_theme() +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) + #, expand = expansion(0)
  ylab("Percentage of Firms") + 
  xlab("Number of Employees") +
  scale_fill_manual(name = "", breaks = c("RCT sample", "INEGI Economic Census"), values = c("lightblue", "white")) +
  scale_x_discrete(breaks = as.character(c(1, 5, 10, 15, "20+")), expand = expansion(mult = 0.05)) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = c(0.75, 0.85),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(family = "Times New Roman", size = 12, margin = margin(r = 30, unit = "pt")),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 1),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  guides(fill = guide_legend(override.aes = list(color = c(NA, "black"))))
ggsave_(here("results", "figures", "hist_survey_nemployees_comp.pdf"), width = 6, height = 4, device = cairo_pdf)

# (3.2): Define numbers for footnotes.
n_firms %>%
  print_n("n_emp_census_n_firms.tex",
          "2019 INEGI Economic Census number of firms")
n_omit <- get_n_omitted("nb_employees", data = data_nb_employees)
n_omit %>% 
  print_n("n_emp_n_omit.tex",
          "Number of omitted firms")
n_q_n_omit <- nrow(data_nb_employees) - n_omit
n_q_n_omit %>% 
  print_n("n_q_n_omit.tex",
          "Question sample size")

