#--------------------------------------------------------------------------------------------
# File name: 		      22e_admin_bar_plots.R
# Creation date:      2022-01-18
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# 	- here("proc", "regs_for_figs.qs")
#   - here("proc", "fintech_fee_light.csv")
#   - here("proc", "colors.csv")
# Files created:
#   - here("results", "numbers", "stat_n_no_control_no_24.tex")
#   - here("results", "figures", "cum_takeup_treat_fee_barplot.eps")
#   - here("results", "figures", "cum_takeup_treat_fee_barplot.pdf")
# Purpose:
# 	- Figure C.7: Take-up by Treatment Arm: Generate administrative data bar plots: take-up by treatment.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(qs)
library(modelsummary)
library(cowplot)
library(gridExtra)
library(showtext)
font_add(family = "Times New Roman", regular = "Times.ttf")
showtext_auto()
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "05c_barplot_functions.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
group_colors <- read_csv(here("proc", "colors.csv"))
#########################################################

#######################################################
##    (1): Takeup on day of deadline by treatment.   ##
#######################################################
# (1.1): Import regressions.
all_regressions <- qread(here("proc", "regs_for_figs.qs"))
for (i in 1:length(all_regressions)) {
  object <- all_regressions[[i]]
  assign(names(all_regressions)[i], object)
}
rm(list = ls()[str_detect(ls(), "reg")] %>% .[!(.%in% c("reg_cum_daily", "reg_cum_daily_fee", "reg_cum_daily_fee_fe_fullsample", "reg_cum_daily_fee_fast_deadline", "reg_cum_daily_fee_independent_fast_deadline"))])

# (1.2): Keep deadline day (October 6).
reg_cum_daily <- reg_cum_daily$`Oct 6`
reg_cum_daily_fee_independent_fast_deadline <- reg_cum_daily_fee_independent_fast_deadline$`Oct 6`

# (1.3*): Get coef, standard error, and confidence intervals from modelplot.
treat_takeup_data <- modelplot(reg_cum_daily)[["data"]] %>% 
  mutate(term = case_when(term == "treat_T2" ~  "No deadline,\nno reminder",
                          term == "treat_T3" ~  "No deadline,\nannounced reminder",
                          term == "treat_T4" ~  "No deadline,\nunannounced reminder",
                          term == "treat_T5" ~  "Deadline,\nno reminder",
                          term == "treat_T6" ~  "Deadline,\nannounced reminder",
                          term == "treat_T7" ~  "Deadline,\nunannounced reminder",
                          term == "treat_T8" ~  "Same-day deadline,\nno reminder"),
         order = c(1, 3, 2, 4, 6, 5, 7)) %>% 
  arrange(-order) %>% 
  mutate(term = factor(term, levels = term)) %>% 
  rename(lci = conf.low) %>% 
  rename(uci = conf.high) %>% 
  rename(coef = estimate)

# (1.5*): Generate and save plot.
current_cols <- group_colors %>% 
  filter(str_detect(group, "anticipated|deadline")) %>% 
  arrange(group) %>% 
  mutate(col2 = ifelse(str_detect(group, "no deadline"), NA, col2)) %>% 
  pivot_longer(cols = col1:col2, values_to = "col") %>% 
  filter(!is.na(col)) %>% 
  mutate(order = c(7, 3, 4, 6, 1, 5, 2)) %>% 
  arrange(-order) %>% 
  pull(col)

plot_1 <- ggplot(data = treat_takeup_data, aes(x = term, y = coef, fill = term)) +
  geom_col() +
  set_theme(size = 17) +
  labs(x = NULL, y = NULL) +
  scale_x_discrete() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.35)) +
  scale_fill_manual(values = current_cols) +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = margin(0, 0, 0, 0),
        text = element_text(family = "Times New Roman")) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = .2)) +
  coord_flip()
plot_1

# (1.6): Count number of observations.
n_obs_no_control_no_24 <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
  filter(!(treat_type %in% c("T1", "T8"))) %>% 
  count() %>% 
  pull(n)
n_obs_no_control_no_24 %>% 
  print_n("stat_n_no_control_no_24.tex", 
          "Number of firms without control and same-day dedline groups.")

rm(reg_cum_daily, treat_takeup_data, i)

##################################################################################
##    (3): Takeup on day of deadline by fee (including same-day (fast) deadline).   ##
##################################################################################
# (3.1): Get coef, standard error, and confidence intervals.

fee_takeup_fast_deadline_data <- modelplot(reg_cum_daily_fee_independent_fast_deadline)[["data"]] %>% 
  mutate(term = case_when(term == "fee_2.75_control" ~  "2.75% fee",
                          term == "fee_3_control" ~  "3.00% fee"),
          order = c(1, 2)) %>% 
  mutate(term = factor(term, levels = term)) %>% 
  rename(lci = conf.low) %>% 
  rename(uci = conf.high) %>% 
  rename(coef = estimate)

# (3.2): Generate and save plot.

plot_2 <- ggplot(data = fee_takeup_fast_deadline_data, aes(x = term, y = coef, fill = term)) +
  geom_col() +
  set_theme(size = 17) +
  labs(x = NULL, y = NULL) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 27)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.35)) +
  scale_fill_manual(values = c("#CC6677", "#88CCEE")) +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.margin = margin(0, 0, 0, 0),
        text = element_text(family = "Times New Roman")) +
  geom_errorbar(aes(ymin = lci, ymax = uci, width = .2)) +
  coord_flip()
plot_2

rm(fee_takeup_fast_deadline_data, reg_cum_daily_fee_fast_deadline)

plot_grid(
  plot_2 + theme(axis.text.x = element_blank()), NULL,
  plot_1,
  label_size = 26, hjust = 0,
  nrow = 3, align = "v", rel_heights = c(2.25/10.5, 0.75/10.5, 7.5/10.5)
)

ggsave_(here("results", "figures", "cum_takeup_treat_fee_barplot.eps"), width = 9, height = 10)
ggsave_(here("results", "figures", "cum_takeup_treat_fee_barplot.pdf"), width = 9, height = 10)

