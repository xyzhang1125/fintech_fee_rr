#--------------------------------------------------------------------------------------------
# File name: 		      22a_admin_graphs_functions.R
# Creation date:      2022-11-02
# Author:          		César Landín
# Files used:
# 	- here("proc", "colors.csv")
# Files created:
#   - (none)
# Purpose:
# 	- Define functions for take-up and treatment effect graphs.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(lubridate)
library(scales)
library(data.table)
library(fixest)
library(tabulator)
library(multcomp)
library(conflicted)
library(qs)
library(modelsummary)
conflict_prefer("last", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "set_theme.R"))
group_colors <- read_csv(here("proc", "colors.csv"))
#########################################################

###########################################################
##  (1): Define graph functions and general parameters.  ##
###########################################################
# (1.1): Graph pre-treatment sales.
graph_pretreat_sales <- function(data, variable) {
  if (variable == "timestamp_week") {
    label = "Weekly"
  } else {
    label = "Monthly"
  }
  data %>% 
    filter(name != "Accepted Offer" & eval(as.name(variable)) <= ymd("2020-08-31")) %>% 
    mutate(name = ifelse(name == "Received Offer", "Treatment", "Control") %>% as.factor()) %>%
    ggplot(aes(x = ymd(eval(as.name(variable))), 
               y = value, 
               group = name, 
               color = name)) + 
    geom_line() + 
    geom_point(size = 1) + 
    scale_x_date(name = "",
                 date_labels = "%b %Y",
                 date_breaks = "1 month") +
    scale_y_continuous(labels = comma) +
    set_theme() + 
    scale_colour_manual(values = c("#AE9FF3", "#5DD488"))  + 
    ylab(str_c("Average ", label, " Sales (Pesos)")) + 
    ggtitle(str_c("Average ", label, " Sales by Group"))  +
    geom_hline(yintercept = 0) + 
    theme(legend.position = "right",
          legend.title = element_blank(),
          legend.key = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 13, margin = margin(t = -8)),
          plot.margin = margin(0, 0, 0, 0))
}

# (1.2): Calculate take-up rate by treatment group.
takeup_rate_pooled <- function(group_name, y_var = "accepted_offer", open_email = FALSE, no_deadline = FALSE) {
  # Open email filter.
  filtered_data <- fintech_fee
  if (open_email) {
    filtered_data %<>% filter(open_email_before_reminder == 1)
  }
  # Deadline filter.
  if (no_deadline) {
    filtered_data %<>% filter(no_deadline == 1)
  }
  # Define y-axis variable
  filtered_data %<>% 
    mutate(y_var_late = eval(as.name(str_c(y_var, "_late"))),
           y_var_date = eval(as.name(str_c(y_var, "_date"))))
  # Keep only var1 == 1 or var2 == 1.
  if (group_name == "rem") {
    variable1 <- "reminder"
    variable2 <- "no_reminder"
  } else if (group_name == "dl") {
    variable1 <- "deadline"
    variable2 <- "no_deadline"
  } else if (group_name == "antrem") {
    variable1 <- "anticipated_reminder"
    variable2 <- "unanticipated_reminder"
  } else if (group_name == "fee") {
    variable1 <- "fee_2.75"
    variable2 <- "fee_3"
  } else if (group_name == "unantrem") {
    variable1 <- "unanticipated_reminder"
    variable2 <- "no_reminder"
  }
  filtered_data %<>%
    rename(var1 = all_of(variable1),
           var2 = all_of(variable2)) %>% 
    select(y_var_date, var1, var2, y_var_late) %>% 
    filter(var1 == 1 | var2 == 1) %>% 
    mutate(grouping = if_else(var1 == 1, variable1, variable2))
  # Count by group.
  group_raw_counts <- filtered_data %>% 
    group_by(grouping) %>% 
    count()
  # Generate empty dataframe with full set of dates.
  empty_df <- cross_join(group_raw_counts %>% select(grouping) %>% unique(),
                         seq(ymd("2020-09-29"), ymd("2021-03-31"),
                             by = "1 day"),
                         copy = TRUE) %>% 
    rename(date = y)
  # Calculate take-up dates.
  y_var_dates <- filtered_data %>% 
    group_by(y_var_date, grouping) %>% 
    summarise_all(sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(date = y_var_date) %>% 
    mutate(is_y_var_date = 1,
           date = as_date(date))
  # Merge take-up dates into full dataframe.
  final_df <- empty_df %>% 
    left_join(y_var_dates, by = c("date", "grouping")) %>% 
    mutate(y_var_late = ifelse(is.na(y_var_late), 0, y_var_late),
           is_y_var_date = ifelse(is.na(is_y_var_date), 0, is_y_var_date),
           is_y_var_date = ifelse(date <= "2020-10-06", 1, is_y_var_date)) %>% 
    group_by(grouping) %>% 
    mutate(n_accept_fee_cum = cumsum(y_var_late)) %>% 
    ungroup() %>% 
    left_join(group_raw_counts, by = c("grouping")) %>%
    mutate(takeup_rate = n_accept_fee_cum/n)
  final_df[is.na(final_df)] <- 0
  # Rename variables
  final_df %<>%
    rename(!!variable1 := var1, 
           !!variable2 := var2,
           !!group_name := grouping)
  return(final_df)
}

# (1.3): Calculate take-up rate, heterogeneities.
takeup_rate_het <- function(grouping, y_var = "accepted_offer", open_email = FALSE) {
  # Open email filter.
  if (open_email) {
    filtered_data <- fintech_fee %>% filter(open_email_before_reminder == 1)
  } else {
    filtered_data <- fintech_fee
  }
  # Define y-axis variable
  filtered_data %<>% 
    mutate(y_var_late = eval(as.name(str_c(y_var, "_late"))),
           y_var_date = eval(as.name(str_c(y_var, "_date"))))
  # Filter data.
  filtered_data %<>%
    select(y_var_date, deadline, no_deadline, all_of(grouping), y_var_late) %>% 
    rename(grouping = all_of(grouping)) %>% 
    filter(!is.na(grouping))
  # Count by group.
  group_raw_counts <- filtered_data %>% 
    group_by(grouping) %>% 
    count()
  # Generate empty dataframe with full set of dates.
  empty_df <- cross_join(group_raw_counts %>% select(grouping) %>% unique(),
                         seq(ymd("2020-09-29"), ymd("2021-03-31"),
                             by = "1 day"),
                         copy = TRUE) %>% 
    rename(date = y)
  # Calculate take-up dates.
  y_var_dates <- filtered_data %>% 
    group_by(y_var_date, grouping) %>% 
    summarise_all(sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    rename(date = y_var_date) %>% 
    mutate(is_y_var_date = 1,
           date = as_date(date))
  # Merge take-up dates into full dataframe.
  final_data <- empty_df %>% 
    left_join(y_var_dates, by = c("date", "grouping")) %>% 
    mutate(y_var_late = ifelse(is.na(y_var_late), 0, y_var_late),
           is_y_var_date = ifelse(is.na(is_y_var_date), 0, is_y_var_date),
           is_y_var_date = ifelse(date <= "2020-10-06", 1, is_y_var_date)) %>% 
    group_by(grouping) %>% 
    mutate(n_accept_fee_cum = cumsum(y_var_late)) %>% 
    ungroup() %>% 
    left_join(group_raw_counts, by = c("grouping")) %>%
    mutate(takeup_rate = n_accept_fee_cum/n) %>% 
    rename(!!grouping := grouping)
  final_data[is.na(final_data)] <- 0
  return(final_data)
}

# (1.4): Graph take-up rate: basic graph definition.
graph_takeup_rate <- function(data, y_axis_label, labels, current_cols, slinelabels = FALSE) {
  # Get max date.
  max_date <- max(data$date)
  
  # Define line and point size depending on dates.
  if (max_date != "2020-10-06") {
    lsize <- 1.8
    psize <- 0.5
  } else {
    lsize <- 2.4
    psize <- 6.3
  }
  
  # Add deadline line for long term figures
  l1 <- ggplot() + 
    geom_vline(xintercept = ymd("2020-10-06"), linetype = "dotted", size = 1)
  
  # Define labels: single line for panel figures
  labels <- case_when(!slinelabels ~ str_replace(labels, "&", "\n&"), 
                      slinelabels ~ labels)
  
  # Define line type depending on number of treatments
  lines <- c("solid", "dashed")
  if (nrow(current_cols) == 4) {lines <- rep(lines, each = 2)}
  
  # Generate graph
  if (max_date != "2020-10-06") {
    g1 <- l1
  } else {
    g1 <- ggplot()
  }
  g1 <- g1 + 
    # Line and points
    geom_line(data = data, 
              aes(x = date, 
                  y = takeup_rate, 
                  color = current_group,
                  linetype = current_group),
              linewidth = lsize)
  
  # If short term, add shapes.
  if (max_date == "2020-10-06") {
    g1 <- g1 +     
      geom_point(data = data %>% filter(is_y_var_date == 1), 
                 aes(x = date, 
                     y = takeup_rate, 
                     color = current_group,
                     shape = current_group),
                 size = psize)
  }
  # Parameters for all graphs
  g1 <- g1 +
    # Axis labels
    xlab("Day") +
    ylab(y_axis_label) +
    # Axis breaks
    scale_x_date(date_labels = c("1", "2", "3", "4", "5", "6", "7", "8"), 
                 date_breaks = "1 day") + 
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    # Legend
    scale_color_manual(name = NULL, values = current_cols$final_col, labels = labels) +
    scale_shape_manual(name = NULL, values = c(16, 17, 16, 17), labels = labels) + 
    scale_linetype_manual(name = NULL, values = lines, labels = labels) +
    guides(color = guide_legend(nrow = nrow(current_cols) / ifelse(slinelabels, 1, 2),
                                byrow = slinelabels),
           linetype = guide_legend(nrow = nrow(current_cols) / ifelse(slinelabels, 1, 2),
                                   byrow = slinelabels),
           shape = guide_legend(nrow = nrow(current_cols) / ifelse(slinelabels, 1, 2),
                                byrow = slinelabels)) +
    # Other formatting
    set_theme(legend_position = "bottom", legend_text_size = 17) + 
    theme(
          # text = element_text(family = "Times New Roman", size = 17),
          text = element_text(size = 17),
          # axis.text.x = element_text(family = "Times New Roman", hjust = 1, size = 16),
          axis.text.x = element_text(hjust = 1, size = 16),
          axis.title.y = element_text(margin = margin(r = 15)),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.spacing.y = unit(-20, "points"),
          #legend.text = element_text(family = "Times New Roman", margin = margin(t = 5)),
          legend.text = element_text(margin = margin(t = 5)),
          legend.background = element_blank(),
          legend.box.margin = margin(ifelse(max_date == "2010-10-06", -10, -20), 0, 0, 0),
          legend.box.background = element_blank(),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(2.75, "cm"),
          plot.margin = margin(0, 0, 0, 0))
}

# (1.5): String splitting functions for separating group names.
str_d <- function(string, pattern) {str_detect(str_to_lower(string), pattern)}
str_split_lower_left <- function(string) {str_split(string, " &")[[1]][1] %>% str_to_lower()}
str_split_lower_right <- function(string) {str_split(string, "& ")[[1]][2] %>% str_to_lower()}

# (1.6): Graph take-up rate by treatment group.
graph_takeup_rate_pooled <- function(data, y_var = "accepted_offer", grouping, datefilter, slinelabels = TRUE) {
  # Define y-axis label
  y_axis_label <- case_when(y_var == "accepted_offer" ~ "Take-Up",
                            y_var == "open_email" ~ "Email Openings",
                            y_var == "open_email_reminder" ~ "Email and Reminder Openings")
  # Define labels
  label_groups <- case_when(grouping == "rem" ~ c("Reminder", "No reminder"),
                            grouping == "dl" ~ c("Deadline", "No deadline"),
                            grouping == "antrem" ~ c("Announced reminder", "Unannounced reminder"),
                            grouping == "fee" ~ c("2.75% fee", "3% fee"),
                            grouping == "unantrem" ~ c("Unannounced reminder", "No reminder"))
  
  # Define current colors
  current_cols <- tibble(group = label_groups) %>% 
    mutate(group = str_to_lower(group) %>% 
             str_replace(pattern = "unannounced",
                         replacement = "unanticipated") %>% 
             str_replace(pattern = "announced",
                         replacement = "anticipated")) %>%  #### TEMP FIX FOR UNNANOUNCED
    left_join(group_colors, by = "group") %>%
    mutate(final_col = col1,
           group = group %>% 
             str_replace(pattern = "unanticipated",
                         replacement = "unannounced") %>% 
             str_replace(pattern = "anticipated",
                         replacement = "announced"))  #### TEMP FIX FOR UNNANOUNCED
  
  # Rename variable to use and filter dates.
  proc_data <- data %>% 
  rename(current_group = all_of(grouping)) %>% 
        #### TEMP FIX FOR UNNANOUNCED ####
  mutate(current_group = current_group %>% 
           str_replace(pattern = "unanticipated",
                       replacement = "unannounced") %>% 
           str_replace(pattern = "anticipated",
                       replacement = "announced")) %>% 
    #### TEMP FIX FOR UNNANOUNCED ####
    filter(date <= ymd(datefilter)) %>% 
    mutate(current_group = str_replace(current_group, "_", " ") %>% 
             str_remove("fee ") %>% 
             str_to_sentence()) %>% 
    left_join(tibble(full_name = label_groups) %>% 
                mutate(current_group = str_replace(full_name, "% fee", "")), 
              by = "current_group") %>% 
    mutate(current_group = factor(full_name, levels = label_groups))
  
  g1 <- graph_takeup_rate(proc_data, y_axis_label, label_groups, current_cols, slinelabels) 
  
  return(g1)
}

# (1.7): Graph take-up rate, heterogeneities.
graph_takeup_rate_het <- function(data, y_var = "accepted_offer", grouping, datefilter, slinelabels = TRUE) {
  # Define y-axis label
  y_axis_label <- case_when(y_var == "accepted_offer" ~ "Take-Up",
                            y_var == "open_email" ~ "Email Openings",
                            y_var == "open_email_reminder" ~ "Email and Reminder Openings")
  # Define label order.
  proc_data <- data %>% 
    rename(current_group = all_of(grouping)) %>% 
    filter(date <= ymd(datefilter))
  group_level <- tibble(group = unique(proc_data$current_group)) %>%
    mutate(left_group = lapply(unique(proc_data$current_group), str_split_lower_left) %>% unlist(),
           right_group = lapply(unique(proc_data$current_group), str_split_lower_right) %>% unlist(),
           above_level_right = ifelse(str_d(left_group, "above"), 1, 0),
           below_level_right = ifelse(str_d(right_group, "below"), -1, 0),
           no_deadline_level_left = ifelse(str_d(left_group, "no deadline"), -3, 0),
           no_deadline_level_right = ifelse(str_d(right_group, "no deadline"), -1, 0),
           no_reminder_level_left = ifelse(str_d(left_group, "no reminder"), -3, 0),
           no_reminder_level_right = ifelse(str_d(right_group, "no reminder"), -1, 0),
           unantrem_level_left = ifelse(str_d(left_group, "unannounced"), -3, 0),
           unantrem_level_right = ifelse(str_d(right_group, "unannounced"), -1, 0),
           #### Unanticipated reminder adjustment ###
           unantrem_level_left = case_when(str_d(left_group, "unannounced") ~ 3, 
                                           left_group == "announced reminder" ~ 4,
                                           TRUE ~ 0),
           no_reminder_level_left = ifelse(str_d(left_group, "unannounced") &
                                             str_d(left_group, "no_reminder"), -4, 0),
           unantrem_level_right = case_when(str_d(right_group, "unannounced") ~ 1,
                                            right_group == "announced reminder" ~ 2, 
                                            TRUE ~0),
           ##########################################
           #### Expected gain adjustment ###
           expgain_level_left = ifelse((str_d(right_group, "gain")|
                                          str_d(right_group, "offer")|
                                          str_d(right_group, "baseline")) &
                                         str_d(left_group, "no reminder"), -4, 0),
           # expgain_level_right = ifelse(str_d(left_group, "gain"), 1, 0),
           # expgain_level_right = ifelse(str_d(right_group, "below"), -1, 0),
           #################################
           usage_level_right = ifelse(str_d(right_group, "1st"), -3, 0),
           fee_level = ifelse(str_d(group, "75"), 1, 0),
           group_score = above_level_right + below_level_right + no_deadline_level_left + no_deadline_level_right + no_reminder_level_left + 
             no_reminder_level_right + unantrem_level_left + unantrem_level_right + expgain_level_left + usage_level_right + fee_level) %>%
    arrange(desc(group_score))
  label_groups <- group_level$group
  proc_data %<>% mutate(current_group = factor(current_group, levels = label_groups))
  
  # Define current colors
  current_cols <- tibble(group = label_groups)
  current_cols$left_group <- lapply(current_cols$group, str_split_lower_left) %>% unlist()
  current_cols$right_group <- lapply(current_cols$group, str_split_lower_right) %>% unlist()
  current_cols %<>%
    left_join(group_colors %>% 
                rename(left_group = group) %>% 
                mutate(left_group = str_replace_all(left_group,
                                                    pattern = "unanticipated",
                                                    replacement = "unannounced"),
                       left_group = str_replace_all(left_group,
                                                    pattern = "anticipated",
                                                    replacement = "announced")),
              by = "left_group") %>% 
    mutate(final_col = ifelse(str_detect(str_to_lower(right_group), "below|3%|no reminder|unannounced reminder|1st"), col2, col1))
  
  g1 <- graph_takeup_rate(proc_data, y_axis_label, label_groups, current_cols, slinelabels) 
  
  return(g1)
}

# (1.8): Modify date scale and add line.
change_scale <- function(graph, breaks, y_position, size) {
  output <- graph +
    scale_x_date(limits = ymd(c("2020-09-29", "2021-03-31")), name = "", date_labels = "%b", breaks = breaks) + 
    # annotate(geom = "text", x = ymd("2020-10-11"), y = y_position, size = size,
             # label = "Deadline to accept \n lower fee")  +
    # theme(axis.text.x = element_text(family = "Times New Roman", size = 16),
    #       axis.text.y = element_text(family = "Times New Roman", size = 16)) +
    labs(x = NULL)
  suppressWarnings(output)
}

# (1.9): Graph treatment effect: basic graph definition.
graph_treat_effect <- function(data, current_cols) {
  # Define line and point size    
  lsize <- 2.4
  psize <- 6.3
  # Parameters for all graphs
  ggplot(data, aes(x = date, y = coef, color = term)) +
    # Line and points
    geom_hline(yintercept = 0) + 
    geom_point(position = position_dodge(width = 0.5), size = psize) +  #color = current_cols, 
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, size = lsize, position = position_dodge(width = 0.5)) + #color = current_cols, 
    # Axis labels
    xlab("Day") +
    ylab("Treatment Effect (pp)") +
    # Axis breaks
    scale_x_date(name = "Day", date_breaks = "day" , date_labels = paste(seq(0, 8, 1))) + 
    scale_y_continuous(labels = label_number(accuracy = 1, scale = 100)) +
    # Other formatting
    scale_color_manual(name = "", values = current_cols) + 
    set_theme(legend_position = "bottom", size = 17, legend_text_size = 17) +
    theme(
          #text = element_text(family = "Times New Roman", size = 14),
          #axis.text.x = element_text(family = "Times New Roman", hjust = 1, size = 16),
          axis.text.x = element_text(hjust = 1, size = 16),
          axis.title.y = element_text(margin = margin(r = 15)),
          legend.title = element_blank(),
          legend.spacing.x = unit(0.3, "cm"), 
          legend.spacing.y = unit(-20, "points"),
          legend.background = element_blank(),
          legend.box.margin = margin(-10, 0, 0, 0),
          legend.box.background = element_blank(),
          # legend.text = element_text(family = "Times New Roman", margin = margin(t = 5)),
          legend.text = element_text(margin = margin(t = 5)),
          legend.key = element_rect(fill = NA, color = NA),
          legend.key.size = unit(2.75, "cm"),
          legend.key.height = unit(20, "points"),
          plot.margin = margin(5, 0, 0, 0))
}

# (1.10): Graph treatment effect by treatment group.
graph_treat_effect_pooled <- function(reg, treat_group) {
  # Get coefficients and confidence intervals.
  dates <- c("Sep 29", "Sep 30", "Oct 1", "Oct 2", "Oct 3", "Oct 4", "Oct 5", "Oct 6")
  treat_var <- case_when(treat_group == "rem" ~ "reminder",
                         treat_group == "dl" ~ "deadline",
                         treat_group == "antrem" ~ "anticipated_reminder",
                         treat_group == "fee" ~ "fee_2.75",
                         treat_group == "unantrem" ~ "unanticipated_reminder")
  current_graph_data <- lapply(dates, get_coef_cis, model = reg, var_comb = treat_var) %>% 
    bind_rows() %>% 
    mutate(term = treat_group)
  
  # Define current colors
  shortnames <- tribble(~group, ~shortname, 
                        "reminder", "rem", 
                        "anticipated reminder", "antrem", 
                        "deadline", "dl", 
                        "2.75% fee", "fee",
                        "unanticipated reminder", "unantrem")
  current_cols <- group_colors %>% 
    left_join(shortnames, by = "group") %>% 
    filter(shortname == treat_group) %>%
    pull(col1)
  
  # Generate graph
  current_graph <- graph_treat_effect(current_graph_data, current_cols) +
    theme(legend.position = "none")
  
  return(current_graph)
}

# (1.11): Graph treatment effect, heterogeneities.
graph_treat_effect_het <- function(reg, treat_group, interaction, negative_term, positive_term) {
  # Get coefficients and confidence intervals.
  dates <- c("Sep 29", "Sep 30", "Oct 1", "Oct 2", "Oct 3", "Oct 4", "Oct 5", "Oct 6")
  current_graph_data <- bind_rows(
    lapply(dates, get_coef_cis, model = reg, var_comb = paste(treat_group, "+", interaction)) %>% 
      bind_rows() %>% 
      mutate(term = positive_term),
    lapply(dates, get_coef_cis, model = reg, var_comb = paste(treat_group)) %>% 
      bind_rows() %>% 
      mutate(term = negative_term)
  )
  
  # Define current colors
  current_cols <- tibble(group = treat_group %>% 
                           str_replace("_", " ") %>% 
                           str_to_lower()) %>% 
    mutate(group = ifelse(str_detect(group, "usage"), "anticipated reminder", group)) %>% 
    left_join(group_colors, by = "group") %>% 
    pivot_longer(cols = c("col1", "col2"), values_to = "color") %>% 
    pull(color)
  
  # Generate graph
  current_graph <- graph_treat_effect(current_graph_data, current_cols) +
    guides(color = guide_legend(nrow = 2), byrow =  T)
  
  return(current_graph)
}

# (1.12): Adjust graph size and legend.
adjust_size <- function(graph, text_size = 28, legend_size = 28, type = "pooled", panel_graphs = FALSE) {
  if (type %in% c("fsize")) {legend_size <- 27} #"expgain",
  # if (type %in% c("dltreat", "dlcond")) {legend_size <- 23.4}
  output_graph <- graph +
    theme(
          axis.title.x = element_text(size = text_size),
          axis.title.y = element_text(size = text_size),
          axis.text.x = element_text(size = text_size),
          axis.text.y = element_text(size = text_size),
          legend.text = element_text(size = legend_size),
          legend.box.margin = margin(-15, 10, 0, 10))
  if (panel_graphs) {
    output_graph <- output_graph +
      theme(legend.justification = "left",
            legend.margin = margin(l = -60))
  }
  return(output_graph)
}

# (1.13): Define function for getting coefficient and confidence intervals from combinations of variables.
get_coef_cis <- function(model, date, var_comb) {
  model_proc <- model[[date]]
  raw <- model_proc %>% 
    glht(str_c(var_comb, "= 0")) %>% 
    confint()
  raw <- raw[["confint"]]
  output <- tribble(~date, ~coef, ~lci, ~uci, 
                    mdy(paste(date, 2020)), raw[1], raw[2], raw[3])
  return(output)
}

# (1.14): Calculate differences in takeup.
takeup_dif <- function(df, group_name) {
  proc_df <- df %>% 
    rename(grouping = all_of(group_name)) %>% 
    filter(date == "2021-03-17") %>% 
    select(grouping, takeup_rate) %>% 
    arrange(desc(takeup_rate))
  paste("The difference in take-up between the", proc_df$grouping[1], "group and the",
        proc_df$grouping[2], "group is", 100 * round(proc_df$takeup_rate[1] - proc_df$takeup_rate[2], 4),
        "percentage points")
}

# (1.15): Define y-axis breaks.
# Function that adds 1 to a number until it's divisible by 3 or 4
add_until_divisible <- function(num) {
  num_scale <- num * 100
  while (num_scale %% 3 != 0 && num_scale %% 4 != 0) {
    num_scale <- num_scale + 1
  }
  return(num_scale/100)
}
# Function that checks whether a number is divisible by 3 or 4
check_divisibility <- function(num) {
  num_scale <- num * 100
  if (near(num_scale %% 3, 0)) {
    return(3)
  } else {
    return(4)
  }
}
# Function to define axis breaks
define_y_axis_breaks <- function(ll, lu) {
  # Get rounded lower and upper limits
  ll_round <- round(ll, 2)
  lu_round <- round(lu, 2)
  
  # Calculate distance between upper and lower limits
  dist_raw <- lu - ll
  dist_round <- round(lu_round - ll_round, 2)
  
  # Define updated distance
  dist_divisible <- add_until_divisible(dist_round)
  div <- check_divisibility(dist_divisible)
  
  # Check additional distance from rounded distance
  dist_add <- dist_divisible - dist_round
  
  # If only 1, add to lower limit, if 2, add to both
  if (near(dist_add, .02)) {
    ll_final <- ll_round - .01
    lu_final <- lu_round + .01
  } else if (near(dist_add, .01)) {
    ll_final <- ll_round - .01
    lu_final <- lu_round
  } else {
    ll_final <- ll_round
    lu_final <- ll_final + dist_divisible
  }
  
  # Define final sequence of breaks
  final_breaks <- seq(ll_final, lu_final, (lu_final - ll_final)/div)
  
  return(final_breaks)
}
define_y_axis_breaks_beta <- function(ll, lu) {
  # Calculate the range and determine the tick spacing
  range <- lu - ll
  tick_spacing <- ifelse(range <= 0.12, 0.02, 0.04)
  
  # Calculate the number of ticks needed
  num_ticks <- min(6, ceiling(range / tick_spacing))
  
  # Calculate the tick values
  tick_values <- seq(floor(ll / tick_spacing) * tick_spacing,
                     ceiling(lu / tick_spacing) * tick_spacing,
                     by = tick_spacing)
  
  # Include the closest tick to the lower or upper limit
  dist_ll <- ll - min(tick_values)
  dist_lu <- lu - max(tick_values)
  if (dist_ll < 0 & # If the lower limit is below the minimum tick value 
                    # And the distance between an additional tick is smaller than the current distance
      abs((min(tick_values) - tick_spacing - ll)) < abs(dist_ll)) {
    tick_values %<>% append(min(tick_values) - tick_spacing)
  } 
  if (dist_lu > 0 & # If the upper limit is above the maximum tick value
                    # And the distance between an additional tick is smaller than the current distance
      abs((max(tick_values) + tick_spacing - lu)) < abs(dist_lu)) {
    tick_values %<>% append(max(tick_values) + tick_spacing)
  }

  return(tick_values)
}
