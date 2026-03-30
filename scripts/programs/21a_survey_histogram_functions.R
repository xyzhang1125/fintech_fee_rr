#--------------------------------------------------------------------------------------------
# File name: 		      21a_survey_histogram_functions.R
# Creation date:      2023-05-18
# Author:          		César Landín
# Files used:
# 	- (None)
# Files created:
# 	- (None)
# Purpose:
# 	- Define functions for survey histograms.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
#########################################################


#############################################
##  (1): Define data processing functions.  #
#############################################
# (1.1): Keep valid answers (drop other, don't know, refuse to answer).
keep_valid_ans <- function(df, var) {
  df %>% 
    filter(!is.na(eval(as.name(var))) &
             !(eval(as.name(var))) %in% c(-666, -777, -888))
}

# (1.2): Define function to calculate and save stats.
calc_stats <- function(df, var_name, var_label = "", dig = 2) {
  # Mean
  df %>%
    num_mean(var_name, dig) %>%
    print_n(str_c("survey_", var_name, "_mean.tex"),
            note = ifelse(var_label == "", "", paste("Mean", var_label)))
  # Median
  df %>%
    num_median(var_name, dig) %>%
    print_n(str_c("survey_", var_name, "_median.tex"),
            note = ifelse(var_label == "", "", paste("Median", var_label)))
  # SD
  df %>%
    num_sd(var_name, dig) %>%
    print_n(str_c("survey_", var_name, "_sd.tex"),
            note = ifelse(var_label == "", "", paste("SD of", var_label)))
}

# (1.3): Define function for processing graph data for binned histograms.
proc_hist_graph_data <- function(df, var, min, max, diff, direction = NULL) {
  if (is.null(direction)) {
    direction <- ifelse(max >= abs(min), "right", "left")
  }
  if (direction == "right") {
    right_closed <- FALSE
    is_nan_replace <- (max + diff / 2)
  } else {
    right_closed <- TRUE
    is_nan_replace <- (min - diff / 2)
  }
  
  if (!exists("accepted_offer_late", where = df)) {
    proc_df <- df %>% mutate(accepted_offer_late = 1)
  } else {
    proc_df <- df
  }

  proc_df %<>%
    filter(!is.na(eval(as.name(var)))) %>%
    mutate(accepted = ifelse(accepted_offer_late == 1, "Accepted offer", "Did not accept offer") %>% as.factor())

  proc_df %>%
    select(accepted, all_of(var)) %>%
    group_by(accepted) %>%
    mutate(bins = cut(eval(as.name(var)), breaks = seq(min, max, by = diff), include.lowest = TRUE, right = right_closed, dig.lab = 5)) %>%
    ungroup() %>%
    complete(bins, accepted) %>%
    mutate(current_var = as.numeric(!is.na(eval(as.name(var))))) %>%
    select(-all_of(var)) %>%
    group_by(bins, accepted) %>%
    summarise(count = sum(current_var)) %>%
    ungroup() %>%
    group_by(accepted) %>%
    mutate(obs_group = sum(count)) %>%
    ungroup() %>%
    mutate(bin_def = bins,
           bins = str_remove_all(bins, "[\\[\\]()]") %>%
             str_split(",") %>%
             map_dbl(~ mean(as.numeric(.), na.rm = TRUE)),
           bins = ifelse(is.nan(bins), is_nan_replace, bins),
           pct = count / obs_group) %>%
    ungroup() %>%
    arrange(accepted, bins)
}

######################################
##  (2): Define graphing functions.  #
######################################
# (2.1): Bin number function.
binwidth <- function(data, variable) {
  max <- data %>% pull(eval(as.name(variable))) %>% max(na.rm = TRUE)
  min <- data %>% pull(eval(as.name(variable))) %>% min(na.rm = TRUE)
  range <- max - min + 1
  if (range - floor(range) != 0) {div <- 0.25} else {div <- 1}
  bw <- div
  return(bw)
}

# (2.2): Histogram function.
graph_hist_simple <- function(data, variable, xlab, bin_width = data %>% binwidth(variable), center = 0, breaks = NULL, caption = "") {
  if (!is.null(breaks)) {
    g1 <- data %>% 
      ggplot() +
      geom_histogram(aes(x = eval(as.name(variable)),
                         y = after_stat(count) / nrow(data)), 
                     fill = "lightblue", color = "white", 
                     center = center, binwidth = bin_width,
                     breaks = breaks)
  } else {
    g1 <- data %>% 
      ggplot() +
      geom_histogram(aes(x = eval(as.name(variable)),
                         y = after_stat(count) / nrow(data)), 
                     fill = "lightblue", color = "white", 
                     center = center, binwidth = bin_width)
  }
  g1 <- g1 + 
    geom_hline(yintercept = 0, linewidth = 0.425) +
    # Scale and design
    set_theme() +
    theme(plot.caption = element_text(size = 7),
          plot.margin = margin(0, 20, -10, 3),
          axis.text.x = element_text(margin = margin(t = -5, r = 20))) +
          # axis.text.y = element_text(margin = margin(r = -5))) +
    scale_x_continuous(expand = expansion(mult = 0.01)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) + 
    # Labels
    ylab("Percentage of Firms") +
    xlab(xlab) +
    labs(caption = caption)
  g1
}

# (2.3): Define graph function.
graph_hist_tail <- function(df, xlab, break_dist = 2, unique = FALSE) {
  # Define bin distances
  binlist <- unique(df$bins) %>% sort()
  calc_break <- binlist[2] - binlist[1]
  min_bin <- binlist[1]
  max_bin <- tail(binlist, 2)[1]
  # max_bin_label <- str_c(comma(max_bin + calc_break/2, accuracy = ), "+")
  # Define breaks and labels
  # if (calc_break > 1) {
  # current_breaks <- seq(min_bin - calc_break/2, max_bin + calc_break/2, (2*calc_break))
  current_breaks <- c(head(seq(min_bin - calc_break/2, max_bin + calc_break/2, (break_dist * calc_break)), -1), max_bin + calc_break)
  # current_labels <- c(head(seq(min_bin - calc_break/2, max_bin + calc_break/2, (2*calc_break)), -1), max_bin_label)
  current_labels <- comma(seq(min_bin - calc_break/2, max_bin + calc_break/2, (break_dist * calc_break)))
  current_labels[length(current_labels)] <- str_c(current_labels[length(current_labels)], "+")
  # } else {
  #   current_breaks <- seq(0, 1, 0.2)
  #   # current_labels <- c(percent(seq(0, 0.8, 0.2)), "100%+")
  #   current_labels <- (seq(0, 1, 0.2)) #percent
  # }
  # Generate plot
  # Get levels of "accepted" (in case we want to use other labels)
  acc_levels <- df %>% pull(accepted) %>% levels() %>% as.character()
  current_plot <- ggplot() +
    geom_col(data = subset(df, accepted == acc_levels[2]),
             aes(x = bins, y = pct, fill = accepted, alpha = accepted),
             position = position_identity()) +
    geom_col(data = subset(df, accepted == acc_levels[1]),
             aes(x = bins, y = pct, fill = accepted, alpha = accepted),
             position = position_identity()) +
    geom_hline(yintercept = 0, linewidth = 0.425) +
    set_theme() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    ylab("Percentage of Firms") + 
    xlab(xlab) +
    scale_fill_manual(name = "", breaks = c(acc_levels[1], acc_levels[2]), values = c("lightblue", "gray50")) +
    scale_alpha_manual(name = "", breaks = c(acc_levels[1], acc_levels[2]), values = c(0.7, 1)) +
    scale_x_continuous(breaks = current_breaks,
                       labels = current_labels,
                       expand = expansion(mult = 0.01)) +
    theme(legend.position = c(0.75, 0.85),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(margin = margin(r = 30)),
          axis.text.x = element_text(margin = margin(t = -10)),
          axis.text.y = element_text(margin = margin(r = 0)),
          plot.margin = margin(l = 2, b = 1, r = 10))
  
  if (unique) {
    current_plot <- current_plot +
      scale_fill_manual(name = "", breaks = acc_levels[1], values = "lightblue") +
      scale_alpha_manual(name = "", breaks = acc_levels[1], values = 1) +
      theme(legend.position = "none")
  }
  return(current_plot)
}

# graph_hist <- function(df, xlab) {
#   # max_bin_label
#   # Define bin distances
#   binlist <- unique(df$bins) %>% sort()
#   calc_break <- binlist[2] - binlist[1]
#   min_bin <- binlist[1]
#   max_bin <- tail(binlist, 2)[1]
#   # Define breaks and labels
#   if (calc_break > 1) {
#     # current_breaks <- seq(min_bin - calc_break/2, max_bin + calc_break/2, (2*calc_break))
#     current_breaks <- c(head(seq(min_bin - calc_break/2, max_bin + calc_break/2, (2*calc_break)), -1), max_bin + calc_break/2)
#     # current_labels <- c(head(seq(min_bin - calc_break/2, max_bin + calc_break/2, (2*calc_break)), -1), max_bin_label)
#     current_labels <- comma(seq(min_bin - calc_break/2, max_bin + calc_break/2, (2*calc_break)))
#   } else {
#     current_breaks <- seq(0, 1, 0.2)
#     # current_labels <- c(percent(seq(0, 0.8, 0.2)), "100%+")
#     current_labels <- (seq(0, 1, 0.2)) #percent
#   }
#   # Generate plot
#   ggplot() +
#     geom_col(data = subset(df, accepted == "Did not accept offer"),
#              aes(x = bins, y = pct, fill = accepted, alpha = accepted),
#              position = position_identity()) +
#     geom_col(data = subset(df, accepted == "Accepted offer"),
#              aes(x = bins, y = pct, fill = accepted, alpha = accepted),
#              position = position_identity()) +
#     geom_hline(yintercept = 0, linewidth = 0.425) +
#     set_theme() +
#     scale_y_continuous(labels = percent_format(accuracy = 1)) +
#     ylab("Percentage of Firms") + 
#     xlab(xlab) +
#     scale_fill_manual(name = "", breaks = c("Accepted offer", "Did not accept offer"), values = c("lightblue", "gray50")) +
#     scale_alpha_manual(name = "", values = c("Accepted offer" = 0.7, "Did not accept offer" = 1)) +
#     scale_x_continuous(breaks = current_breaks,
#                        labels = current_labels,
#                        expand = expansion(mult = 0.01)) +
#     theme(legend.position = c(0.75, 0.85),
#           legend.title = element_blank(),
#           legend.key = element_blank(),
#           legend.text = element_text(margin = margin(r = 30)),
#           axis.text.x = element_text(margin = margin(t = -10)),
#           axis.text.y = element_text(margin = margin(r = 0)))
# }
