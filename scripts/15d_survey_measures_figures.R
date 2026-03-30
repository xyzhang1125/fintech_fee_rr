#--------------------------------------------------------------------------------------------
# File name: 		      15d_survey_measures_figures.R
# Creation date:      2023-02-16
# Author:          		César Landín
# Files used:
#   - here("proc", "survey_successful.csv")
#   - here("proc", "het_survey_measures_unantrem_late.csv")
#   - here("proc", "het_survey_measures_dl_late.csv")
#   - here("proc", "het_survey_measures_antrem_late.csv")
#   - here("results", "tables", "romano_p_antrem_unantrem.tex")
# Files created:
#   - here("results", "figures", "het_survey_measures_antrem_late_romano.eps")
# Purpose:
# 	- Figure 10: Heterogeneous Effects of Announced Reminder by GSS Measures.
#   - Figure C.16: Heterogeneous Effect of Unannounced Reminder by Survey Measures.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(tabulator)
library(cowplot)
library(scales)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # change to times.ttf for running on the server
showtext_auto()
options(readr.show_col_types = FALSE)
group_colors <- read_csv(here("proc", "colors.csv"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

####################################################################################################
##    (1): Process data and define functions for section 12 bar graphs and corresponding tables.   #
####################################################################################################
# Now, I will mention seven statements that you may agree or disagree with. 
# Using the following scale from 1 to 5, indicate your agreement with each one selecting the appropriate number from this scale. 
# Please answer these questions openly and sincerely. 
# 5 – completely agree
# 4 – agree
# 3 – neither agree nor disagree
# 2 – disagree
# 1 – completely disagree
# 12.11.	[Trust] I trust advertised offers. Tell me your answer from the following scale:
# 12.12.	[Trust] I believe that good advertised offers are too good to be true. Tell me your answer from the following scale:
# 12.13.	[Reciprocity] I am more inclined to do business with people who live up to their promises. Tell me your answer from the following scale:
# 12.14.	[Procrastination] I tend to postpone tasks, even when I know it is better to do them immediately. Tell me your answer from the following scale:
# 12.15.	[Memory] I tend to have good memory about pending tasks that I have to do and complete. Tell me your answer from the following scale:
# 12.16.	[Memory] I tend to think my memory is better than it really is. Tell me your answer from the following scale:
# 12.17.	[Attention] I can focus completely when I have to finish a task. Tell me your answer from the following scale:

# Filter for section 12: all firms

# (1.1): Import why deadline and why reminder figures.
data_survey_measures <- read_csv(here("proc", "survey_successful.csv")) %>% 
  select(organization_uuid, contains("binary"), req_ans_q12.11,
         reminder, deadline, anticipated_reminder, unanticipated_reminder)

# (1.2): Define spacing function.
spacing <- function(group_size, group_num, sep_svy, sep_group = NA, sep_between = 1) {
  if (!is.na(sep_group)) {
    pair <- c(1, sep_between)
    base_group <- c(pair, (pair + sep_group + 2))
  } else {
    base_group <- seq(1, group_size * sep_between, sep_between)
  }
  group_dist <- max(base_group) + sep_svy - 1
  seq <- c()
  for (i in 1:group_num) {
    if (is_empty(seq)) {
      seq <- base_group
    } else {
      group_dist <- max(seq) + sep_svy - 1
      seq <- c(seq, base_group + group_dist)
    }
  }
  return(seq)
}

# (1.3): Define significance stars function.
sig_stars_simple <- function(data) {
  output <- tibble()
  for (svy in survey_measures_varnames) {
    filter_data <- data %>% filter(group == "Treat_only" & survey_measure == svy)
    z <- filter_data$coef / filter_data$se
    stars <- case_when(abs(z) >= 2.576 ~ "***",
                       abs(z) >= 1.960 ~ "**",
                       abs(z) >= 1.645 ~ "*",
                       abs(z) < 1.645 ~ "")
    output %<>%
      bind_rows(tribble(~survey_measure, ~z_score, ~stars,
                        svy, abs(z), stars))
  }
  return(output)
}

##################################
##    (2): Generate bar graphs.  #
##################################
# (2.1): Define variable names.
survey_measures_fullnames <- c("Trust", "Reciprocity", "Procrastination", "Memory", "Overconfidence", "Attention")
survey_measures_varnames <- c("trust_scale_1_binary", "reciprocity_scale_binary", "procrastination_scale_binary", "memory_scale_1_binary",  "memory_scale_2_binary", "attention_scale_binary")

# (2.2): Define graph and regression parameters.
regression_parameters <- tribble(
  ~subgroup, ~label, ~sub_title, ~group_treat, ~group_control,
  "unanticipated_reminder", "unantrem", "Unannounced Reminder Effect", "Unannounced Reminder", "No Reminder",
  "deadline", "dl", "Deadline Effect", "Deadline", "No Deadline",
  "anticipated_reminder", "antrem", "Announced Reminder Effect", "Announced Reminder", "Unannounced Reminder") %>% 
  as.data.frame()

# (2.3): Define footnote.
survey_footnote <- paste("The survey question asked respondents whether they agreed or disagreed with the following six statements:",
                         "(1) \\emph{Trust}: I trust advertised offers.",
                         "(2) \\emph{Reciprocity}: I am more inclined to do business with people who live up to their promises.",
                         "(3) \\emph{Procrastination}: I tend to postpone tasks, even when I know it is better to do them immediately.",
                         "(4) \\emph{Memory}: I tend to have good memory about pending tasks that I have to do and complete.",
                         "(5) \\emph{Overconfidence}: I tend to think my memory is better than it really is.",
                         "(6) \\emph{Attention}: I can focus completely when I have to finish a task.",
                         "The scale of these responses is 1 to 5, where 5 is highest level of agreement",
                         "and 1 highest level of disagreement.",
                         "Binary measure variables were created from these responses,",
                         "coding 4 and 5 (agree and completely agree) as ``High''  and 1-3 (completely disagree, disagree",
                         "and neither agree nor disagree) as ``Low''.")


### Romanowolf stars in the figure (late):
text <- character()
counter <- 1
for (r in 1:nrow(regression_parameters)) {
  if (r==3) {
  # Define current colors
  current_cols <- group_colors %>% 
    mutate(group = str_replace_all(group, "anticipated", "announced")) %>%  #### TEMP ANNOUNCED FIX
    filter(group %in% c(str_to_lower(regression_parameters$group_treat[r]), str_to_lower(regression_parameters$group_control[r]))) %>% 
    pivot_longer(cols = col1:col2) %>% 
    mutate(order = 1:4) %>% 
    filter(order %in% c(1, 4)) %>% 
    pull(value)
  cb_pal_top <- rep(current_cols, 6)
  cb_pal_four_col <- rep(current_cols, 6, each = 2)
  cb_pal_four_fill <- rep(c("#FFFFFF", current_cols[1], "#FFFFFF", current_cols[2]), 6)
  
  for (wt in c("")) {
    # Define treatment and control group for legends.
    group_treat <- regression_parameters$group_treat[r]
    group_control <- regression_parameters$group_control[r]
    # Define max takeup and max/min effect for limits.
    all_data <- here("proc", str_c("het_survey_measures_", regression_parameters$label[r], "_", c("late"), wt, ".csv")) %>%  #"firstday", "ontime", 
      lapply(read_csv, col_types = cols()) %>% 
      bind_rows() %>% 
      mutate(gen_group = rep(str_c(c("late"), wt), each = 24)) #"firstday", "ontime", 
    max_takeup <- max(all_data$coef)
    for (t in c("late")) { #"firstday", "ontime", 
      print(paste("Working on", ifelse(wt == "_wt", "weighted", "unweighted"), t, regression_parameters$label[r], "graphs"))
      # Import coefficients and standard errors from regressions of take-up on survey measures.
      current_data <- read_csv(here("proc", str_c("het_survey_measures_", regression_parameters$label[r], "_", t, wt, ".csv")), col_types = cols())
      # Process data.
      treatment_graph_data <- current_data %>% 
        filter(group != "control") %>% 
        left_join(current_data %>% 
                    filter(group == "control") %>% 
                    select(svy_measure, svy_status, coef) %>% 
                    rename(coef_control = coef),
                  by = c("svy_measure", "svy_status")) %>% 
        mutate(coef = coef - coef_control,
               lci = ifelse(group == "treatment", coef - 1.96 * se, NA),
               uci = ifelse(group == "treatment", coef + 1.96 * se, NA),
               bar_group = rep(c("Low", "High"), 6),
               bar_group = factor(bar_group, levels = unique(bar_group)))      
      current_data %<>%
        mutate(lci = ifelse(group == "treatment", coef - 1.96 * se, NA),
               uci = ifelse(group == "treatment", coef + 1.96 * se, NA),
               bar_group = rep(c("Low survey measure, untreated", "Low survey measure, treated", "High survey measure, untreated", "High survey measure, treated"), 6),
               bar_group = factor(bar_group, levels = unique(bar_group)))
      # Define significance stars
      sig_stars <- tibble()
      for (svy in survey_measures_varnames) {
        filter_data <- current_data %>% filter(group == "treatment" & svy_measure == svy)
        beta1 <- filter_data %>% filter(svy_status == 1) %>% pull(treat_coef)
        sebeta1 <- filter_data %>% filter(svy_status == 1) %>% pull(se_or)
        z <- beta1 / sebeta1
        stars <- case_when(abs(z) >= 2.576 ~ "***", #"|- *** -|",
                           abs(z) >= 1.960 ~ "**",
                           abs(z) >= 1.645 ~ "*",
                           abs(z) < 1.645 ~ "")
        sig_stars %<>%
          bind_rows(
            tribble(~survey_measure, ~z_score, ~stars,
                    svy, abs(z), stars)
          )
      }
      antrem_unantrem_romano <- readLines(here("results", "tables", "romano_p_antrem_unantrem.tex"))
      rw_pvalues <- str_trim(antrem_unantrem_romano [6:11]) %>% 
        str_extract_all("\\d*\\.\\d+") %>% 
        sapply(`[`, 3) %>%  # Extract the third match from each row
        as.numeric()
      # Define the function to add stars
      add_crosses <- function(p) {
        if (p <= 0.01) return("+++")  # Smaller plus signs
        if (p <= 0.05) return("++")
        if (p <= 0.1) return("+")
        return("")
      }
      if (nrow(sig_stars) == length(rw_pvalues)) {
        sig_stars <- sig_stars %>%
          mutate(
            rw_pvalues = rw_pvalues,             # Add Romano-Wolf p-values
            r_crosses = sapply(rw_pvalues, add_crosses) # Assign crosses based on p-values
          )
      }
      # Recenter graph on half of actual range.
      max_treat <- max(treatment_graph_data$uci)
      min_treat <- min(treatment_graph_data$lci)
      range_upper <- max_treat + 0.2 - min_treat
      recenter_upper <- (max_treat - min_treat) * 0.5 / range_upper
      lp <- max_takeup / 0.4
      lp_alt <- lp * 5 / 3
      max_axis <- max(seq(0, ceiling(max_takeup * 10) / 10, 0.2)[seq(0, ceiling(max_takeup * 10) / 10, 0.2) <= max_takeup + 0.05])
      range_lower <- max_axis + 0.1 * lp_alt
      recenter_lower <-(range_lower - max_axis / 2) / range_lower
      up <- (range_upper - 0.2) / 0.75
      # Define spacing parameters.
      expand_param <- 0.1
      line_bar_width <- 0.8
      spacing_12 <- spacing(group_size = 2, group_num = 6, sep_svy = 3, sep_between = 2.15) + 0.5 
      spacing_24 <- spacing(group_size = 4, group_num = 6, sep_svy = 2, sep_group = 0.15, sep_between = 2)
      min_break <- seq(-1, 1, 0.2)[seq(-1, 1, 0.2) >= (min_treat) & seq(-1, 1, 0.2) <= (max_treat + up * 0.22)]
      # Generate graph.
      g1 <- ggplot(treatment_graph_data,
                   aes(x = spacing_12, y = coef, color = bar_group)) +
        geom_hline(yintercept = 0, linewidth = 0.5) + 
        geom_point(stat = "identity", size = 3) +
        geom_errorbar(aes(ymax = uci, ymin = lci), linewidth = line_bar_width, width = 0.5) +
        # Scale and design
        set_theme(size = 16) +
        scale_color_manual(name = "", values = cb_pal_top) +
        scale_x_continuous(limits = c(-expand_param, max(spacing_24) + line_bar_width + expand_param), 
                           expand = c(0, 0)) +
        # scale_y_continuous(limits = c(min_treat, max_treat + up * 0.25), 
        #                    breaks = seq(-1, 1, 0.2)[seq(-1, 1, 0.2) >= (min_treat - 0.05) & seq(-1, 1, 0.2) <= (max_treat + 0.05)],
        #                    labels = label_comma(accuracy = 0.1)) +
        # Labels
        #ylim(-1, 1) +
        scale_y_continuous(
          breaks = seq(-0.8, 1.0, by = 0.2),
          labels = label_number(accuracy = 1, scale = 100)
        ) +
        ylab("Treatment Effect (pp)") +
        annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = 0.70, label = rep(" ", 6), size = 5) +
        annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = 0.65, label = sig_stars$stars, size = 9) +
        annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = 0.55, label = sig_stars$r_crosses, size = 6) +
        #annotate("text", x = spacing_12 + expand_param, 
        #         label = rep(c("Low", "High"), 6), size = 5.5, y = max_treat + up * 0.15) +
        #annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = max_treat + up * 0.25, label = survey_measures_fullnames, size = 5.5) +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "none",
              axis.title.y = element_text(hjust = 0, margin = margin(t = 0, r = 10, b = 0, l = 0)),
              #plot.margin = margin(t = 40),
              text = element_text(family = "Times New Roman")) 
     
      g2 <- ggplot(current_data, aes(x = spacing_24, y = coef, color = bar_group, fill = bar_group)) +
        geom_bar(size = 1, stat = "identity", width = line_bar_width) +
        geom_hline(yintercept = -0.005, linewidth = 0.5) + 
        # Scale and design
        set_theme(size = 16) +
        scale_fill_manual(name = "", values = cb_pal_four_fill) +
        scale_color_manual(name = "", values = cb_pal_four_col) + 
        scale_x_continuous(limits = c(-expand_param, max(spacing_24) + line_bar_width + expand_param), expand = c(0, 0)) +
        # scale_y_continuous(limits = c(-0.1 * lp_alt, max_takeup), 
        #                    breaks = seq(0, ceiling(max_takeup * 10) / 10, 0.2), 
        #                    labels = label_comma(accuracy = 0.1)) +
        # Labels
        ylab("Take-Up") +
        #annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = max_treat + up * 0.01, label = rep(" ", 6), size = 5) +
        #annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = max_treat + up * 0.05, label = sig_stars$stars, size = 9) +
        annotate("text", x = spacing_12 + expand_param,
                 label = rep(c("Low", "High"), 6), size = 5.5, y = 0.9, 
                 family = "Times New Roman") +
        annotate("text", x = spacing(group_size = 1, group_num = 6, sep_svy = 5.15) + 1.5 + expand_param, y = 1.0, label = survey_measures_fullnames, size = 5.5, 
                 family = "Times New Roman") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "none",
              axis.title.y = element_text(hjust = 0.2, margin = margin(t = 0, r = 10, b = 0, l = 0)),
              text = element_text(family = "Times New Roman")) + #element_text(hjust = 0.9, )) +
        guides(fill = guide_legend(nrow=2,byrow=TRUE)) +
        ylim(0, 1) +
        scale_y_continuous(
          breaks = seq(0, 0.8, by = 0.2),
          labels = percent_format(accuracy = 1)
        ) +
        # Bottom rectangle (only fill)
        geom_rect(aes(xmin = 5.75, xmax = 6.00, ymin = 0.05 * lp_alt + 1.2, ymax = 0.025 * lp_alt + 1.2), color = NA, fill = current_cols[1], size = 1) +
        geom_rect(aes(xmin = 6.00, xmax = 6.25, ymin = 0.05 * lp_alt + 1.2, ymax = 0.025 * lp_alt + 1.2), color = NA, fill = current_cols[2], size = 1) +
        geom_rect(aes(xmin = 5.8515, xmax = 6.1485, ymin = 0.044025 * lp_alt + 1.2, ymax = 0.030075 * lp_alt + 1.2), color = NA, fill = "white", size = 1) +
        geom_rect(aes(xmin = 5.75, xmax = 6.00, ymin = 0.075 * lp_alt + 1.2, ymax = 0.1 * lp_alt + 1.2), color = NA, fill = current_cols[1], size = 1) +
        geom_rect(aes(xmin = 6.00, xmax = 6.25, ymin = 0.075 * lp_alt + 1.2, ymax = 0.1 * lp_alt + 1.2), color = NA, fill = current_cols[2], size = 1) +
        annotate("text", x = 6.75, y = 0.0375 * lp_alt + 1.2, label = group_control, size = 5.5, hjust = 0, family = "Times New Roman") +
        annotate("text", x = 6.75, y = 0.0875 * lp_alt + 1.2, label = group_treat, size = 5.5, hjust = 0, family = "Times New Roman") +
        
        # # Second set of legends (Low and High GSS Measure)
        # geom_segment(aes(x = 17.25, xend = 18, y = 0.0375 * lp_alt + 1.2, yend = 0.0375 * lp_alt + 1.2), color = current_cols[1], size = 1) +
        # annotate("text", x = 18.5, y = 0.0375 * lp_alt + 1.2, label = "Low GSS Measure", size = 5.5, hjust = 0, family = "Times New Roman") +
        # geom_segment(aes(x = 17.25, xend = 18, y = 0.0875 * lp_alt + 1.2, yend = 0.0875 * lp_alt + 1.2), color = current_cols[2], size = 1) +
        # annotate("text", x = 18.5, y = 0.0875 * lp_alt + 1.2, label = "High GSS Measure", size = 5.5, hjust = 0, family = "Times New Roman")
        # Second set of legends (Low and High GSS Measure)
        geom_segment(aes(x = 17.25, xend = 18, y = 0.0875 * lp_alt + 1.2, yend = 0.0875 * lp_alt + 1.2), color = current_cols[1], size = 1) +
        annotate("text", x = 18.5, y = 0.0875 * lp_alt + 1.2, label = "Low GSS Measure", size = 5.5, hjust = 0, family = "Times New Roman") +
        geom_segment(aes(x = 17.25, xend = 18, y = 0.0375 * lp_alt + 1.2, yend = 0.0375 * lp_alt + 1.2), color = current_cols[2], size = 1) +
        annotate("text", x = 18.5, y = 0.0375 * lp_alt + 1.2, label = "High GSS Measure", size = 5.5, hjust = 0, family = "Times New Roman")
      
      plot_grid(g2, NULL, g1, align = "h", axis = "bt", labels = "", ncol = 1, rel_heights = c(7, 0.1, 4.2))
      ggsave_(here("results", "figures", str_c("het_survey_measures_", regression_parameters$label[r], "_", t, wt, "_romano.eps")), width = 10, height = 6)
      #ggsave_(here("results", "figures", str_c("het_survey_measures_", regression_parameters$label[r], "_", t, wt, "_pres_romano.eps")), width = 10, height = 6)
      # Define time
      ot_l_title <- case_when(t == "firstday" ~ "on September 29",
                              t == "ontime" ~ "by October 6",
                              t == "late" ~ "from September 29 to March 31")
      # Define footnote.
      current_footnote <- paste(
        # Introduction
        # 1. What is shown in this figure?
        "This figure shows treatment effects and take-up rates by",
        str_to_lower(regression_parameters$group_treat[r]),
        "and level of survey measures.", 
        # 2. What is the unit of observation?
        "The unit of observation is at the firm level.",
        # 3. What is the regression in the figure?
        "The top panel of the figure shows treatment effects of",  
        str_to_lower(regression_parameters$group_treat[r]), "on lower fee offer take-up, splitting by low or high survey measure response.",
        "The coefficients and confident intervals come from the regression",
        "$\\mathbbm{1}(Adopt)_i = \\beta_0 + \\beta_1 \\mathbbm{1}(Survey \\; measure)_i +",
        str_c("\\beta_2 \\mathbbm{1}(", str_replace(regression_parameters$group_treat[r], fixed(" "), " \\; "), ")_i +"),
        "\\beta_3 \\mathbbm{1}(Survey \\; measure)_i \\times",
        str_c("\\mathbbm{1}(", str_replace(regression_parameters$group_treat[r], fixed(" "), " \\; "), ")_i + \\varepsilon_i$,"),
        "where the ``Low''  coefficient is $\\beta_2$, the ``High''  coefficient is $\\beta_2 + \\beta_3$",
        "and the significance stars come from the significance of $\\beta_3$.",
        "The bottom panel of the figure displays take-up rates for firms by they survey measure and treatment status.",
        
        "\\\\ \n\\indent",
        
        # Data
        # 1. Where does the data come from? If a survey, which firms were asked the questions used and how many firms are omitted from the table?
        "Data comes from survey conducted on a random sample of firms in the experiment",
        "(\\emph{N} = \\inputnumber{survey_n_success.tex}),",
        str_c("includes take-up", ot_l_title, ", and "),
        "firms from the", str_to_lower(regression_parameters$group_treat[r]),
        "and", str_to_lower(regression_parameters$group_control[r]), "groups.",
        "All \\inputnumber{survey_n_success.tex} firms in the survey were asked this particular question. ",
        survey_footnote,
        data_survey_measures %>% filter(is.na(trust_scale_1_binary)) %>% nrow(), 
        "firms that did not answer the question were excluded from the sample. ",
        
        "\\\\ \n\\indent",
        
        # Econometric details
        ifelse(wt != "", "Observations are weighted by the inverse probability of being sampled.", ""),
        "Confidence intervals constructed using robust standard errors.",
        "* $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.")
      
      # format_save_graph_footnote(current_footnote, str_c("het_survey_measures_fig_", regression_parameters$label[r], "_", t, wt, ".tex"))
      # Generate latex code.
      text %<>%
        append(str_c(ifelse(counter %in% seq(1, 18, 6), str_c("\\subsection*{", regression_parameters$sub_title[r], "} \n"), ""),
                     ifelse(counter %in% seq(1, 18, 3), str_c("\\subsubsection*{Using ", ifelse(wt == "_wt", "weighted", "unweighted"), " data} \n"), ""),
                     str_c("\\textbf{Take-up ", ot_l_title, "} \n"),
                     "% BAR PLOT: Correlation Between Take-up ", ot_l_title, " and Survey Measures \n", 
                     "% Figure generated in scripts/cesar/22_bar_plots.R. \n",
                     "\\begin{figure}[H] \n",
                     "\\caption{Heterogeneous Treatment Effects of ", regression_parameters$group_treat[r], " by Survey Measures} \n",
                     "\\label{fig:", str_c("het_survey_measures_", regression_parameters$label[r], "_", t, wt), "} \n",
                     "\\centering \n",
                     "\\includegraphics[width = 0.75\\textwidth]{./results/figures/", 
                     str_c("het_survey_measures_", regression_parameters$label[r], "_", t, wt, ".eps"), "} \n",
                     "\\end{figure} \n",
                     "\\input{./results/tables/het_survey_measures_", regression_parameters$label[r], "_", t, wt, ".tex", "} \n",
                     "\\input{./results/notes/het_survey_measures_", regression_parameters$label[r], "_", t, wt, ".tex", "} \n",
                     "\\clearpage \n"))
      counter %<>% + 1
      }
    }
  }
}



