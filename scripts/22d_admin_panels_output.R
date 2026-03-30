#--------------------------------------------------------------------------------------------
# File name: 		      22d_admin_panels_output.R
# Creation date:      2023-05-20
# Author:          		César Landín
# Files used:
#   - here("proc", "graphs_admin_panel.qs")
#   - here("proc", "fintech_fee_light.csv")
# Files created:
#   - here("results", "figures", "cum_accepted_offer_panel_main.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_main.eps")
#   - here("results", "figures", "cum_accepted_offer_panel_long.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_feetype.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_fsize.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_expgain.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_dltreat_rem.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_dltreat_antrem.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_dl_het_long.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_fee.tex")
#   - here("results", "figures", "cum_accepted_offer_panel_rem_openemail.tex")
# Purpose:
# 	- Generate panels with take-up and treatment graphs for the paper and presentation.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(cowplot)
library(qs)
library(scales)
library(tabulator)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # changed to times
showtext_auto()
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "22b_admin_panels_functions.R"))
source(here("scripts", "programs", "graph_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

###########################################
##  (1): Generate single panel figures.  ##
###########################################
# (1.1): Import figures (presentation versions) SUBSTITUTE FOR PANEL VERSIONS
panel_figs <- qread(here("proc", "graphs_admin_panel.qs"))
for (i in 1:length(panel_figs)) {
  object <- panel_figs[[i]]
  assign(names(panel_figs)[i], object)
}
rm(panel_figs, object)

# (1.3): Load take-up data.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
  mutate(fee_3.00 = ifelse(fee_type == "3% offer", 1, ifelse(is.na(fee_type), NA, 0)))

# (1.2): Define single panel plotting function.
plot_single_panel <- function(g, ...) {
  # Keep files in selected group
  current_group <- het_files %>% 
    filter(group == g) %>% 
    mutate(col_pos = c(1:2))
    
  
  # Split between beta/line figures
  current_figs_line <- current_group %>% filter(str_detect(filename, "line")) %>% pull(objectname)
  current_figs_beta <- current_group %>% filter(str_detect(filename, "beta")) %>% pull(objectname)
  
  # Get plots
  left_plot <- current_group %>% filter(col_pos == 1) %>% pull(objectname) %>% lapply(get_object) %>% .[[1]]
  right_plot <- current_group %>% filter(col_pos == 2) %>% pull(objectname) %>% lapply(get_object) %>% .[[1]]
  
  # Fix number of legend rows
  max_nrows <- max(left_plot[["guides"]][["colour"]][["nrow"]], right_plot[["guides"]][["colour"]][["nrow"]])
  left_plot[["guides"]][["colour"]][["nrow"]] <- max_nrows 
  right_plot[["guides"]][["colour"]][["nrow"]] <- max_nrows
  
  # Fix plot margins
  left_plot <- left_plot +
    theme(#legend.position = c(0.30, 0.75),
      #legend.margin = margin(l = -77, b = -12),
          #legend.box.margin = margin(b = -3),
          axis.title.x = element_text(margin = margin(t = 5, b = 8)))
  right_plot <- right_plot +
    theme(
      #legend.margin = margin(l = -77, b = -12),
          #legend.box.margin = margin(b = -3),
          # legend.spacing.y = unit(0.5, "cm"), # can't use this since byrow is not true
          #legend.key.height = unit(39, "points"),
          axis.title.x = element_text(margin = margin(t = 5, b = 8)))
  
  # Generate plot
  current_lines <- plot_grid(left_plot, NULL, right_plot,
                             rel_widths = c(1, 0.025, 1),
                             # rel_heights = c(1, 0.2),
                             nrow = 1,
                             align = "hv", axis = "tbl", 
                             ...) +
    theme(plot.margin = margin(t = 0))
  current_lines
  
  # Set smaller height for main / open email figures
  if ((het_files %>% filter(group == g) %>% pull(axis_group) %>% unique()) %in% 
      c("main", "openemail", "openemail_unantrem")) {
    current_height <- 6.5
  } else {
    current_height <- 8
  }
  
  # Save plot
  current_name <- str_c("cum_accepted_offer_panel_", g, ".eps")
  ggsave(here("results", "figures", current_name), width = 22, height = current_height)
}


# Define list of figures to use.
keep_files <- c(
  # Figure 8
  "panel_cum_accepted_offer_beta_feetype_rem.eps",
  "panel_cum_accepted_offer_line_feetype_rem.eps",
  "panel_cum_accepted_offer_beta_feetype_antrem.eps",
  "panel_cum_accepted_offer_line_feetype_antrem.eps",
  "panel_cum_accepted_offer_beta_feetype_dl.eps",
  "panel_cum_accepted_offer_line_feetype_dl.eps",
  
  # Figure 7
  "panel_cum_accepted_offer_beta_fsize_rem.eps",
  "panel_cum_accepted_offer_line_fsize_rem.eps",
  "panel_cum_accepted_offer_beta_fsize_antrem.eps",
  "panel_cum_accepted_offer_line_fsize_antrem.eps",
  "panel_cum_accepted_offer_beta_fsize_dl.eps",
  "panel_cum_accepted_offer_line_fsize_dl.eps",
  
  # Figure C.9
  "panel_cum_accepted_offer_beta_expgain_rem.eps",
  "panel_cum_accepted_offer_line_expgain_rem.eps",
  "panel_cum_accepted_offer_beta_expgain_antrem.eps",
  "panel_cum_accepted_offer_line_expgain_antrem.eps",
  "panel_cum_accepted_offer_beta_expgain_dl.eps",
  "panel_cum_accepted_offer_line_expgain_dl.eps",
  
  # Figure C.11
  "panel_cum_accepted_offer_beta_dltreat_rem.eps",
  "panel_cum_accepted_offer_line_dltreat_rem.eps",
  
  # Figure C.14 
  # "panel_cum_accepted_offer_beta_dl_unantrem_dl_antrem.eps", #Should it be _updated.eps? 
  # "panel_cum_accepted_offer_beta_dltreat_antrem.eps", #Should it be _updated.eps? 
  # "panel_cum_accepted_offer_beta_dlcond_rem_antrem_unantrem.eps",
  # "panel_cum_accepted_offer_line_dltreat_antrem_norem.eps",
  
  # Figure C.15
  "panel_cum_accepted_offer_line_fsize_dl_long.eps",
  "panel_cum_accepted_offer_line_feetype_dl_long.eps",
  
  # Figure 5
  "panel_cum_accepted_offer_beta_pooled_fee.eps",
  "panel_cum_accepted_offer_line_pooled_fee.eps",
  
  # Figure 6
  "panel_cum_accepted_offer_beta_pooled_rem.eps",
  "panel_cum_accepted_offer_line_pooled_rem.eps",
  "panel_cum_accepted_offer_beta_pooled_antrem.eps",
  "panel_cum_accepted_offer_line_pooled_antrem.eps",
  "panel_cum_accepted_offer_beta_pooled_dl.eps",
  "panel_cum_accepted_offer_line_pooled_dl.eps",
  
  # Figure C.10
  "panel_cum_accepted_offer_beta_pooled_unantrem.eps",
  "panel_cum_accepted_offer_line_pooled_unantrem.eps",
  
  #Figure C.8
  "panel_cum_accepted_offer_line_nodl_fee_long.eps",
  "panel_cum_accepted_offer_line_pooled_rem_long.eps",
  "panel_cum_accepted_offer_line_pooled_antrem_long.eps",
  "panel_cum_accepted_offer_line_pooled_dl_long.eps",
  
  # Figure C.12
  "panel_cum_accepted_offer_beta_pooled_rem_openemail.eps",
  "panel_cum_accepted_offer_line_pooled_rem_openemail.eps",
  "panel_cum_accepted_offer_beta_pooled_antrem_openemail.eps",
  "panel_cum_accepted_offer_line_pooled_antrem_openemail.eps",
  "panel_cum_accepted_offer_beta_pooled_dl_openemail.eps",
  "panel_cum_accepted_offer_line_pooled_dl_openemail.eps"
  
)

# (1.2): Plot all single panels.
het_files <- het_files %>%
  filter(group != "dltreat_dlrem") %>%
  filter(filename %in% keep_files) 

export_files <- het_files$filename 

het_files %>%
  tab(group) %>%
  pull(group) %>%
  .[!(str_detect(., "long"))] %>%
  lapply(plot_single_panel)

# (1.3): Make dataset with number of firms by treatment.
get_num_firms_data <- function(treat, notreat) {
  n_firms <- fintech_fee %>% 
    filter(eval(as.name(treat)) == 1 | eval(as.name(notreat)) == 1) %>% 
    nrow()
  day_dl <- fintech_fee %>% 
    filter(eval(as.name(treat)) == 1 | eval(as.name(notreat)) == 1) %>% 
    tab(treat_description) %>% 
    pull(treat_description) %>% 
    str_detect("24") %>% 
    sum()
  return(data.frame(n_firms = n_firms, day_dl = day_dl))
}
num_firms <- get_num_firms_data("fee_2.75", "fee_3.00") %>% 
  bind_rows(get_num_firms_data("reminder", "no_reminder")) %>% 
  bind_rows(get_num_firms_data("deadline", "no_deadline")) %>% 
  bind_rows(get_num_firms_data("anticipated_reminder", "unanticipated_reminder")) %>% 
  mutate(treat = c("fee", "rem", "dl", "antrem"))

# (1.4): Numbers
stat_n_fee <- num_firms %>% filter(treat == "fee") %>% pull(n_firms)
stat_n_fee %>% 
  print_n("stat_n_fee.tex", 
          "Number of firms with 2.75\\% and 3.00\\% offers.")
    
stat_n_rem_openemail <- fintech_fee %>% filter((reminder == 1 | no_reminder == 1) & open_email_before_reminder == 1) %>% nrow()
stat_n_rem_openemail %>% 
    print_n("stat_n_rem_openemail.tex", 
            "Number of firms with or without a reminder that opened the email before the reminder")

stat_n_dl_openemail <- fintech_fee %>% filter((deadline == 1 | no_deadline == 1) & fast_deadline == 0 & open_email_before_reminder == 1) %>% nrow()
stat_n_dl_openemail %>% 
  print_n("stat_n_dl_openemail.tex", 
          "Number of firms with or without a deadline that opened the email before the reminder")

stat_n_antrem_openemail <- fintech_fee %>% filter((anticipated_reminder == 1 | unanticipated_reminder == 1) & open_email_before_reminder == 1) %>% nrow()
stat_n_antrem_openemail %>% 
  print_n("stat_n_antrem_openemail.tex", 
          "Number of firms with an anticipated or unanticipated reminder that opened the email before the reminder")

stat_n_rem_dl <- fintech_fee %>% filter((reminder == 1 | no_reminder == 1) & (deadline == 1 | no_deadline == 1)) %>% nrow()
stat_n_rem_dl %>% 
  print_n("stat_n_rem_dl.tex", 
          "Number of firms with reminder and deadline interaction")

stat_n_antrem_norem <- fintech_fee %>% filter(anticipated_reminder == 1 | no_reminder == 1) %>% nrow()
stat_n_antrem_norem %>% 
  print_n("stat_n_antrem_norem.tex", 
          "Number of firms with anticipated reminder and no reminder")

stat_n_unantrem_norem <- fintech_fee %>% filter(unanticipated_reminder == 1 | no_reminder == 1) %>% nrow()
stat_n_unantrem_norem %>% 
  print_n("stat_n_unantrem_norem.tex", 
          "Number of firms with unanticipated reminder and no reminder")

########################################
##  (2): Generate main figure panel.  ##
########################################
# (2.1): Define latex function.
panel_het_single <- function(panelname, label, slabel) {
  str_c("\\begin{subfigure}[t]{\\textwidth}\n",
        "    \\caption{", label, "}\n",
        "    \\label{fig:", panelname, "_", slabel, "}\n",
        "    \\centering\n",
        "    \\vspace{-.48\\baselineskip}\n",
        "    \\includegraphics[width = \\textwidth]{../results/figures/", str_replace(panelname, "cum_accepted_offer_panel", str_c("cum_accepted_offer_panel_", slabel)), ".eps}\n",
        "    \\vspace{-0.3cm}\n",
        "\\end{subfigure}\n")
}

# (2.2): Generate and export main panel.
#### ***Keep for NBER (Figure 6)***
get_labels <- function(df, label_type) {
  df %>% 
    pull(all_of(label_type)) %>% 
    str_split("; ") %>% 
    .[[1]]
}
current_labels <- group_labels %>% 
  filter(group == "main") %>% 
  get_labels("labels")
current_slabels <- group_labels %>% 
  filter(group == "main") %>%
  get_labels("slabels")
mapply(panel_het_single,
       rep("cum_accepted_offer_panel", 3),
       current_labels,
       current_slabels) %>% 
  str_c(collapse = "") %>% 
  write_(here("results", "figures", "cum_accepted_offer_panel_main.tex"))
#### ***Keep for NBER***

# (2.3): Generate and export footnote.
stat_n_rem <- num_firms %>% filter(treat == current_slabels[1]) %>% pull(n_firms)
stat_n_rem %>% 
  print_n("stat_n_rem.tex",
          "Number of firms with a reminder or without a reminder")
stat_n_dl <- num_firms %>% filter(treat == current_slabels[3]) %>% pull(n_firms)
stat_n_dl %>% 
  print_n("stat_n_dl.tex",
          "Number of firms with or without a deadline")
stat_n_antrem <- num_firms %>% filter(treat == current_slabels[2]) %>% pull(n_firms)
stat_n_antrem %>% 
  print_n("stat_n_antrem.tex",
          "Number of firms with an anticipated or unanticipated reminder")


####################################################
##  (3): Generate panel with long-term take-up.   ##
####################################################
# (3.1): Define latex function.
panel_long_single <- function(objectname, label, slabel) {
  str_c("\\begin{subfigure}[t]{0.49\\textwidth}\n",
        "    \\caption{", label, "}\n",
        "    \\label{fig:cum_accepted_offer_panel_long_", slabel, "}\n",
        "    \\centering\n",
        "    \\vspace{-.5\\baselineskip}\n",
        "    \\includegraphics[width = \\textwidth]{../results/figures/panel_", objectname, ".eps}\n",
        "\\vspace{-.4cm}\n",
        "\\end{subfigure}\n")
}

# (3.2): Define function parameters.
#### ***Keep for NBER (Figure C.8)***
current_objectnames <- c("cum_accepted_offer_line_nodl_rem_long",
                         "cum_accepted_offer_line_nodl_antrem_long",
                         "cum_accepted_offer_line_pooled_dl_long", 
                         "cum_accepted_offer_line_nodl_fee_long")
current_labels <- group_labels %>% filter(group == "long") %>% get_labels("labels")
current_slabels <- group_labels %>% filter(group == "long") %>% get_labels("slabels")

# (3.3): Generate and export panel.
mapply(panel_long_single,
       current_objectnames,
       current_labels,
       current_slabels) %>% 
  str_c(collapse = "") %>% 
  write_(here("results", "figures", "cum_accepted_offer_panel_long.tex"))
#### ***Keep for NBER***

# (3.4): Generate and export footnote.
dl_nrow <- function(df) {
  df %>% 
    filter(deadline == 0) %>% 
    nrow()
}
stat_n_fee_nodl <- fintech_fee %>% 
  filter(fee_2.75 == 1 | fee_3.00 == 1) %>% 
  dl_nrow()
stat_n_fee_nodl %>% 
  print_n("stat_n_fee_nodl.tex", 
          "Number of firms with with 2.75\\% and 3.00\\% offers and without a deadline.")
stat_n_rem_nodl <- fintech_fee %>% 
  filter(reminder == 1 | no_reminder == 1) %>% 
  dl_nrow()
stat_n_rem_nodl %>% 
  print_n("stat_n_rem_nodl.tex",
          "Number of firms with a reminder or without a reminder and without a deadline")
stat_n_antrem_nodl <- fintech_fee %>% 
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1) %>% 
  dl_nrow()
stat_n_antrem_nodl %>% 
  print_n("stat_n_antrem_nodl.tex",
          "Number of firms with an anticipated or unanticipated reminder and without a deadline.")


#####################################################
##  (4): Generate fee and baseline sales panels.   ##
#####################################################
# (4.1): Generate and export fee panel.

#### ***Keep for NBER Figure 8***
current_labels <- group_labels %>% 
  filter(group == "feetype") %>%
  get_labels("labels")
current_slabels <- group_labels %>% 
  filter(group == "feetype") %>%
  get_labels("slabels")
mapply(panel_het_single,
       rep("cum_accepted_offer_panel_feetype", 3),
       current_labels,
       current_slabels) %>% 
  str_c(collapse = "") %>% 
  write_(here("results", "figures", "cum_accepted_offer_panel_feetype.tex"))
#### ***Keep for NBER***

# (4.2): Generate and export baseline sales panel.
#### ***Keep for NBER Figure 7***
current_labels <- group_labels %>% 
  filter(group == "feetype") %>%
  get_labels("labels")
current_slabels <- group_labels %>% 
  filter(group == "feetype") %>%
  get_labels("slabels")
mapply(panel_het_single,
       rep("cum_accepted_offer_panel_fsize", 3),
       current_labels,
       current_slabels) %>% 
  str_c(collapse = "") %>% 
  write_(here("results", "figures", "cum_accepted_offer_panel_fsize.tex"))
#### ***Keep for NBER***

# (4.2): Generate and export expected gain panel.
#### ***Keep for NBER (Figure C.9)***
mapply(panel_het_single,
       rep("cum_accepted_offer_panel_expgain", 3),
       current_labels,
       current_slabels) %>% 
  str_c(collapse = "") %>% 
  write_(here("results", "figures", "cum_accepted_offer_panel_expgain.tex"))
#### ***Keep for NBER***



###############################################################
##  (6): Generate long-term deadline heterogeneity panels.   ##
###############################################################
# (6.1): Define latex function.
#### ***Keep for NBER (Figure C.15)***
panel_dl_het_long <- function(panelname, objectname, label, slabel) {
  str_c("\\begin{subfigure}[t]{0.49\\textwidth}\n",
        "    \\caption{", label, "}\n",
        "    \\label{fig:", panelname, "_", slabel, "}\n",
        "    \\centering\n",
        "    \\vspace{-.5\\baselineskip}\n",
        "    \\includegraphics[width = \\textwidth]{../results/figures/panel_", objectname, ".eps}\n",
        "\\end{subfigure}\n"
        # "\\vspace{0.3\\baselineskip}\n"
        )
}

current_objectnames <- het_files %>% 
  filter(axis_group == "long" & str_detect(group, "dl") & !str_detect(objectname, "pooled|nodl")) %>% 
  pull(objectname)
current_labels <- group_labels %>% 
  filter(group == "dl_het_long") %>%
  get_labels("labels")
current_slabels <- group_labels %>% 
  filter(group == "dl_het_long") %>%
  get_labels("slabels")
mapply(panel_dl_het_long,
       rep("cum_accepted_offer_panel_dl_het_long", 2),
       current_objectnames,
       current_labels,
       current_slabels) %>% 
  str_c(collapse = "") %>% 
  write_(here("results", "figures", "cum_accepted_offer_panel_dl_het_long.tex"))
#### ***Keep for NBER***



### Other Latex figures ###
### ***Keep for NBER (Figure C.12)***
generate_subfigure_latex <- function(captions, labels, graphics_paths) {
  # Check if input vectors have the same length
  if (length(captions) != length(labels) || length(labels) != length(graphics_paths)) {
    stop("All input vectors (captions, labels, graphics_paths) must have the same length.")
  }
  
  # Initialize an empty string to store the generated LaTeX code
  latex_code <- ""
  
  # Loop through each subfigure and create the LaTeX code
  for (i in seq_along(captions)) {
    latex_code <- paste0(latex_code, 
                         "\\begin{subfigure}[t]{\\textwidth}\n",
                         "    \\caption{", captions[i], "}\n",
                         "    \\label{", labels[i], "}\n",
                         "    \\centering\n",
                         "    \\vspace{-.48\\baselineskip}\n",
                         "    \\includegraphics[width=\\textwidth]{", graphics_paths[i], "}\n",
                         "    \\vspace{-0.3cm}\n",
                         "\\end{subfigure}\n\n")
  }
  
  return(latex_code)
}

# Usage (openemail): 
captions <- c("Reminder Effect", "Announced-reminder Effect", "Deadline Effect")
labels <- c("fig:cum_accepted_offer_panel_rem_openemail", 
            "fig:cum_accepted_offer_panel_antrem_openemail",
            "fig:cum_accepted_offer_panel_dl_openemail")
graphics_paths <- c("../results/figures/cum_accepted_offer_panel_rem_openemail.eps", 
                    "../results/figures/cum_accepted_offer_panel_antrem_openemail.eps",
                    "../results/figures/cum_accepted_offer_panel_dl_openemail.eps")

latex_code <- generate_subfigure_latex(captions, labels, graphics_paths)

# Write the LaTeX code to a .tex file
output_path <- here("results", "figures", "cum_accepted_offer_panel_openemail.tex")
writeLines(latex_code, output_path)
######## ***Keep for NBER***


#### ***Keep for NBER Figure C.14***
generate_subfigure_latex_four <- function(captions, labels, graphics_paths, width = "\\textwidth") {
  # Check if input vectors have the same length
  if (length(captions) != length(labels) || length(labels) != length(graphics_paths)) {
    stop("All input vectors (captions, labels, graphics_paths) must have the same length.")
  }
  
  # Initialize an empty string to store the generated LaTeX code
  latex_code <- ""
  
  # Loop through each subfigure and create the LaTeX code
  for (i in seq_along(captions)) {
    latex_code <- paste0(latex_code, 
                         "\\begin{subfigure}[t]{", width, "}\n",
                         "    \\caption{", captions[i], "}\n",
                         "    \\label{", labels[i], "}\n",
                         "    \\centering\n",
                         "    \\vspace{-.5\\baselineskip}\n",
                         "    \\includegraphics[width=\\textwidth]{", graphics_paths[i], "}\n",
                         "    \\vspace{-0.4cm}\n",
                         "\\end{subfigure}\n")
  }
  
  return(latex_code)
}

# Example usage
captions <- c("Take-Up by Treatment", "Reminder Type Cond’l on Deadline", "Announced Reminder Cond’l on Deadline", "Deadline Cond’l on Reminder Type")
labels <- c("fig:cum_accepted_offer_line_dltreat_antrem_norem", 
            "fig:cum_accepted_offer_beta_dl_unantrem_dl_antrem", 
            "fig:cum_accepted_offer_beta_dltreat_antrem",
            "fig:cum_accepted_offer_beta_dlcond_rem_antrem_unantrem")
graphics_paths <- c("../results/figures/panel_cum_accepted_offer_line_dltreat_antrem_norem.eps", 
                    "../results/figures/panel_cum_accepted_offer_beta_dl_unantrem_dl_antrem.eps", 
                    "../results/figures/panel_cum_accepted_offer_beta_dltreat_antrem.eps",
                    "../results/figures/panel_cum_accepted_offer_beta_dlcond_rem_antrem_unantrem.eps")

# Set the width for subfigures as 0.49\textwidth
latex_code <- generate_subfigure_latex_four(captions, labels, graphics_paths, width = "0.49\\textwidth")

# Write the LaTeX code to a .tex file
output_path <- here("results", "figures", "cum_accepted_offer_line_beta_dl_antrem_unantrem.tex")
writeLines(latex_code, output_path)
#### ***Keep for NBER***
