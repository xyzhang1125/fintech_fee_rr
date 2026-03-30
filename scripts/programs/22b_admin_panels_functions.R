#--------------------------------------------------------------------------------------------
# File name: 		      22b_admin_panels_functions.R
# Creation date:      2023-05-20
# Author:          		César Landín
# Files used:
# 	- (None)
# Files created:
#   - (None)
# Purpose:
# 	- Define parameters and functions for take-up/treatment effect panel figures.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(cowplot)
source(here("scripts", "programs", "output_directory.R"))
#########################################################

#####################################################
##  (1): Define figure parameters and functions.   ##
#####################################################
# (1.1): Define panel file names and groups.
het_files <- tribble(~filename, ~group, ~axis_group,
                     ##### MAIN FIGURES ####
                     # Effect of lower fee
                     "panel_cum_accepted_offer_line_pooled_fee.eps", "fee", "main",
                     "panel_cum_accepted_offer_beta_pooled_fee.eps", "fee", "main",
                     # Effect of reminder
                     "panel_cum_accepted_offer_line_pooled_rem.eps", "rem", "main",
                     "panel_cum_accepted_offer_beta_pooled_rem.eps", "rem", "main",
                     # Effect of unanticipated reminder
                     "panel_cum_accepted_offer_line_pooled_unantrem.eps", "unantrem", "main",
                     "panel_cum_accepted_offer_beta_pooled_unantrem.eps", "unantrem", "main",
                     # Effect of deadline
                     "panel_cum_accepted_offer_line_pooled_dl.eps", "dl", "main",
                     "panel_cum_accepted_offer_beta_pooled_dl.eps", "dl", "main",
                     # Effect of anticipated reminder
                     "panel_cum_accepted_offer_line_pooled_antrem.eps", "antrem", "main", 
                     "panel_cum_accepted_offer_beta_pooled_antrem.eps", "antrem", "main",
                     
                     ##### OPEN EMAIL (first for paper, others for presentation) #####
                     # Effect of lower fee conditional on opening email
                     "panel_cum_accepted_offer_line_pooled_fee_openemail.eps", "fee_openemail", "openemail",
                     "panel_cum_accepted_offer_beta_pooled_fee_openemail.eps", "fee_openemail", "openemail",
                     # Effect of reminder conditional on opening email
                     "panel_cum_accepted_offer_line_pooled_rem_openemail.eps", "rem_openemail", "openemail_rem",
                     "panel_cum_accepted_offer_beta_pooled_rem_openemail.eps", "rem_openemail", "openemail_rem",
                     # Effect of unanticipated reminder conditional on opening email
                     # "panel_cum_accepted_offer_line_pooled_unantrem_openemail.eps", "unantrem_openemail", "openemail_unantrem",
                     # "panel_cum_accepted_offer_beta_pooled_unantrem_openemail.eps", "unantrem_openemail", "openemail_unantrem",
                     # Effect of deadline conditional on opening email
                     "panel_cum_accepted_offer_line_pooled_dl_openemail.eps", "dl_openemail", "openemail",
                     "panel_cum_accepted_offer_beta_pooled_dl_openemail.eps", "dl_openemail", "openemail",
                     # Effect of anticipated reminder conditional on opening email
                     "panel_cum_accepted_offer_line_pooled_antrem_openemail.eps", "antrem_openemail", "openemail",
                     "panel_cum_accepted_offer_beta_pooled_antrem_openemail.eps", "antrem_openemail", "openemail",
                     
                     # # Effect of reminder by offer value conditional on opening email
                     "panel_cum_accepted_offer_line_feetype_rem_openemail.eps", "rem_fee_openemail", "openemail",
                     "panel_cum_accepted_offer_beta_feetype_rem_openemail.eps", "rem_fee_openemail", "openemail",
                     ## Effect of unanticipated reminder by offer value conditional on opening email
                     # "panel_cum_accepted_offer_line_feetype_unantrem_openemail.eps", "unantrem_fee_openemail", "openemail",
                     # "panel_cum_accepted_offer_beta_feetype_unantrem_openemail.eps", "unantrem_fee_openemail", "openemail",
                     # Effect of deadline by offer value conditional on opening email
                     "panel_cum_accepted_offer_line_feetype_dl_openemail.eps", "dl_fee_openemail", "openemail",
                     "panel_cum_accepted_offer_beta_feetype_dl_openemail.eps", "dl_fee_openemail", "openemail",
                     # Effect of anticipated reminder by offer value conditional on opening email
                     "panel_cum_accepted_offer_line_feetype_antrem_openemail.eps", "antrem_fee_openemail", "openemail",
                     "panel_cum_accepted_offer_beta_feetype_antrem_openemail.eps", "antrem_fee_openemail", "openemail",
                     
                     ##### LONG TERM ####
                     # Long-term effect of lower fee
                     "panel_cum_accepted_offer_line_pooled_fee_long.eps", "fee_long", "long",
                     # Long-term effect of reminder
                     "panel_cum_accepted_offer_line_pooled_rem_long.eps", "rem_long", "long",
                     # Long-term effect of unanticipated reminder
                     # "panel_cum_accepted_offer_line_pooled_unantrem_long.eps", "unantrem_long", "long",
                     # Long-term effect of deadline
                     "panel_cum_accepted_offer_line_pooled_dl_long.eps", "dl_long", "long",
                     # Long-term effect of anticipated reminder
                     "panel_cum_accepted_offer_line_pooled_antrem_long.eps", "antrem_long", "long",
                     
                     # Long-term effect of lower fee conditional on no deadline
                     "panel_cum_accepted_offer_line_nodl_fee_long.eps", "fee_nodl_long", "long",
                     # Long-term effect of reminder conditional on no deadline
                     "panel_cum_accepted_offer_line_nodl_rem_long.eps", "rem_nodl_long", "long",
                     # Long-term effect of unanticipated reminder conditional on no deadline
                     # "panel_cum_accepted_offer_line_pooled_unantrem_long.eps", "unantrem_long", "long",
                     # Long-term effect of anticipated reminder conditional on no deadline
                     "panel_cum_accepted_offer_line_nodl_antrem_long.eps", "antrem_nodl_long", "long",
                     
                     # Long-term effect of reminder by offer value
                     "panel_cum_accepted_offer_line_feetype_rem_long.eps", "rem_feetype_long", "long",
                     # # Long-term effect of unanticipated reminder by offer value
                     # "panel_cum_accepted_offer_line_feetype_unantrem_long.eps", "unantrem_feetype_long", "long",
                     # Long-term effect of deadline by offer value
                     "panel_cum_accepted_offer_line_feetype_dl_long.eps", "dl_feetype_long", "long",
                     # Long-term effect of anticipated reminder by offer value
                     "panel_cum_accepted_offer_line_feetype_antrem_long.eps", "antrem_feetype_long", "long",
                     # Long-term effect of deadline by baseline sales
                     "panel_cum_accepted_offer_line_fsize_dl_long.eps", "dl_fsize_long", "long",
                     
                     #### OFFER VALUE ####
                     # Effect of reminder by offer value
                     "panel_cum_accepted_offer_line_feetype_rem.eps", "rem_feetype", "main",
                     "panel_cum_accepted_offer_beta_feetype_rem.eps", "rem_feetype", "main",
                     # # Effect of unanticipated reminder by offer value
                     # "panel_cum_accepted_offer_line_feetype_unantrem.eps", "unantrem_feetype", "main",
                     # "panel_cum_accepted_offer_beta_feetype_unantrem.eps", "unantrem_feetype", "main",
                     # Effect of deadline by offer value
                     "panel_cum_accepted_offer_line_feetype_dl.eps", "dl_feetype", "main",
                     "panel_cum_accepted_offer_beta_feetype_dl.eps", "dl_feetype", "main",
                     # Effect of anticipated reminder by offer value 
                     "panel_cum_accepted_offer_line_feetype_antrem.eps", "antrem_feetype", "main",
                     "panel_cum_accepted_offer_beta_feetype_antrem.eps", "antrem_feetype", "main",
                     
                     #### BASELINE SALES ####
                     # Reminder by baseline sales
                     "panel_cum_accepted_offer_line_fsize_rem.eps", "rem_fsize", "main",
                     "panel_cum_accepted_offer_beta_fsize_rem.eps", "rem_fsize", "main",
                     # # Unanticipated reminder by baseline sales
                     # "panel_cum_accepted_offer_line_fsize_unantrem.eps", "unantrem_fsize", "main",
                     # "panel_cum_accepted_offer_beta_fsize_unantrem.eps", "unantrem_fsize", "main",
                     # Deadline by baseline sales
                     "panel_cum_accepted_offer_line_fsize_dl.eps", "dl_fsize", "main",
                     "panel_cum_accepted_offer_beta_fsize_dl.eps", "dl_fsize", "main",
                     # Anticipated reminder by baseline sales
                     "panel_cum_accepted_offer_line_fsize_antrem.eps", "antrem_fsize", "main",
                     "panel_cum_accepted_offer_beta_fsize_antrem.eps", "antrem_fsize", "main",
                     
                     #### EXPECTED GAIN ####
                     # Reminder by expected gain
                     "panel_cum_accepted_offer_line_expgain_rem.eps", "rem_expgain", "main",
                     "panel_cum_accepted_offer_beta_expgain_rem.eps", "rem_expgain", "main",
                     # Deadline by expected gain
                     "panel_cum_accepted_offer_line_expgain_dl.eps", "dl_expgain", "main",
                     "panel_cum_accepted_offer_beta_expgain_dl.eps", "dl_expgain", "main",
                     # Anticipated reminder by expected gain
                     "panel_cum_accepted_offer_line_expgain_antrem.eps", "antrem_expgain", "main",
                     "panel_cum_accepted_offer_beta_expgain_antrem.eps", "antrem_expgain", "main",
                     
                     #### INTERACTIVE TREATMENT EFFECTS ####
                     # Reminder conditional on deadline
                     "panel_cum_accepted_offer_line_dltreat_rem.eps", "dltreat_rem", "interactions",
                     "panel_cum_accepted_offer_beta_dltreat_rem.eps", "dltreat_rem", "interactions",
                     # Deadline conditional on reminder
                     "panel_cum_accepted_offer_line_dltreat_rem.eps", "dltreat_dlrem", "interactions",
                     "panel_cum_accepted_offer_beta_dlcond_rem.eps", "dltreat_dlrem", "interactions",
                     # # Unanticipated reminder conditional on deadline
                     # "panel_cum_accepted_offer_line_dltreat_unantrem.eps", "dltreat_unantrem", "interactions",
                     # "panel_cum_accepted_offer_beta_dltreat_unantrem.eps", "dltreat_unantrem", "interactions",
                     # # Deadline conditional on unanticipated reminder
                     # "panel_cum_accepted_offer_line_dltreat_unantrem.eps", "dltreat_dlunantrem", "interactions",
                     # "panel_cum_accepted_offer_beta_dlcond_unantrem.eps", "dltreat_dlunantrem", "interactions",
                     # Anticipated reminder conditional on deadline
                     "panel_cum_accepted_offer_line_dltreat_antrem.eps", "dltreat_antrem", "interactions",
                     "panel_cum_accepted_offer_beta_dltreat_antrem.eps",  "dltreat_antrem", "interactions",
                     # Deadline conditional on anticipated reminder
                     "panel_cum_accepted_offer_line_dltreat_antrem.eps", "dltreat_dlantrem", "interactions",
                     "panel_cum_accepted_offer_beta_dlcond_antrem.eps", "dltreat_dlantrem", "interactions") %>% 
  mutate(objectname = str_remove(filename, "panel_") %>% str_remove(".eps"))

# (1.2): Define group labels.
group_labels <- tribble(~group, ~title, ~labels, ~slabels,
                        "fee", "Effect of Lower Fee on Take-Up", NA, NA,
                        "unantrem", "Effect of Unannounced Reminder on Take-Up", NA, NA,
                        "rem", "Effect of Reminder on Take-Up", NA, NA,
                        "dl", "Effect of Deadline on Take-Up", NA, NA,
                        "antrem", "Effect of Announced Reminder on Take-Up", NA, NA,
                        "main", "Effect of Treatment on Take-Up", 
                        "Reminder Effect; Announced-reminder Effect; Deadline Effect",
                        "rem; antrem; dl",
                        
                        "long",
                        "Long-Term Offer Take-Up by Treatment",
                        "Reminder Effect; Announced-reminder Effect; Deadline Effect; Offer Value",
                        "rem; antrem; dl; fee",
                        
                        "dl_het_long",
                        "Long-Term Offer Take-Up by Deadline, Fee and Baseline Sales",
                        paste("Take-Up by Deadline and Fee;",
                              "Take-Up by Deadline and Baseline Sales"),
                        "feetype; fsize",

                        "rem_openemail", "Effect of Reminder on Take-Up Conditional on Opening the Email ", NA, NA,
                        
                        "feetype",
                        "Treatment Effect on Take-Up by Offer Value",
                        "Reminder Effect; Announced-reminder Effect; Deadline Effect",
                        "rem; antrem; dl",

                        "fsize",
                        "Treatment Effect on Take-Up by Baseline Sales",
                        "Reminder Effect; Announced-reminder Effect; Deadline Effect",
                        "rem; antrem; dl",
                        
                        "expgain",
                        "Treatment Effect on Take-Up by Expected Gain",
                        "Reminder Effect; Announced-reminder Effect; Deadline Effect",
                        "rem; antrem; dl",

                        "dl_rem",
                        "Effect of Deadlines Interacted with Reminder",
                        paste("Take-Up by Treatment;",
                              "Effect of Deadline Conditional on Reminder;", 
                              "Effect of Reminder Conditional on Deadline"),
                        "takeup; dl_cond; tr_cond",
                        
                        "dl_antrem",
                        "Effect of Deadlines Interacted with Announced Reminder",
                        paste("Take-Up by Treatment;",
                              "Effect of Announced Reminder Conditional on Deadline;", 
                              "Effect of Deadline Conditional on Announced Reminder"),
                        "takeup; tr_cond; dl_cond")

# (1.3): Define functions to get the axes of a list of figures.
get_axes <- function(plot) {
  tribble(~y_min, ~y_max,
          layer_scales(plot)$y$range$range[1],
          layer_scales(plot)$y$range$range[2])
}
get_axes_all <- function(graphs) {
  ranges <- lapply(graphs, get_axes) %>% 
    bind_rows()
  output <- c(min(ranges$y_min), max(ranges$y_max))
  return(output)
}
get_axes_all_group <- function(agroup, type, min_max) {
  current_object_names <- het_files %>% 
    filter(axis_group == agroup) %>% 
    filter(str_detect(filename, type)) %>%
    pull(objectname)
  current_axes <- get_axes_all(lapply(current_object_names, get))[ifelse(min_max == "min", 1, 2)]
  return(current_axes)
}

# (1.4): Define function for plotting one line with two figures.
plot_line <- function(fig_left, fig_right, current_line_y_min, current_line_y_max, current_beta_y_min, current_beta_y_max) {
  plot_grid(fig_left + 
              scale_y_continuous(limits = c(current_line_y_min, current_line_y_max)),
            # NULL, # Insert some space between figures
            fig_right + 
              scale_y_continuous(limits = c(current_beta_y_min, current_beta_y_max)), 
            rel_widths = c(1, 1),
            nrow = 1,
            align = "v") 
    # theme(plot.margin = unit(c(2, 0, 2, 0), "cm")) # Add some margin to top and bottom of figures
}

# (1.5): Define function to get objects.
get_object <- function(string) {
  eval(as.name(string))
}
