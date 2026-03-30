#--------------------------------------------------------------------------------------------
# File name: 		      22c_admin_graphs_output.R
# Creation date:      2021-10-21
# Author:          		César Landín
# Files used:
# 	- here("proc", "24a_weekly_sales.csv")
# 	- here("proc", "24a_monthly_sales.csv")
# 	- here("proc", "fintech_fee_light.csv")
# 	- here("proc", "balance_table_data.csv")
# 	- here("proc", "survey_successful.csv")
# 	- here("proc", "regs_for_figs.qs")
# 	- here("proc", "admin_graph_axis_dataset_line.csv")
# 	- here("proc", "admin_graph_axis_dataset_beta.csv")
# Files created:
# 	- here("results", "figures", "sales_by_week.eps")
# 	- here("results", "figures", "pres_sales_by_month.eps")
# 	- here("results", "figures", "sales_by_month.eps")
# 	- here("proc", "admin_graph_axis_dataset_line.csv")
# 	- here("proc", "admin_graph_axis_dataset_beta.csv")
# 	- here("proc", "graphs_admin_panel.qs")
# Purpose:
# 	- Generate main take-up and treatment effect graphs for paper and presentation using
#     the functions defined in 12a_admin_graphs_functions.R.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
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
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # changed to times
showtext_auto()
conflict_prefer("last", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(magrittr::set_names)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "output_directory.R"))
source(here("scripts", "programs", "set_theme.R"))
source(here("scripts", "programs", "22a_admin_graphs_functions.R"))
source(here("scripts", "programs", "22b_admin_panels_functions.R"))
#########################################################

################################################################
##    (1): Process sales, take-up and treatment effect data.  ##
################################################################
# (1.1): Import weekly and monthly sales.
# This is generated in 24a_prepsalestrends.R
# 33,978 unique firms.
weekly_sales <- read_csv(here("proc", "24a_weekly_sales.csv"))
monthly_sales <- read_csv(here("proc", "24a_monthly_sales.csv"))

# (1.2): Process weekly and monthly sales data.
proc_sales_data <- function(data, variable) {
  proc_data <- data %>% 
    select(all_of(variable), sales_w_control, sales_w_offer,sales_w_accepted) %>% 
    pivot_longer(cols = 2:length(.)) %>%
    mutate(name = gsub("sales_w_|sales_log_|sales_asinh_", "", name)) %>%
    arrange(eval(as.name(variable))) %>%
    mutate(value = ifelse(name == "accepted" & eval(as.name(variable)) < ymd("2020-09-28"), NA, value)) %>%
    mutate(name = case_when(name == "control" ~ "Control", 
                            name == "offer" ~ "Received Offer",
                            name == "accepted" ~ "Accepted Offer"))
  return(proc_data)
}
weekly_sales_win_dat <- weekly_sales %>% proc_sales_data("timestamp_week")
monthly_sales_win_dat <- monthly_sales %>% proc_sales_data("timestamp_month")
rm(weekly_sales, monthly_sales)

# (1.3): Load take-up data.
fintech_fee <- fread(here("proc", "fintech_fee_light.csv")) %>%
  mutate(fee_3 = ifelse(fee_type == "3% offer", 1, ifelse(is.na(fee_type), NA, 0)))
fintech_fee %>% tab(fee_type, fee_2.75, fee_3)

# (1.4): Calculate differences in takeup.
takeup_dif(takeup_rate_pooled("rem"), "rem")
takeup_dif(takeup_rate_pooled("dl"), "dl")
takeup_dif(takeup_rate_pooled("antrem"), "antrem")
takeup_dif(takeup_rate_pooled("fee"), "fee")

# (1.5): Calculate above / below median baseline sales.
baseline_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
  select(organization_uuid, valid_volume_w5, valid_volume) %>% 
  mutate(median_sales = median(valid_volume_w5),
         above_median_sales = ifelse(valid_volume_w5 >= median_sales, 1, 0))
baseline_data %>% tab(above_median_sales)
baseline_data %>% select(-organization_uuid) %>% group_by(above_median_sales) %>% summarise_all(mean)
fintech_fee %<>% left_join(baseline_data %>% select(-median_sales), by = "organization_uuid")

# (1.7): Calculate above / below median number of employees.
survey_data <- read_csv(here("proc", "survey_successful.csv"))
survey_data %<>% 
  filter(nb_employees != -888) %>%
  mutate(above_median_employees = ifelse(nb_employees >= median(nb_employees, na.rm = TRUE), 1, 0))
fintech_fee %<>% 
  left_join(survey_data %>% 
              select(organization_uuid, above_median_employees), 
            by = "organization_uuid")

# (1.8): Calculate expected gain
fintech_fee %<>%
  # Multiply pre-experiment sales by fee reduction
  mutate(old_fee = ifelse(commission_model == "FIXED_RATE", 3.5, 3.75),
         new_fee = ifelse(fee_type == "2.75% offer", 2.75, 3.00),
         fee_reduction = (old_fee - new_fee)/100,
         savings_potential = valid_volume * fee_reduction)

# (1.9): Calculate above / below median expected gain.
fintech_fee %<>% 
  mutate(above_median_expected_gain = ifelse(savings_potential >= median(savings_potential, na.rm = TRUE), 1, 0))

#(1.10): Process data by treatment group * interaction #
create_interaction_group <- function(df, group1, group2) {
  for (group2_var in group2) {
    for (group1_var in group1) {
      print(paste("Working on group1", group1_var, "group2", group2_var))
      name <- str_c(str_split(group1_var, "_")[[1]][1], "_", str_split(group2_var, "_")[[1]][1], "_group")
      df %<>%
        mutate(interaction_group = ifelse(!is.na(eval(as.name(group1_var))) & !is.na(eval(as.name(group2_var))),
                                          paste(eval(as.name(group1_var)), eval(as.name(group2_var))), NA),
               interaction_group = str_replace(interaction_group, " ", " & ") %>%
                 str_replace_all("_", " ") %>%
                 str_to_sentence()) %>%
        rename(!!name := interaction_group)
    }
  }
  return(df)
}



fintech_fee %<>%
  mutate(rem_group    = case_when(reminder    == 1 ~ "reminder",
                                  no_reminder == 1 ~ "no_reminder"),
         unantrem_group = case_when(unanticipated_reminder == 1 ~ "unannounced_reminder",
                                    no_reminder == 1 ~ "no_reminder"),
         dl_group     = case_when(deadline    == 1 ~ "deadline", 
                                  no_deadline == 1 ~ "no_deadline"),
         antrem_group = case_when(anticipated_reminder   == 1 ~ "announced_reminder",
                                  unanticipated_reminder == 1 ~ "unannounced_reminder"),
         fsize_group  = case_when(above_median_sales == 1 ~ "above_median_sales",
                                  above_median_sales == 0 ~ "below_median_sales"),
         numemp_group = case_when(above_median_employees == 1 ~ "above_median_employees", 
                                  above_median_employees == 0 ~ "below_median_employees"),
         expgain_group = case_when(above_median_expected_gain == 1 ~ "above_median_gain",
                                   above_median_expected_gain == 0 ~ "below_median_gain")
  )
base_groups <- c("rem_group", "unantrem_group", "dl_group", "antrem_group")
fintech_fee %<>% 
  create_interaction_group(base_groups, c("fee_type", "fsize_group", "expgain_group")) %>% 
  create_interaction_group("dl_group", c("rem_group", "unantrem_group", "antrem_group")) %>%
  mutate(dl_rem_unantrem_antrem_group = dl_antrem_group) %>%
  mutate(dl_rem_unantrem_antrem_group = ifelse(is.na(dl_rem_unantrem_antrem_group), dl_rem_group, dl_rem_unantrem_antrem_group))


# Define the function to apply hyphenation rules
apply_hyphenation <- function(obs) {
  if (is.na(obs)) {
    return(obs)
  }
  
  # Split the observation around '&'
  parts <- unlist(strsplit(obs, "&"))
  
  # Function to hyphenate if there are three words
  hyphenate <- function(part) {
    words <- unlist(strsplit(trimws(part), " "))
    
    if (length(words) == 3) {
      return(paste(words[1], "-", words[2], " ", words[3], sep = ""))
    } else {
      return(part)
    }
  }
  
  # Apply hyphenation to both parts of the observation
  if (length(parts) == 2) {
    return(paste(hyphenate(parts[1]), "&", " ", hyphenate(parts[2]), sep = ""))
  } else {
    return(hyphenate(parts[1]))
  }
}

remove_double_spaces <- function(text) {
  # Use gsub to replace multiple spaces with a single space
  gsub("\\s{2,}", " ", text)
}

columns_to_modify <- c("rem_fee_group", "rem_fsize_group", "rem_expgain_group",
                       "unantrem_fee_group", "unantrem_fsize_group", "unantrem_expgain_group",
                       "dl_fee_group", "dl_fsize_group", "dl_expgain_group",
                       "antrem_fee_group", "antrem_fsize_group", "antrem_expgain_group",
                       "dl_rem_group", "dl_unantrem_group", "dl_antrem_group")

fintech_fee <- fintech_fee %>%
  mutate(across(all_of(columns_to_modify), ~ sapply(., apply_hyphenation) %>% sapply(remove_double_spaces)))


# (1.11): Load regressions data.
pres_regressions <- qread(file = here("proc", "regs_for_figs.qs"))
for (i in 1:length(pres_regressions)) {
  object <- pres_regressions[[i]]
  assign(names(pres_regressions)[i], object)
}
rm(pres_regressions, object)

########################################################
##    (2): Generate weekly sales and take-up graphs.  ##
########################################################
# (2.1): Monthly sales graphs. (NBER version for the graph)
graph_pretreat_sales(monthly_sales_win_dat, "timestamp_month") +
  guides(color = guide_legend(nrow = 2)) +
  theme(plot.title = element_blank(),
        legend.position = "top", #c(0.1, 0.15))
        legend.margin = margin(l = -360),
        legend.text = element_text(family = "Times New Roman", size = 12),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.text.y = element_text(family = "Times New Roman", size = 12),
        axis.title.y = element_text(family = "Times New Roman", size = 12))
ggsave_(here("results", "figures", "sales_by_month.eps"), width = 7, height = 4)
rm(weekly_sales_win_dat, monthly_sales_win_dat)

# (2.2): Define list of figures to export.
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
    "panel_cum_accepted_offer_beta_dl_unantrem_dl_antrem.eps", #Should it be _updated.eps? #NBER YES
    "panel_cum_accepted_offer_beta_dltreat_antrem.eps", #Should it be _updated.eps? #NBER YES
    "panel_cum_accepted_offer_beta_dlcond_rem_antrem_unantrem.eps",
    "panel_cum_accepted_offer_line_dltreat_antrem_norem.eps",

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

het_files <- het_files %>%
  filter(filename %in% keep_files)

export_files <- het_files$filename 

# (2.3): Cumulative offer take-up line graphs.
# for (gtype in c("paper", "pres", "panel")) {
graph_width <- 9.5
run_again <- 1
while (run_again == 1) {
  if (file.exists(here("proc", "admin_graph_axis_dataset_line.csv"))) {
    run_again <- 0
    axis_dataset <- read_csv(here("proc", "admin_graph_axis_dataset_line.csv"))
  } else {
    axis_dataset <- tribble(~group, ~y_min, ~y_max,
                            "main", 0.18, 0.7,
                            "openemail_rem", 0.18, 0.7,
                            "openemail", 0.18, 0.7,
                            "long", 0.18, 0.7,
                            "interactions", 0.18, 0.7)
  }
  # Graph type
  for (gtype in c("panel", "pres")) {
    # Treatment group
    for (tg in c("unantrem", "rem", "dl", "antrem", "fee")) { #
      # Pooled / heterogeneity
      for (type in c("pooled", "feetype", "dltreat", "fsize", "expgain", "nodl")) {
        # Date
        for (date in c("2020-10-06", "2021-03-31")) {
          # Email type.
          for (em in c(FALSE, TRUE)) {
            # Don't do long-term graphs conditional on email opening
            if (em == TRUE & date == "2021-03-31") {next} # & tg != "dl"

            # Don't do short-term deadline figures
            if (type == "nodl" & date != "2021-03-31") {next}

            # Only prepare email graphs if looking at pooled results
            if (em == TRUE & !(type %in% c("pooled", "feetype"))) {next}

            # Define file names
            current_filename <- str_c("cum_accepted_offer_line_", type, "_", tg)
            # if (date == "2020-10-06") {current_filename %<>% str_c("_short")}
            if (date == "2021-03-31") {current_filename %<>% str_c("_long")}
            if (em) {current_filename %<>% str_c("_openemail")}
            if (!current_filename %in% het_files$objectname) {next}

            # Get processed data for figures
            if (type == "pooled") {
              current_data <- takeup_rate_pooled(tg, open_email = em)
              current_graph <- graph_takeup_rate_pooled(data = current_data,
                                                        grouping = tg,
                                                        datefilter = date)
              # Apply Times New Roman font
              current_graph <- current_graph +
                theme(text = element_text(family = "Times New Roman"))

            } else if (type == "feetype" & tg != "fee") {
              current_data <- takeup_rate_het(str_c(tg, "_fee_group"), open_email = em)
              current_graph <- graph_takeup_rate_het(data = current_data,
                                                     grouping = str_c(tg, "_fee_group"),
                                                     datefilter = date)
              # Apply Times New Roman font
              current_graph <- current_graph +
                theme(text = element_text(family = "Times New Roman"))

            } else if (type == "dltreat" & !(tg %in% c("dl", "fee"))) {
              current_data <- takeup_rate_het(str_c("dl_", tg, "_group"), open_email = em)
              current_graph <- graph_takeup_rate_het(data = current_data,
                                                     grouping = str_c("dl_", tg, "_group"),
                                                     datefilter = date)

              # Apply Times New Roman font
              current_graph <- current_graph +
                theme(text = element_text(family = "Times New Roman"))

            } else if (type %in% c("fsize", "usage", "expgain") & tg != "fee") {
              current_data <- takeup_rate_het(str_c(tg, "_", type, "_group"), open_email = em)
              current_graph <- graph_takeup_rate_het(data = current_data,
                                                     grouping = str_c(tg, "_", type, "_group"),
                                                     datefilter = date)
              # Apply Times New Roman font
              current_graph <- current_graph +
                theme(text = element_text(family = "Times New Roman"))

            } else if (type == "nodl") {
              current_data <- takeup_rate_pooled(tg, no_deadline = TRUE)
              current_graph <- graph_takeup_rate_pooled(data = current_data,
                                                        grouping = tg,
                                                        datefilter = date)
              # Apply Times New Roman font
              current_graph <- current_graph +
                theme(text = element_text(family = "Times New Roman"))
            } else {
              next
            }

            # Change scale depending on date for long-term graphs
            max_point <- current_data %>% filter(date <= date) %>% pull(takeup_rate) %>% max(na.rm = TRUE)
            #max_point <- 0.35
            if (date == "2021-03-31") {
              current_graph %<>% change_scale("1 month", 29.5*max_point/30, 5)
            }

            # Show and export graph
            # Presentation / regular versions
            # # for (pres in c("", "pres_", "panel_")) {
            #   if (pres != "" & date != "2020-10-06") {
            #     next
            #   }

            # Adjust theme for presentation and panel graphs
            if (gtype == "pres") {
              current_graph %<>% adjust_size(type = type)
            } else if (gtype == "panel") {
              current_graph %<>% adjust_size(type = type,
                                             panel_graphs = TRUE)
            }

            # Save graph axes
            if (!exists("axis_dataset_temp")) {
              axis_dataset_temp <- tibble()
            }
            axis_dataset_temp %<>%
              bind_rows(tibble(group = het_files %>% filter(objectname == current_filename) %>% pull(axis_group),
                               y_min = get_axes(current_graph)[1] %>% as.numeric(),
                               y_max = get_axes(current_graph)[2] %>% as.numeric()))

            # Get axes depending on graph group
            current_group <- het_files %>%
              filter(objectname == current_filename) %>%
              pull(axis_group) %>%
              unique()
            current_axes <- axis_dataset %>%
              filter(group == current_group)
            current_breaks <- define_y_axis_breaks(current_axes$y_min, current_axes$y_max)
            current_graph <- current_graph +
              scale_y_continuous(labels = percent_format(accuracy = 1),
                                 breaks = current_breaks,
                                 limits = c(current_breaks[1]*0.98, max(current_breaks)*1.02))
            # Xinyu add note: replication package 0.97 and 1.03
            # limits = c(current_axes$y_min*0.98, current_axes$y_max*1.02))

            # Modify legend position
            if (current_group %in% c("main", "openemail", "openemail_rem") &
                type == "pooled") {
              # (current_group == "main" & type == "pooled")) {
              current_graph <- current_graph + theme(legend.position = c(0.15, 0.75))
            } else if (type %in% c("feetype", "fsize", "expgain", "dltreat")) {
              current_graph <- current_graph +
                theme(legend.justification = "left",
                      legend.position = "top",
                      legend.margin = margin(l = -77, b = -12),
                      legend.box.margin = margin(b = -3),
                      legend.key.height = unit(39, "points"),
                      legend.spacing.y = unit(0, "points"))
            }
            # if (current_group %in% c("main", "openemail", "openemail_rem") &
            #     type == "pooled" & tg %in% c("unantrem", "antrem")) {
            #   current_graph <- current_graph + theme(legend.position = c(0.25, 0.75))
            # }
            
            if (current_group == "long") {
              current_graph <- current_graph +
                theme(legend.justification = "left",
                      legend.position = "top",
                      legend.margin = margin(l = -77, b = -12),
                      legend.box.margin = margin(b = -15),
                      legend.spacing.y = unit(-40, "points"),
                      legend.key.height = unit(75, "points"),
                      legend.key.size = unit(2.1, "cm"))
            }

            # if (current_group %in% c("openemail","openemail_rem") &
            #     type == "pooled" & tg %in% c("rem","antrem","dl")) {
            #   current_graph <- current_graph + scale_y_continuous(labels = percent_format(accuracy = 1),
            #                                                       breaks = c(0.34, 0.43, 0.52, 0.61),
            #                                                       limits = c(0.34*0.98, 0.61*1.02))
            # }
            # Xinyu added above for testing
            
            if (current_group %in% c("openemail","openemail_rem") & tg %in% c("rem","antrem","dl")) {
              current_graph <- current_graph + scale_y_continuous(labels = percent_format(accuracy = 1),
                                                                  breaks = c(0.34, 0.43, 0.52, 0.61),
                                                                  limits = c(0.34*0.98, 0.61*1.02))
            }
            
            # Xinyu's note: 0.97 and 1.03 are added in the replication package
            # Save panel graphs as objects in the environment
            if (gtype == "panel" & str_c(gtype, "_", current_filename, ".eps") %in% export_files) {
              assign(current_filename, current_graph)
            }

            # Save graphs
            ggsave_(here("results", "figures", str_c("panel_", current_filename, ".eps")),
                    width = graph_width, height = 6)
            # ggsave_(here("results", "figures", str_c(current_filename, "_pres.eps")),
            #         width = graph_width, height = graph_width * 0.7)
          }
        }
      }
    }
  }

  # Get min/max axes from all figures by group
  axis_dataset_temp %<>%
    group_by(group) %>%
    summarise(y_min = min(y_min),
              y_max = max(y_max))
  write_csv(axis_dataset_temp, here("proc", "admin_graph_axis_dataset_line.csv"))
  rm(axis_dataset_temp)
}




# Apply the filter and mutate in a single pipe
result <- takeup_rate_het(grouping = c("dl_rem_unantrem_antrem_group"), y_var = "accepted_offer", open_email = TRUE) |>
  filter(date <= "2020-10-06") |>
  mutate(
    date = case_when(
      date == "2020-09-29" ~ 1,
      date == "2020-09-30" ~ 2,
      date == "2020-10-01" ~ 3,
      date == "2020-10-02" ~ 4,
      date == "2020-10-03" ~ 5,
      date == "2020-10-04" ~ 6,
      date == "2020-10-05" ~ 7,
      date == "2020-10-06" ~ 8,
      TRUE ~ as.numeric(date)  # Ensure it's numeric
    )
  ) |>
  mutate(dl_rem_unantrem_antrem_group = factor(dl_rem_unantrem_antrem_group, levels = c("Deadline & no reminder",
                                                                                        "No deadline & no reminder",
                                                                                        "Deadline & unannounced reminder",
                                                                                        "No deadline & unannounced reminder",
                                                                                        "Deadline & announced reminder",
                                                                                        "No deadline & announced reminder")))

adjust_size_new <- function(graph, text_size = 28, legend_size = 28, type = "pooled", panel_graphs = FALSE) {
  if (type %in% c("fsize")) {legend_size <- 27} #"expgain",
  # if (type %in% c("dltreat", "dlcond")) {legend_size <- 23.4}
  output_graph <- graph +
    theme(
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      axis.text.x = element_text(size = text_size),
      axis.text.y = element_text(size = text_size),
      legend.text = element_text(size = legend_size),
      #legend.box.margin = margin(-30, 10, 0, -50))
      legend.box.margin = margin(-15, 10, 0, 10))
  if (panel_graphs) {
    output_graph <- output_graph +
      theme(legend.justification = "left",
            legend.margin = margin(l = -77))
  }
  return(output_graph)
}



plot_obj <- ggplot(result, aes(x = date, y = takeup_rate, color = dl_rem_unantrem_antrem_group, linetype = dl_rem_unantrem_antrem_group)) +
  geom_line(aes(linetype = dl_rem_unantrem_antrem_group), size = 2.4) +
  geom_point(aes(shape = dl_rem_unantrem_antrem_group), size = 6.3) +
  labs(x = "Day", y = "Take-Up") +
  scale_color_manual(values = c("#DDCC77", "#AA4499", "#AA9944", "#771166", "#7A7033", "#550044")) +
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed", "dotted", "dotted")) +
  scale_shape_manual(values = c(16, 17, 16, 17, 16, 17)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:8, labels = 1:8) +
  theme_minimal(base_size = 17) +
  set_theme(legend_position = "top", size = 17, legend_text_size = 17) +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text.x = element_text(hjust = 1, size = 16),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.justification = "left",
    legend.position = "top",
    legend.key.height = unit(20, "points"),
    legend.title = element_blank(),
    legend.direction = "vertical",
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(20, "points"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.text = element_text(margin = margin(t = 5)),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(2.75, "cm"),
    plot.margin = margin(5, 0, 0, 0)
  ) %>%
  adjust_size_new(text_size = 28, legend_size = 28, panel_graphs = TRUE)
plot_obj
# Save the plot as a `.qs` file
qsave(plot_obj, here("proc", "panel_cum_accepted_offer_line_dltreat_antrem_norem.qs"))

# Save the plot as an image file if needed
ggsave_(
  filename = here("results", "figures", "panel_cum_accepted_offer_line_dltreat_antrem_norem.eps"),
  plot = plot_obj,
  width = 9.5,
  height = 6
)



# Apply the filter and mutate in a single pipe
result <- takeup_rate_het(grouping = c("dl_rem_unantrem_antrem_group"), y_var = "accepted_offer", open_email = TRUE) |>
  filter(date <= "2020-10-06") |>
  mutate(
    date = case_when(
      date == "2020-09-29" ~ 1,
      date == "2020-09-30" ~ 2,
      date == "2020-10-01" ~ 3,
      date == "2020-10-02" ~ 4,
      date == "2020-10-03" ~ 5,
      date == "2020-10-04" ~ 6,
      date == "2020-10-05" ~ 7,
      date == "2020-10-06" ~ 8,
      TRUE ~ as.numeric(date)  # Ensure it's numeric
    )
  ) |>
  mutate(dl_rem_unantrem_antrem_group = factor(dl_rem_unantrem_antrem_group, levels = c("Deadline & no reminder", 
                                                                                        "No deadline & no reminder",
                                                                                        "Deadline & unannounced reminder", 
                                                                                        "No deadline & unannounced reminder",
                                                                                        "Deadline & announced reminder",
                                                                                        "No deadline & announced reminder")))

adjust_size_new <- function(graph, text_size = 28, legend_size = 28, type = "pooled", panel_graphs = FALSE) {
  if (type %in% c("fsize")) {legend_size <- 27} #"expgain",
  # if (type %in% c("dltreat", "dlcond")) {legend_size <- 23.4}
  output_graph <- graph +
    theme(
      axis.title.x = element_text(size = text_size),
      axis.title.y = element_text(size = text_size),
      axis.text.x = element_text(size = text_size),
      axis.text.y = element_text(size = text_size),
      legend.text = element_text(size = legend_size),
      #legend.box.margin = margin(-30, 10, 0, -50))
      legend.box.margin = margin(-15, 10, 0, 10))
  if (panel_graphs) {
    output_graph <- output_graph +
      theme(legend.justification = "left",
            legend.margin = margin(l = -77))
  }
  return(output_graph)
}



plot_obj <- ggplot(result, aes(x = date, y = takeup_rate, color = dl_rem_unantrem_antrem_group, linetype = dl_rem_unantrem_antrem_group)) +
  geom_line(aes(linetype = dl_rem_unantrem_antrem_group), size = 2.4) +
  geom_point(aes(shape = dl_rem_unantrem_antrem_group), size = 6.3) +
  labs(x = "Day", y = "Take-Up") +
  scale_color_manual(values = c("#DDCC77", "#AA4499", "#AA9944", "#771166", "#7A7033", "#550044")) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed", "dashed", "dotted", "dotted")) + 
  scale_shape_manual(values = c(16, 17, 16, 17, 16, 17)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = 1:8, labels = 1:8) +
  theme_minimal(base_size = 17) +
  set_theme(legend_position = "top", size = 17, legend_text_size = 17) +
  theme(
    text = element_text(family = "Times New Roman", size = 14),
    axis.text.x = element_text(hjust = 1, size = 16),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.justification = "left",
    legend.position = "top",
    legend.key.height = unit(20, "points"),
    legend.title = element_blank(),
    legend.direction = "vertical",
    legend.spacing.x = unit(0.3, "cm"),
    legend.spacing.y = unit(20, "points"),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.text = element_text(margin = margin(t = 5)),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(2.75, "cm"),
    plot.margin = margin(5, 0, 0, 0)
  ) %>%
  adjust_size_new(text_size = 28, legend_size = 28, panel_graphs = TRUE)
plot_obj
# Save the plot as a `.qs` file
qsave(plot_obj, here("proc", "panel_cum_accepted_offer_line_dltreat_antrem_norem.qs"))

# Save the plot as an image file if needed
ggsave_(
  filename = here("results", "figures", "panel_cum_accepted_offer_line_dltreat_antrem_norem.eps"),
  plot = plot_obj,
  width = 9.5,
  height = 6
)


 

#################################################
##    (3): Generate model coefficient graphs.  ##
#################################################
# (3.1): Define parameters for heterogeneous effect coefficient graphs.
reg_het_parms <- tribble(
  ~reg,                                     ~graph_name, ~treat_group,             ~interaction,                                ~negative_term,                                                  ~positive_term,
  #"reg_cum_daily_het_Y_VAR_unantrem_fee",   "feetype",   "unanticipated_reminder", "unanticipated_reminder:fee_2.75",         "Unannounced-reminder effect for 3.00% offer",                   "Unannounced-reminder effect for 2.75% offer",
  "reg_cum_daily_het_Y_VAR_rem_fee",        "feetype",   "reminder",               "reminder:fee_2.75",                         "Reminder effect for 3.00% offer",                               "Reminder effect for 2.75% offer",
  "reg_cum_daily_het_Y_VAR_antrem_fee",     "feetype",   "anticipated_reminder",   "anticipated_reminder:fee_2.75",             "Announced-reminder effect for 3.00% offer",                     "Announced-reminder effect for 2.75% offer",
  "reg_cum_daily_het_Y_VAR_dl_fee",         "feetype",   "deadline",               "deadline:fee_2.75",                         "Deadline effect for 3.00% offer",                               "Deadline effect for 2.75% offer",
  #"reg_cum_daily_het_Y_VAR_fsize_unantrem", "fsize",     "unanticipated_reminder", "above_median_sales:unanticipated_reminder", "Unannounced-reminder effect | below-median sales",            "Unannounced-reminder effect | above-median sales",             
  "reg_cum_daily_het_Y_VAR_fsize_rem",      "fsize",     "reminder",               "above_median_sales:reminder",               "Reminder effect | below-median sales",                          "Reminder effect | above-median sales",             
  "reg_cum_daily_het_Y_VAR_fsize_antrem",   "fsize",     "anticipated_reminder",   "above_median_sales:anticipated_reminder",   "Announced-reminder effect | below-median sales",                "Announced-reminder effect | above-median sales", 
  "reg_cum_daily_het_Y_VAR_fsize_dl",       "fsize",     "deadline",               "above_median_sales:deadline",               "Deadline effect | below-median sales",                          "Deadline effect | above-median sales",
  #"reg_cum_daily_het_Y_VAR_dl_unantrem",    "dltreat",   "unanticipated_reminder", "unanticipated_reminder:deadline",           "Unannounced-reminder effect | no deadline",                   "Unannounced-reminder effect | deadline",
  "reg_cum_daily_het_Y_VAR_dl_rem",         "dltreat",   "reminder",               "deadline:reminder",                         "Reminder effect | no deadline",                                 "Reminder effect | deadline",
  "reg_cum_daily_het_Y_VAR_dl_antrem",      "dltreat",   "anticipated_reminder",   "anticipated_reminder:deadline",             "Announced vs. unannounced reminder | no deadline",              "Announced vs. unannounced reminder | deadline",
  "reg_cum_daily_het_Y_VAR_dl_rem",         "dlcond",    "deadline",               "deadline:reminder",                         "Deadline effect | no reminder",                                 "Deadline effect | reminder",
  "reg_cum_daily_het_Y_VAR_dl_antrem",      "dlcond",    "deadline",               "anticipated_reminder:deadline",             "Deadline effect | unannounced reminder",                        "Deadline effect | announced reminder",
  "reg_cum_daily_het_Y_VAR_expgain_rem",    "expgain",   "reminder",               "above_median_expected_gain:reminder",       "Reminder effect | below-median gain",                           "Reminder effect | above-median gain",
  "reg_cum_daily_het_Y_VAR_expgain_antrem", "expgain",   "anticipated_reminder",   "above_median_expected_gain:anticipated_reminder","Announced-reminder effect | below-median gain",          "Announced-reminder effect | above-median gain",
  "reg_cum_daily_het_Y_VAR_expgain_dl",     "expgain",   "deadline",               "above_median_expected_gain:deadline",       "Deadline effect | below-median gain",                           "Deadline effect | above-median gain")




# (3.2): Get all pooled and heterogeneous effect coefficient graphs.
run_again <- 1
while (run_again == 1) {
  if (file.exists(here("proc", "admin_graph_axis_dataset_beta.csv"))) {
    run_again <- 0 
    axis_dataset <- read_csv(here("proc", "admin_graph_axis_dataset_beta.csv"))
  } else {
    axis_dataset <- tribble(~group, ~y_min, ~y_max,
                            "main", -0.08, 0.08,
                            "openemail_rem", -0.08, 0.08,
                            "openemail", -0.08, 0.08,
                            "long", -0.08, 0.08,
                            "interactions", -0.08, 0.08)
  }
  # Generate all pooled effect coefficient graphs.
  # Graph type
  for (gtype in c("panel", "pres")) { #"pres",
    # Variables
    for (y_var in c("accepted_offer", "open_email")) { # "open_email", "open_erem"
      # Treatment group
      for (tg in c("rem", "antrem", "dl", "fee", "unantrem")) {
        # Email type
        for (em in c(FALSE, TRUE)) {
          # Define file and object names
          current_reg <- str_c("reg_cum_daily_pooled_", y_var, "_", tg)
          current_filename <- str_c("cum_", y_var, "_beta_pooled_", tg)
          if (em) {
            current_reg %<>% str_c("_openemail")
            current_filename %<>% str_c("_openemail")
          }
          if (!exists(current_reg)) {next}
          if (!current_filename %in% het_files$objectname) {next}

          # Generate figure
          current_graph <- graph_treat_effect_pooled(eval(as.name(current_reg)), tg)
          current_graph <- current_graph +
            theme(
              text = element_text(family = "Times New Roman")
            ) 
          
          # Adjust theme for presentation and panel graphs
          if (gtype == "pres") {
            current_graph %<>% adjust_size(type = type)
          } else if (gtype == "panel") {
            current_graph %<>% adjust_size(text_size = 28, legend_size = 28, panel_graphs = TRUE)
          }

          # Save graph axes
          if (!exists("axis_dataset_temp")) {
            axis_dataset_temp <- tibble()
          }
          axis_dataset_temp %<>%
            bind_rows(tibble(group = het_files %>% filter(objectname == current_filename) %>% pull(axis_group),
                             y_min = get_axes(current_graph)[1] %>% as.numeric(),
                             y_max = get_axes(current_graph)[2] %>% as.numeric()))
          
          # Get axes depending on graph group
          current_axes <- axis_dataset %>%
            filter(group == (het_files %>%
                               filter(objectname == current_filename) %>%
                               pull(axis_group) %>%
                               unique()))

          # current_breaks <- c(seq(0, current_axes$y_max, 0.02),
          #                     seq(0, current_axes$y_min, -0.02)) %>%
          #   unique() %>%
          #   sort()
          current_breaks <- define_y_axis_breaks_beta(current_axes$y_min, current_axes$y_max)
          current_graph <- current_graph +
            scale_y_continuous(labels = function(x) x * 100,
                               limits = c(current_axes$y_min*1.02, current_axes$y_max*1.02),
                               breaks = current_breaks)
            # scale_y_continuous(
            #   breaks = seq(-0.04, 0.06, by = 0.02),
            #   labels = scales::label_number(accuracy = 1, scale = 100))
            
          # current_graph
          # print(current_graph)

           if (em && y_var %in% c("accepted_offer") & tg %in% c("rem", "antrem", "dl")) {
            current_graph <- current_graph + scale_y_continuous(labels = function(x) x * 100,
                                                                breaks = c(-0.04, -0.02, 0.00, 0.02, 0.04, 0.06, 0.08),
                                                                limits = c(-0.04*1.02, 0.09))}

          
          # Save panel graphs as objects in the environment
          if (gtype == "panel" & str_c(gtype, "_", current_filename, ".eps") %in% export_files) {
            assign(current_filename, current_graph)
          }

          # Save graphs
          # ggsave_(here("results", "figures", str_c(gtype, "_", current_filename, ".eps")),
          #         width = 9.5, height = ifelse(gtype == "pres", 9.5 * 0.7, 6))
          # Save graphs
          ggsave_(here("results", "figures", str_c("panel_", current_filename, ".eps")), 
                  width = 9.5, height = 6)
          # ggsave_(here("results", "figures", str_c(current_filename, "_pres.eps")), 
          #         width = 9.5, height = 9.5 * 0.7)
        }
      }
    }
  }
  # Generate all heterogeneous effect coefficient graphs.
  # Graph type
  for (gtype in c("panel", "pres")) { #"pres", 
    # Variables
    for (y_var in c("accepted_offer", "open_email")) { #"open_email", "open_erem"
      for (i in 1:nrow(reg_het_parms)) {
        # Email type
        for (em in c(FALSE, TRUE)) {
          # Define file and object names
          current_reg <- str_replace(reg_het_parms$reg[i], "Y_VAR", y_var)
          tg <- case_when(str_detect(reg_het_parms$interaction[i], "unanticipated") ~ "unantrem",
                          str_detect(reg_het_parms$interaction[i], "anticipated") ~ "antrem",
                          str_detect(reg_het_parms$interaction[i], "reminder") ~ "rem", 
                          TRUE ~ "dl")
          current_filename <- str_c("cum_", y_var, "_beta_", reg_het_parms$graph_name[i], "_", tg)
          if (em) {
            current_reg %<>% str_c("_openemail")
            current_filename %<>% str_c("_openemail")
          }
          if (!exists(current_reg)) {next}
          if (!current_filename %in% het_files$objectname) {next}
          
          # Generate figure
          current_graph <- graph_treat_effect_het(reg = eval(as.name(current_reg)),
                                                  treat_group = reg_het_parms$treat_group[i],
                                                  interaction = reg_het_parms$interaction[i],
                                                  negative_term = reg_het_parms$negative_term[i],
                                                  positive_term = reg_het_parms$positive_term[i])
          current_graph <- current_graph +
            theme(
              text = element_text(family = "Times New Roman")
            )
          
          # Adjust theme for presentation and panel graphs
          if (gtype == "pres") {
            current_graph %<>% adjust_size(type = type)
          } else if (gtype == "panel") {
            current_graph %<>% 
              adjust_size(text_size = 28, legend_size = 28, type = reg_het_parms$graph_name[i], panel_graphs = TRUE)
              # theme(legend.key.height = unit(10, "points"))
          }
          # Modify legend position
          current_graph <- current_graph + 
            theme(legend.position = "top",
                  legend.justification = "left",
                  legend.margin = margin(l = -77, b = -12),
                  legend.box.margin = margin(b = -3),
                  legend.key.height = unit(39, "points"))
          
          # Save graph axes
          if (!exists("axis_dataset_temp")) {
            axis_dataset_temp <- tibble() 
          }
          axis_dataset_temp %<>%
            bind_rows(tibble(group = het_files %>% filter(objectname == current_filename) %>% pull(axis_group),
                             y_min = get_axes(current_graph)[1] %>% as.numeric(),
                             y_max = get_axes(current_graph)[2] %>% as.numeric()))
          
          # Get axes depending on graph group
          current_axes <- axis_dataset %>% 
            filter(group == (het_files %>% 
                               filter(objectname == current_filename) %>% 
                               pull(axis_group) %>% 
                               unique()))
          # current_breaks <- c(seq(0, current_axes$y_max, 0.02),
          #                     seq(0, current_axes$y_min, -0.02)) %>% 
          #   unique() %>% 
          #   sort()
          current_breaks <- define_y_axis_breaks_beta(current_axes$y_min, current_axes$y_max)
          current_graph <- current_graph + 
            scale_y_continuous(labels = function(x) x * 100,
                               limits = c(current_axes$y_min*1.02, current_axes$y_max*1.02),
                               breaks = current_breaks)
          current_graph
          
          # Save panel graphs as objects in the environment
          if (gtype == "panel" & str_c(gtype, "_", current_filename, ".eps") %in% export_files) {
            assign(current_filename, current_graph)
          }
          
          # Save graphs
          # ggsave_(here("results", "figures", str_c(gtype, "_", current_filename, ".eps")), 
          #         width = 9.5, height = ifelse(gtype == "pres", 9.5 * 0.7, 6))
          # Save graphs
          ggsave_(here("results", "figures", str_c("panel_", current_filename, ".eps")), 
                  width = 9.5, height = 6)
          # ggsave_(here("results", "figures", str_c(current_filename, "_pres.eps")), 
          #         width = 9.5, height = 9.5 * 0.7)
          
        }
      }
    }
  }
  # Get min/max axes from all figures by group
  axis_dataset_temp %<>%
    group_by(group) %>% 
    summarise(y_min = min(y_min),
              y_max = max(y_max))
  write_csv(axis_dataset_temp, here("proc", "admin_graph_axis_dataset_beta.csv"))
  rm(axis_dataset_temp)
}



# reg_final <- reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem

# Function to extract coefficients and confidence intervals
get_coef_cis_three_model <- function(model, date, var_comb) {
  model_proc <- model[[date]]
  raw <- model_proc %>% 
    glht(str_c(var_comb, "= 0")) %>% 
    confint()
  raw <- raw[["confint"]]
  output <- tribble(
    ~date, ~coef, ~lci, ~uci, 
    mdy(paste(date, 2020)), raw[1], raw[2], raw[3]
  )
  return(output)
}

get_coef_cis_three_model <- function(model, date, var_comb) {
  model_proc <- model[[date]]
  
  # Extract the number of observations and parameters from the feols model
  n <- model_proc$nobs       # Total number of observations
  k <- length(coef(model_proc))  # Number of estimated parameters
  
  # Calculate degrees of freedom
  df <- n - k
  
  # Perform the General Linear Hypothesis Test
  glht_result <- model_proc %>% 
    glht(str_c(var_comb, "= 0"))
  
  # Compute the confidence intervals with t-distribution
  conf_int <- confint(glht_result, calpha = qt(0.975, df))
  
  # Extract confidence interval values
  raw <- conf_int[["confint"]]
  
  # Create a tibble with the results
  output <- tribble(
    ~date, ~coef, ~lci, ~uci, 
    mdy(paste(date, 2020)), raw[1], raw[2], raw[3]
  )
  
  return(output)
}

graph_treat_effect_het_three_models <- function(
    reg_final,
    term_no_reminder, term_ant_reminder, term_unant_reminder,
    psize = 6.3, lsize = 2.4, current_cols = c("#DDCC77", "#AA9944", "#7A7033")
) {
  # Define the dates for which you want to extract coefficients
  dates <- c("Sep 29", "Sep 30", "Oct 1", "Oct 2", "Oct 3", "Oct 4", "Oct 5", "Oct 6")
  
  # Extract coefficients and confidence intervals for each reminder condition
  current_graph_data <- bind_rows(
    # No reminder
    lapply(dates, function(date) get_coef_cis_three_model(reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem, date, "deadline")) %>%
      bind_rows() %>%
      mutate(term = term_no_reminder),
    
    # Anticipated reminder
    lapply(dates, function(date) {
      get_coef_cis_three_model(reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem, date, "deadline + anticipated_reminder:deadline")
    }) %>%
      bind_rows() %>%
      mutate(term = term_ant_reminder),
    
    # Unanticipated reminder
    lapply(dates, function(date) {
      get_coef_cis_three_model(reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem, date, "deadline + unanticipated_reminder:deadline")
    }) %>%
      bind_rows() %>%
      mutate(term = term_unant_reminder)
  )
  
  # Convert date to character to avoid conversion issues
  current_graph_data$date <- as.character(current_graph_data$date)
  
  # Set the order of the terms explicitly
  current_graph_data$term <- factor(current_graph_data$term, 
                                    levels = c(term_no_reminder, term_unant_reminder, term_ant_reminder))
  
  current_graph_data <- current_graph_data %>%
    mutate(date = ifelse(date == "2020-09-29", 1, date),
           date = ifelse(date == "2020-09-30", 2, date),
           date = ifelse(date == "2020-10-01", 3, date),
           date = ifelse(date == "2020-10-02", 4, date),
           date = ifelse(date == "2020-10-03", 5, date),
           date = ifelse(date == "2020-10-04", 6, date),
           date = ifelse(date == "2020-10-05", 7, date),
           date = ifelse(date == "2020-10-06", 8, date))
  
  # Generate the graph
  current_graph <- ggplot(current_graph_data, aes(x = date, y = coef, color = term)) +
    # Line and points
    geom_hline(yintercept = 0) + 
    geom_point(position = position_dodge(width = 0.5), size = psize) + 
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, size = lsize, 
                  position = position_dodge(width = 0.5)) + 
    
    # Axis labels
    xlab("Day") +
    ylab("Treatment Effect (pp)") +
    
    # Axis breaks
    #scale_x_discrete(name = "Day", breaks = dates, labels = 1:length(dates)) +  # Ensure breaks are set correctly
    #scale_x_discrete(c(1,2,3,4,5,6,7,8)) +
    #scale_y_continuous(labels = label_number(accuracy = 1, scale = 100)) +
    scale_y_continuous(
      limits = c(-0.04, 0.06),  # Set y-axis limits from -4 to 6
      labels = label_number(accuracy = 1, scale = 100)
    ) +
    
    # Other formatting
    scale_color_manual(name = "", values = current_cols) + 
    theme_minimal(base_size = 17) +
    set_theme(legend_position = "top", size = 17, legend_text_size = 17) +
    theme(
      text = element_text(family = "Times New Roman", size = 14),
      axis.text.x = element_text(hjust = 1, size = 16),
      axis.title.y = element_text(margin = margin(r = 15)),
      legend.justification = "left",
      legend.position = "top",
      legend.margin = margin(l = -77),
      legend.box.margin = margin(b = -3),
      legend.key.height = unit(20, "points"),
      legend.title = element_blank(),
      legend.direction = "vertical",
      legend.spacing.x = unit(0.3, "cm"), 
      legend.spacing.y = unit(20, "points"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.text = element_text(margin = margin(t = 5)),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(2.75, "cm"),
      plot.margin = margin(5, 0, 0, 0)
    )
  current_graph %<>% adjust_size(text_size = 28, legend_size = 28, panel_graphs = FALSE)
  return(current_graph)
}

# Example usage:
# Replace 'reg_no_rem', 'reg_ant_rem', and 'reg_unant_rem' with your actual models.
graph <- graph_treat_effect_het_three_models(
  reg_final = reg_cum_daily_het_accepted_offer_dl_rem_antrem_unantrem,
  term_no_reminder = "Deadline effect | no reminder",
  term_ant_reminder = "Deadline effect | announced reminder",
  term_unant_reminder = "Deadline effect | unannounced reminder"
)
graph

ggsave_(here("results", "figures", str_c("panel_cum_accepted_offer_beta_dlcond_rem_antrem_unantrem.eps")), 
        width = 9.5, height = 6)

# Save the plot as a `.qs` file
qsave(graph, here("proc", "panel_cum_accepted_offer_beta_dlcond_rem_antrem_unantrem.qs"))


get_coef_cis_four_model <- function(model, date, var_comb) {
  model_proc <- model[[date]]
  raw <- model_proc %>% 
    glht(linfct = str_c(var_comb, " = 0")) %>% 
    confint()
  raw <- raw[["confint"]]
  output <- tribble(
    ~date, ~coef, ~lci, ~uci, 
    mdy(paste(date, 2020)), raw[1], raw[2], raw[3]
  )
  return(output)
}

graph_treat_effect_het_four_models <- function(
    reg_dl_unantrem, reg_dl_antrem,
    term_unantrem_nodl, term_unantrem_dl, term_antrem_nodl, term_antrem_dl,
    psize = 6.3, lsize = 2.4, current_cols = c("#CC6677", "#993344", "#88CCEE", "#5599BB")
) {
  dates <- c("Sep 29", "Sep 30", "Oct 1", "Oct 2", "Oct 3", "Oct 4", "Oct 5", "Oct 6")
  
  current_graph_data <- bind_rows(
    lapply(dates, function(date) get_coef_cis_four_model(reg_dl_unantrem, date, "unanticipated_reminder")) %>%
      bind_rows() %>%
      mutate(term = term_unantrem_nodl),
    
    lapply(dates, function(date) {
      get_coef_cis_four_model(reg_dl_unantrem, date, "unanticipated_reminder + unanticipated_reminder:deadline")
    }) %>%
      bind_rows() %>%
      mutate(term = term_unantrem_dl),
    
    lapply(dates, function(date) {
      get_coef_cis_four_model(reg_dl_antrem, date, "anticipated_reminder")
    }) %>%
      bind_rows() %>%
      mutate(term = term_antrem_nodl),
    
    lapply(dates, function(date) {
      get_coef_cis_four_model(reg_dl_antrem, date, "anticipated_reminder + anticipated_reminder:deadline")
    }) %>%
      bind_rows() %>%
      mutate(term = term_antrem_dl)
  )
  
  current_graph_data$date <- as.character(current_graph_data$date)
  
  current_graph_data$term <- factor(current_graph_data$term, 
                                    levels = c(term_unantrem_dl, term_unantrem_nodl, term_antrem_dl, term_antrem_nodl))
  
  current_graph_data <- current_graph_data %>%
    mutate(date = ifelse(date == "2020-09-29", 1, date),
           date = ifelse(date == "2020-09-30", 2, date),
           date = ifelse(date == "2020-10-01", 3, date),
           date = ifelse(date == "2020-10-02", 4, date),
           date = ifelse(date == "2020-10-03", 5, date),
           date = ifelse(date == "2020-10-04", 6, date),
           date = ifelse(date == "2020-10-05", 7, date),
           date = ifelse(date == "2020-10-06", 8, date))
  
  current_graph <- ggplot(current_graph_data, aes(x = date, y = coef, color = term)) +
    geom_hline(yintercept = 0) + 
    geom_point(position = position_dodge(width = 0.5), size = psize) + 
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, size = lsize, 
                  position = position_dodge(width = 0.5)) + 
    xlab("Day") +
    ylab("Treatment Effect (pp)") +
    scale_y_continuous(
      breaks = seq(-0.04, 0.06, by = 0.02),
      labels = label_number(accuracy = 1, scale = 100),
      expand = expansion(mult = c(0.1, 0))
    ) +
    scale_color_manual(name = "", values = current_cols) + 
    theme_minimal(base_size = 17) +
    set_theme(legend_position = "top", size = 17, legend_text_size = 17) +
    theme(
      text = element_text(family = "Times New Roman", size = 14),
      axis.text.x = element_text(hjust = 1, size = 16),
      axis.title.y = element_text(margin = margin(r = 15)),
      legend.justification = "left",
      legend.position = "top",
      legend.margin = margin(l = -77),
      legend.box.margin = margin(b = -3),
      legend.key.height = unit(20, "points"),
      legend.title = element_blank(),
      legend.direction = "vertical",
      legend.spacing.x = unit(0.3, "cm"), 
      legend.spacing.y = unit(20, "points"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.text = element_text(margin = margin(t = 5)),
      legend.key = element_rect(fill = NA, color = NA),
      legend.key.size = unit(2.75, "cm"),
      plot.margin = margin(5, 0, 0, 0)
    )
  
  current_graph %<>% adjust_size(text_size = 28, legend_size = 28, panel_graphs = FALSE)
  return(current_graph)
}



graph <- graph_treat_effect_het_four_models(
  reg_dl_unantrem = reg_cum_daily_het_accepted_offer_dl_unantrem,
  reg_dl_antrem = reg_cum_daily_het_accepted_offer_dl_antrem_no_reminder,
  term_unantrem_dl = "Unannounced vs. no reminder | deadline",
  term_unantrem_nodl = "Unannounced vs. no reminder | no deadline",
  term_antrem_dl = "Announced vs. no reminder | deadline",
  term_antrem_nodl = "Announced vs. no reminder | no deadline"
)
graph
ggsave_(here("results", "figures", str_c("panel_cum_accepted_offer_beta_dl_unantrem_dl_antrem.eps")), 
        width = 9.5, height = 6)

# Save the plot as a `.qs` file
qsave(graph, here("proc", "panel_cum_accepted_offer_beta_dl_unantrem_dl_antrem.qs"))




# (3.4): Export all figures.
export_figs <- ls()[c(ls() %in% (str_remove(het_files$filename, "panel_") %>% str_remove(".eps")))]
export_figs %<>% 
  lapply(function(name) get(name)) %>% 
  set_names(export_figs)
qsave(export_figs, here("proc", "graphs_admin_panel.qs"))





# #### Keep only the necessary .eps files ####
# # 1. Path to the figures folder within the project
# figures_folder <- file.path("results", "figures")
# 
# # 2. Specify which panel files to keep (include extensions)
# # Edit this vector with the exact filenames (starting with "panel_") you want to retain:
# keep_panels <- c(
#   # Figure 8
#   "panel_cum_accepted_offer_beta_feetype_rem.eps", 
#   "panel_cum_accepted_offer_line_feetype_rem.eps", 
#   "panel_cum_accepted_offer_beta_feetype_antrem.eps", 
#   "panel_cum_accepted_offer_line_feetype_antrem.eps", 
#   "panel_cum_accepted_offer_beta_feetype_dl.eps", 
#   "panel_cum_accepted_offer_line_feetype_dl.eps", 
#   
#   # Figure 7
#   "panel_cum_accepted_offer_beta_fsize_rem.eps", 
#   "panel_cum_accepted_offer_line_fsize_rem.eps", 
#   "panel_cum_accepted_offer_beta_fsize_antrem.eps", 
#   "panel_cum_accepted_offer_line_fsize_antrem.eps", 
#   "panel_cum_accepted_offer_beta_fsize_dl.eps", 
#   "panel_cum_accepted_offer_line_fsize_dl.eps", 
#   
#   # Figure C.9
#   "panel_cum_accepted_offer_beta_expgain_rem.eps", 
#   "panel_cum_accepted_offer_line_expgain_rem.eps", 
#   "panel_cum_accepted_offer_beta_expgain_antrem.eps", 
#   "panel_cum_accepted_offer_line_expgain_antrem.eps", 
#   "panel_cum_accepted_offer_beta_expgain_dl.eps", 
#   "panel_cum_accepted_offer_line_expgain_dl.eps", 
#   
#   # Figure C.11
#   "panel_cum_accepted_offer_beta_dltreat_rem.eps", 
#   "panel_cum_accepted_offer_line_dltreat_rem.eps", 
#   
#   # Figure C.14 (there should be two more figures here)
#   "panel_cum_accepted_offer_beta_dl_unantrem_dl_antrem.eps", #Should it be _updated.eps? #NBER YES
#   "panel_cum_accepted_offer_beta_dltreat_antrem.eps", #Should it be _updated.eps? #NBER YES
#   "panel_cum_accepted_offer_beta_dlcond_rem_antrem_unantrem.eps",
#   "panel_cum_accepted_offer_line_dltreat_antrem_norem.eps",
#   
#   # Figure C.15
#   "panel_cum_accepted_offer_line_fsize_dl_long.eps", 
#   "panel_cum_accepted_offer_line_feetype_dl_long.eps", 
#   
#   # Figure 5
#   "panel_cum_accepted_offer_beta_pooled_fee.eps", 
#   "panel_cum_accepted_offer_line_pooled_fee.eps", 
#   
#   # Figure 6
#   "panel_cum_accepted_offer_beta_pooled_rem.eps", 
#   "panel_cum_accepted_offer_line_pooled_rem.eps", 
#   "panel_cum_accepted_offer_beta_pooled_antrem.eps", 
#   "panel_cum_accepted_offer_line_pooled_antrem.eps", 
#   "panel_cum_accepted_offer_beta_pooled_dl.eps", 
#   "panel_cum_accepted_offer_line_pooled_dl.eps", 
#   
#   # Figure C.10
#   "panel_cum_accepted_offer_beta_pooled_unantrem.eps", 
#   "panel_cum_accepted_offer_line_pooled_unantrem.eps", 
#   
#   #Figure C.8
#   "panel_cum_accepted_offer_line_nodl_fee_long.eps", 
#   "panel_cum_accepted_offer_line_pooled_rem_long.eps", 
#   "panel_cum_accepted_offer_line_pooled_antrem_long.eps", 
#   "panel_cum_accepted_offer_line_pooled_dl_long.eps", 
#   
#   # Figure C.12
#   "panel_cum_accepted_offer_beta_pooled_rem_openemail.eps", 
#   "panel_cum_accepted_offer_line_pooled_rem_openemail.eps", 
#   "panel_cum_accepted_offer_beta_pooled_antrem_openemail.eps", 
#   "panel_cum_accepted_offer_line_pooled_antrem_openemail.eps", 
#   "panel_cum_accepted_offer_beta_pooled_dl_openemail.eps", 
#   "panel_cum_accepted_offer_line_pooled_dl_openemail.eps" 
# 
#   
# )
# 
# # 3. List all files starting with "panel_"
# panel_files <- list.files(path = figures_folder, pattern = "^panel_", full.names = TRUE)
# 
# # 4. Create full paths for files to keep
# files_to_keep <- file.path(figures_folder, keep_panels)
# 
# # 5. Identify panel files to remove
# files_to_remove <- setdiff(panel_files, files_to_keep)
# 
# # 6. Delete unwanted panel files
# if (length(files_to_remove) > 0) {
#   removed <- file.remove(files_to_remove)
#   message(length(files_to_remove), " panel files removed.")
# } else {
#   message("No panel files to remove. All specified panels are kept.")
# }
# 
# # 7. Summary report
# message("Kept panel files: ", paste(basename(files_to_keep), collapse = ", "))
# message("Removed panel files: ", paste(basename(files_to_remove), collapse = ", "))




