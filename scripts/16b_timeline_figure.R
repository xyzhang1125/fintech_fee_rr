#--------------------------------------------------------------------------------------------
# File name: 		      16b_timeline_figure.R
# Creation date:      2023-07-12
# Author:          		César Landín
# Files used:
# 	- (None)
# Files created:
#   - here("results", "figures", "experiment_timeline.eps")
# Purpose:
# 	- Figure 3: Timeline: Generate study timeline figure showing the timing of the initial email, reminder, 
#     and deadline for each treatment arm.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
library(cowplot)
library(lubridate)
library(lemon)
library(showtext)
font_add(family = "Times New Roman", regular = "times.ttf") # change from Times to times
showtext_auto()
options(readr.show_col_types = FALSE)
group_colors <- read_csv(here("proc", "colors.csv"))
source(here("scripts", "programs", "output_directory.R"))
#########################################################

###############################################
##  (1): Make timeline figure with ggplot.   ##
###############################################
# (1.1): Define colors for group descriptors and vertical lines.
desc_col <- group_colors %>% 
  filter(group %in% c("no reminder", "anticipated reminder", "unanticipated reminder")) %>% 
  relocate(col2, .before = col1) %>% 
  pivot_longer(col2:col1) %>% 
  arrange(name) %>% 
  select(-name) %>% 
  pull(value)
# Flip the second and third characters
desc_col <- desc_col[c(1, 3, 2, 4:6)]
rem_col <- group_colors %>% 
  filter(group == "reminder") %>% 
  pull(col2)
dl_col <- group_colors %>% 
  filter(group == "deadline") %>% 
  pull(col2)

# (1.2): Define data for timeline.
# timeline_data <- tibble(date = ymd(c("2020-09-29", "2020-09-30", "2020-10-01", "2020-10-02", 
#                                      "2020-10-03", "2020-10-04", "2020-10-05", "2020-10-06")),
#                         treat_group = c("Group 1: Control",
#                                         "Group 2: No deadline, no reminder",
#                                         "Group 3: No deadline, announced reminder",
#                                         "Group 4: No deadline, unannounced reminder",
#                                         "Group 5: Deadline, no reminder",
#                                         "Group 6: Deadline, announced reminder",
#                                         "Group 7: Deadline, unannounced reminder",
#                                         "Group 8: Same-day deadline, no reminder"),
#                         offer_desc = c(NA,
#                                        "Receive offer",
#                                        "Receive offer, informed about future reminder",
#                                        "Receive offer",
#                                        "Receive offer, informed about deadline date",
#                                        "Receive offer, informed about deadline date and future reminder",
#                                        "Receive offer, informed about deadline date",
#                                        "Receive offer, informed about same-day deadline\n    ")) %>% 
timeline_data <- tibble(date = ymd(c("2020-09-29", "2020-09-30", "2020-10-01", "2020-10-02", 
                                     "2020-10-03", "2020-10-04", "2020-10-05", "2020-10-06")),
                        treat_group = c("Control",
                                        "No deadline, no reminder",
                                        "No deadline, unannounced reminder",
                                        "No deadline, announced reminder",
                                        "Deadline, no reminder", 
                                        "Deadline, unannounced reminder",
                                        "Deadline, announced reminder",
                                        "Same-day deadline, no reminder"),
                        offer_desc = c(NA,
                                       "Receive offer",
                                       "Receive offer",
                                       "Receive offer, informed about future reminder",
                                       "Receive offer, informed about deadline date",
                                       "Receive offer, informed about deadline date",
                                       "Receive offer, informed about deadline date and future reminder",
                                       "Receive offer, informed about same-day deadline\n    ")) %>% 
  mutate(treat_group = str_replace(treat_group, fixed(": "), ":\n") %>% 
           str_replace(fixed(", "), ",\n"),
         offer_desc = str_replace(offer_desc, fixed(", "), ",\n") %>% 
           str_replace(fixed(" with"), "\nwith"),
         reminder = ifelse(str_detect(treat_group, "announced reminder"), "Reminder", NA),
         # deadline = case_when(str_detect(treat_group, fixed(":\nDeadline")) |
         #                        str_detect(treat_group, "24") ~ "Deadline",
         #                      TRUE ~ NA),
         deadline = ifelse(str_detect(treat_group, "Deadline"), "Deadline", NA),
         colors = c("gray30", desc_col, "gray20"))

# (1.3): Define timeline.
g1 <- ggplot(data = timeline_data) +
  # Insert vertical lines
  geom_vline(xintercept = as.numeric(as.Date("2020-09-29")), linetype = "dotted", linewidth = 0.6, color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-03")), linetype = "dotted", linewidth = 0.6, color = rem_col) +
  geom_vline(xintercept = as.numeric(as.Date("2020-10-04")), linetype = "dotted", linewidth = 0.6, color = dl_col) +
  
  # Remove X and Y axis labels
  labs(x = NULL, y = NULL) +
  
  # X and Y scales
  scale_x_date(
    breaks = ymd(c("2020-09-29", "2020-09-30", "2020-10-01", "2020-10-02", "2020-10-03", "2020-10-04")),
    expand = expansion(add = c(0.99, 0.5)),
    labels = c("Day 1\nSep 29\n|", "Day 2\nSep 30\n|", "...\n\n  ", 
               "Day 6\nOct 04\n|", "Day 7\nOct 05\n|", "Day 8\nOct 06\n|"),
    # date_breaks = "1 day",
    position = "top"
  ) +
  scale_y_reverse(limits = c(8.5, 0.5)) +   
  
  # Theme and overall design
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # axis.ticks.x = element_line(),
        axis.ticks.x = element_blank(),
        # axis.ticks.length = unit(.25, "cm"),
        axis.line.x = element_line(color = "white"),
        axis.text.x = element_text(size = 11, face = "bold", color = "black", family = "Times New Roman"),
        axis.text.x.top = element_text(margin = margin(b = -27)),
        axis.text.y = element_blank(),
        plot.margin = margin(l = -40),
        text = element_text(family = "Times New Roman")) +
  
  geom_segment(aes(x= ymd("2020-09-29"), xend = ymd("2020-10-04"),
                   y = 0.5, yend = 0.5))+
  geom_rect(aes(xmin = ymd("2020-09-28"), xmax = ymd("2020-10-04"),
                ymin = 0, ymax = 0.4),
            fill = "white", linewidth = 10) +
  
  # Add text
  # Add text with reminder
  geom_label(data = timeline_data,
             aes(x = ymd("2020-10-03"), y = c(1:8), label = reminder),
             fill = "white",
             color = rem_col,
             label.size = NA,
             hjust = 0.5,
             family = "Times New Roman") +
  # Add text with deadline
  geom_label(data = timeline_data,
             aes(x = ymd("2020-10-04"), y = c(1:8), label = deadline),
             fill = "white",
             color = dl_col,
             label.size = NA,
             hjust = 0.5,
             family = "Times New Roman") +
  geom_label(data = timeline_data,
             aes(x = ymd("2020-09-29"), y = 8.45, label = "Deadline"),
             fill = "white",
             color = dl_col,
             label.size = NA,
             hjust = 0.5,
             family = "Times New Roman") +
  # Add text with offer
  geom_label(data = timeline_data,
             inherit.aes = FALSE,
             aes(x = ymd("2020-09-29"), y = c(1:8), label = str_wrap(offer_desc, 23)),
             fill = "white",
             color = "black",
             label.size = NA,
             lineheight = 0.85,
             hjust = 0.5,
             family = "Times New Roman") +
  # Add axis break character
  geom_label(aes(x = ymd("2020-10-01"), y = 0, label = "\\\\"),
             fill = "white",
             color = "black",
             size = 8,
             label.size = NA,
             hjust = 0.5,
             vjust = 1.2,
             family = "Times New Roman") +
  #geom_text(data = timeline_data,
   #         family = "Times New Roman") +
    # Adjust the x-axis limits
  coord_capped_cart(top = c("both"))
g1

# (1.4): Generate object with group description.
g2 <- ggplot() +
  theme_nothing() +
  geom_label(data = timeline_data,
             aes(x = ymd("2020-09-27"), y = 8.5, label = ""),
             fill = "white",
             color = "white",
             label.size = NA,
             hjust = 0.5) +
  geom_text(data = timeline_data,
            aes(x = ymd("2020-09-27"), y = c(1:8), label = treat_group), color = timeline_data$colors,
            hjust = 0,
            lineheight = 0.85,
            fontface = "bold", 
            family = "Times New Roman") +
  scale_y_reverse(limits = c(8.5, 0.5)) + 
  # scale_color_manual(values = timeline_data$colors) +
  theme(plot.margin = margin(l = -115, r = 0))
g2

# (1.5): Put together group descriptors and timeline. 
plot_grid(g2 + scale_y_reverse(expand = expansion(add = c(0.1, 1.3))),
          # NULL,
          g1 + 
            scale_y_reverse(expand = expansion(add = c(0.25, 0.1))),
          nrow = 1,
          align = "h", axis = "ltb",
          rel_widths = c(0.35, 1.2))
ggsave(here("results", "figures", "experiment_timeline.eps"), width = 8.2, height = 5)
