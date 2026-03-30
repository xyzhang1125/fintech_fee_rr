#--------------------------------------------------------------------------------------------
# File name: 		      16a_flowchart.R
# Creation date:      
# Author:          		Waldo Ojeda
# Files used:
#   - here("proc", "fintech_fee.csv"))
# Files created:
#   - here("results","figures","experimental_design_flowchart_paper_portrait_orientation.eps")
# Purpose:
# 	- Figure 2: Experimental Design: RCT flowchart.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library("ggplot2")
library("dplyr")
library("magrittr") #library("assertive")
library("ggtext")
library("stringr")
library("extrafont")
library("Cairo")
library("here")
#########################################################

# Bring in data with sample sizes
experiment_randomization <- read.csv(here("proc","fintech_fee.csv"))

# Get total N
n_total <- experiment_randomization %>% count()
n_total <- as.numeric(n_total)

# Get N by Offer or No Offer
experiment_randomization %<>% mutate(control = ifelse(treat_type=="T1",1,0))
n_byoffer_or_nooffer <- experiment_randomization %>% group_by(control) %>% count() %>% ungroup()
n_nooffer <- as.numeric(n_byoffer_or_nooffer %>% filter(control==1) %>% select("n"))
n_offer <- as.numeric(n_byoffer_or_nooffer %>% filter(control==0) %>% select("n"))
## Make sure N by Offer or No Offer add to total N
#assert_is_identical_to_true(n_nooffer+n_offer==n_total)

# Get N by No Deadline, Deadline and Same-day Deadline
## Filter out control
experiment_randomization_nocontrol <- experiment_randomization %>% filter(treat_type!="T1") %>%
                                        select(treat_type, treat_description, fee_type, group_id, group_description)
experiment_randomization_nocontrol %<>% mutate(deadline_description = str_extract(treat_description, "[^,]+"))
n_bydeadlinetype <- experiment_randomization_nocontrol %>% group_by(deadline_description) %>% count() %>% ungroup()
n_nodeadline <- as.numeric(n_bydeadlinetype %>% filter(deadline_description=="No deadline") %>% select("n"))
n_deadline <- as.numeric(n_bydeadlinetype %>% filter(deadline_description=="Deadline") %>% select("n"))
n_24hdeadline <- as.numeric(n_bydeadlinetype %>% filter(deadline_description=="24-hour deadline") %>% select("n"))

# Get N by Reminder Type below Deadline Type
n_byreminder_deadlinetype <- experiment_randomization_nocontrol %>% group_by(treat_description) %>% count() %>% ungroup()
n_nodeadline_noreminder <- as.numeric(n_byreminder_deadlinetype %>% filter(treat_description=="No deadline, no reminder") %>% select("n"))
n_nodeadline_unannouncedreminder <- as.numeric(n_byreminder_deadlinetype %>% filter(treat_description=="No deadline, unanticipated reminder") %>% select("n"))
n_nodeadline_announcedreminder <- as.numeric(n_byreminder_deadlinetype %>% filter(treat_description=="No deadline, anticipated reminder") %>% select("n"))

n_deadline_noreminder <- as.numeric(n_byreminder_deadlinetype %>% filter(treat_description=="Deadline, no reminder") %>% select("n"))
n_deadline_unannouncedreminder <- as.numeric(n_byreminder_deadlinetype %>% filter(treat_description=="Deadline, unanticipated reminder") %>% select("n"))
n_deadline_announcedreminder <- as.numeric(n_byreminder_deadlinetype %>% filter(treat_description=="Deadline, anticipated reminder") %>% select("n"))

# Get N by Fee Type, below Reminder Type below Deadline Type
n_byfeetype_reminder_deadlinetype <- experiment_randomization_nocontrol %>% group_by(group_description) %>% count() %>% ungroup()
n_275_nodeadline_noreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="No deadline, no reminder with 2.75% offer") %>% select("n"))
n_300_nodeadline_noreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="No deadline, no reminder with 3% offer") %>% select("n"))
n_275_nodeadline_unannouncedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="No deadline, unanticipated reminder with 2.75% offer") %>% select("n"))
n_300_nodeadline_unannouncedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="No deadline, unanticipated reminder with 3% offer") %>% select("n"))
n_275_nodeadline_announcedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="No deadline, anticipated reminder with 2.75% offer") %>% select("n"))
n_300_nodeadline_announcedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="No deadline, anticipated reminder with 3% offer") %>% select("n"))

n_275_deadline_noreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="Deadline, no reminder with 2.75% offer") %>% select("n"))
n_300_deadline_noreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="Deadline, no reminder with 3% offer") %>% select("n"))
n_275_deadline_unannouncedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="Deadline, unanticipated reminder with 2.75% offer") %>% select("n"))
n_300_deadline_unannouncedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="Deadline, unanticipated reminder with 3% offer") %>% select("n"))
n_275_deadline_announcedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="Deadline, anticipated reminder with 2.75% offer") %>% select("n"))
n_300_deadline_announcedreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="Deadline, anticipated reminder with 3% offer") %>% select("n"))

n_275_24hdeadline_noreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="24-hour deadline, no reminder with 2.75% offer") %>% select("n"))
n_300_24hdeadline_noreminder <- as.numeric(n_byfeetype_reminder_deadlinetype %>% filter(group_description=="24-hour deadline, no reminder with 3% offer") %>% select("n"))

# Make flowchart for Presentation
# Settings as text label
#text_size_main <- "10pt"
text_size <- "9.8pt"
text_size_main <- "11pt"
l_width <- 0.2
main_padding <-
feerow_padding <- 0.1
#font_type <- "Times"
font_type <- ""

options(repr.plot.width = 5, repr.plot.height =5, repr.plot.res = 300) 
expdesign <- ggplot() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        text = element_text(family = "Times")) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.9, yend = 0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = -6.5, y = 0.65, yend = 0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.65, yend = 0.5, size = l_width) +
  annotate(geom = "segment", x = -6.5, xend = -6.5, y = 0.65, yend = 0.5, size = l_width) +
  # After No Offer/Offer
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.45, yend = 0.25, size = l_width) +
  annotate(geom = "segment", x = -2, xend = 14.8, y = 0.25, yend = 0.25, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y = 0.25, yend = 0.15, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = 0.25, yend = 0.15, size = l_width) +
  annotate(geom = "segment", x = 14.8, xend = 14.8, y = 0.25, yend = -0.65, size = l_width) +
  # After No Deadline/Deadline
  annotate(geom = "segment", x = -2, xend = -2, y = 0, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = 0, yend = -0.125, size = l_width) +
  #annotate(geom = "segment", x = 16.5, xend = 16.5, y = 0, yend = -0.125, size = 1) +
  annotate(geom = "segment", x = -5.2, xend = 1.2, y = -0.125, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 11.2, y = -0.125, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = -5.2, xend = -5.2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 1.2, xend = 1.2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 11.2, xend = 11.2, y = -0.125, yend = -0.28, size = l_width) +
  # After Reminders Section
  ## First vertical lines
  annotate(geom = "segment", x = -5.2, xend = -5.2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 1.2, xend = 1.2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 11.2, xend = 11.2, y =  -0.45, yend = -0.65, size = l_width) +
  ## Horizontal lines
  annotate(geom = "segment", x = -6.0, xend = -4.4, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = -2.8, xend = -1.2, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 0.4, xend = 2.0, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 4.0, xend = 5.6, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 7.2, xend = 8.8, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 10.4, xend = 12.0, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 14.0, xend = 15.6, y =  -0.65, yend = -0.65, size = l_width) +
  ## Second split vertical lines
  annotate(geom = "segment", x = -6.0, xend = -6.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -4.4, xend = -4.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -2.8, xend = -2.8, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -1.2, xend = -1.2, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 0.4, xend = 0.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 2.0, xend = 2.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 4.0, xend = 4.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 5.6, xend = 5.6, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 7.2, xend = 7.2, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 8.8, xend = 8.8, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 10.4, xend = 10.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 12.0, xend = 12.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 14.0, xend = 14.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 15.6, xend = 15.6, y =  -0.65, yend = -0.85, size = l_width) +
  # annotate(geom = "segment", x = 0.88, xend = 0.36, y = 0, yend = 0, size = 2) +
  # annotate(geom = "segment", x = 0.88, xend = 0.36, y = -0.425, yend = -0.425, size = 2) +
  annotate(geom = "richtext", x = 4.8, y = 0.98, hjust = "middle", vjust = "top",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Experimental<br>Sample**</span><br>",
                          "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_total, big.mark=",")),"</span>"),
                          ) +
  annotate(geom = "richtext", x = 4.8, y = 0.45, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Offer**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_offer, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -6.5, y = 0.45, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Control: No Offer**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nooffer, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -2, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 8, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 14.8, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Same-day Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_24hdeadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -5.2, y = -0.37, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_noreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Unannounced<br>Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_unannouncedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 1.2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Announced<br>Reminder**</span>",
                          "<br>","<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_announcedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 4.8, y = -0.37, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_noreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 8, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Unannounced<br>Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_unannouncedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 11.2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Announced<br>Reminder**</span>",
                          "<br>","<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_announcedreminder, big.mark=",")),"</span>")) +
  # Fees under No Deadline/Reminders
  annotate(geom = "richtext", x = -6.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_nodeadline_noreminder, big.mark=",")),"</span>"),
                          label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -4.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_nodeadline_noreminder, big.mark=",")),"</span>"),
                          label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -2.8, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_nodeadline_unannouncedreminder, big.mark=",")),"</span>"),
                          label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -1.2, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_nodeadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 0.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_nodeadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 2.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_nodeadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  # Fees under Deadline/Reminders
  annotate(geom = "richtext", x = 4.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_deadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 5.6, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_deadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 7.2, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_deadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 8.8, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_deadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 10.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_deadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 12.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = " , as.character(format(n_300_deadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 14.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_24hdeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 15.6, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = " , as.character(format(n_300_24hdeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  xlim(-7, 15.7) + ylim(-1, 1) + theme_void() #+ theme(plot.margin = margin(t=0,r=0,b=0,l=3, "cm"))
expdesign

# Save flowchart figures for presentation version
# ggsave(here("results","figures","experimental_design_flowchart_pres.png"),
#         plot = expdesign, width = 9, height = 4, scale = 1.2)
# 
# ggsave(here("results","figures","experimental_design_flowchart_pres.eps"),
#        plot = expdesign, width = 9, height = 4, scale = 1.2, device = cairo_ps)


# Make flowchart for Paper

# Settings as text label
#text_size_main <- "10pt"
text_size <- "11pt"
text_size_main <- "11pt"
l_width <- 0.2
#main_padding <-
feerow_padding <- 0.1
font_type <- "Times"
#font_type <- ""

# Make flowchart for Presentation
options(repr.plot.width = 5, repr.plot.height =5, repr.plot.res = 300) 
expdesign_paper <- ggplot() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        text = element_text(family = "Times")) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.9, yend = 0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = -6.5, y = 0.65, yend = 0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.65, yend = 0.5, size = l_width) +
  annotate(geom = "segment", x = -6.5, xend = -6.5, y = 0.65, yend = 0.5, size = l_width) +
  # After No Offer/Offer
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.45, yend = 0.25, size = l_width) +
  annotate(geom = "segment", x = -2, xend = 14.8, y = 0.25, yend = 0.25, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y = 0.25, yend = 0.15, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = 0.25, yend = 0.15, size = l_width) +
  annotate(geom = "segment", x = 14.8, xend = 14.8, y = 0.25, yend = -0.65, size = l_width) +
  # After No Deadline/Deadline
  annotate(geom = "segment", x = -2, xend = -2, y = 0, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = 0, yend = -0.125, size = l_width) +
  #annotate(geom = "segment", x = 16.5, xend = 16.5, y = 0, yend = -0.125, size = 1) +
  annotate(geom = "segment", x = -5.2, xend = 1.2, y = -0.125, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 11.2, y = -0.125, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = -5.2, xend = -5.2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 1.2, xend = 1.2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 11.2, xend = 11.2, y = -0.125, yend = -0.28, size = l_width) +
  # After Reminders Section
  ## First vertical lines
  annotate(geom = "segment", x = -5.2, xend = -5.2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 1.2, xend = 1.2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 11.2, xend = 11.2, y =  -0.45, yend = -0.65, size = l_width) +
  ## Horizontal lines
  annotate(geom = "segment", x = -6.0, xend = -4.4, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = -2.8, xend = -1.2, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 0.4, xend = 2.0, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 4.0, xend = 5.6, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 7.2, xend = 8.8, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 10.4, xend = 12.0, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 14.0, xend = 15.6, y =  -0.65, yend = -0.65, size = l_width) +
  ## Second split vertical lines
  annotate(geom = "segment", x = -6.0, xend = -6.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -4.4, xend = -4.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -2.8, xend = -2.8, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -1.2, xend = -1.2, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 0.4, xend = 0.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 2.0, xend = 2.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 4.0, xend = 4.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 5.6, xend = 5.6, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 7.2, xend = 7.2, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 8.8, xend = 8.8, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 10.4, xend = 10.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 12.0, xend = 12.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 14.0, xend = 14.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 15.6, xend = 15.6, y =  -0.65, yend = -0.85, size = l_width) +
  # annotate(geom = "segment", x = 0.88, xend = 0.36, y = 0, yend = 0, size = 2) +
  # annotate(geom = "segment", x = 0.88, xend = 0.36, y = -0.425, yend = -0.425, size = 2) +
  annotate(geom = "richtext", x = 4.8, y = 0.98, hjust = "middle", vjust = "top",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Experimental<br>Sample**</span><br>",
                          "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_total, big.mark=",")),"</span>"),
  ) +
  annotate(geom = "richtext", x = 4.8, y = 0.45, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Offer**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_offer, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -6.5, y = 0.45, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Control: No Offer**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nooffer, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -2, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 8, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 14.8, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Same-day Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_24hdeadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -5.2, y = -0.37, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_noreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Unannounced<br>Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_unannouncedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 1.2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Announced<br>Reminder**</span>",
                          "<br>","<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_announcedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 4.8, y = -0.37, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_noreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 8, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Unannounced<br>Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_unannouncedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 11.2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Announced<br>Reminder**</span>",
                          "<br>","<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_announcedreminder, big.mark=",")),"</span>")) +
  # Fees under No Deadline/Reminders
  annotate(geom = "richtext", x = -6.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_nodeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -4.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_nodeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -2.8, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_nodeadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -1.2, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_nodeadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 0.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_nodeadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 2.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_nodeadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  # Fees under Deadline/Reminders
  annotate(geom = "richtext", x = 4.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_deadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 5.6, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_deadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 7.2, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_deadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 8.8, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_300_deadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 10.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_deadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 12.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = " , as.character(format(n_300_deadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 14.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = ", as.character(format(n_275_24hdeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 15.6, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = " , as.character(format(n_300_24hdeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  xlim(-7, 15.7) + ylim(-1, 1) + theme_void() #+ theme(plot.margin = margin(t=0,r=0,b=0,l=3, "cm"))
expdesign_paper


# Make flowchart for Paper in Portrait Orientation

# Settings as text label
#text_size_main <- "10pt"
text_size <- "11pt"
text_size_main <- "11pt"
l_width <- 0.2
#main_padding <-
feerow_padding <- 0.1
font_type <- "Times"
#font_type <- ""

# Make flowchart for Presentation
options(repr.plot.width = 5, repr.plot.height =5, repr.plot.res = 300) 
expdesign_paper <- ggplot() +
  theme(plot.background = element_rect(fill = "white", colour = "white"),
        text = element_text(family = "Times")) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.9, yend = 0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = -6.5, y = 0.65, yend = 0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.65, yend = 0.5, size = l_width) +
  annotate(geom = "segment", x = -6.5, xend = -6.5, y = 0.65, yend = 0.5, size = l_width) +
  # After No Offer/Offer
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = 0.45, yend = 0.25, size = l_width) +
  annotate(geom = "segment", x = -2, xend = 14.8, y = 0.25, yend = 0.25, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y = 0.25, yend = 0.15, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = 0.25, yend = 0.15, size = l_width) +
  annotate(geom = "segment", x = 14.8, xend = 14.8, y = 0.25, yend = -0.65, size = l_width) +
  # After No Deadline/Deadline
  annotate(geom = "segment", x = -2, xend = -2, y = 0, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = 0, yend = -0.125, size = l_width) +
  #annotate(geom = "segment", x = 16.5, xend = 16.5, y = 0, yend = -0.125, size = 1) +
  annotate(geom = "segment", x = -5.2, xend = 1.2, y = -0.125, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 11.2, y = -0.125, yend = -0.125, size = l_width) +
  annotate(geom = "segment", x = -5.2, xend = -5.2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 1.2, xend = 1.2, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y = -0.125, yend = -0.28, size = l_width) +
  annotate(geom = "segment", x = 11.2, xend = 11.2, y = -0.125, yend = -0.28, size = l_width) +
  # After Reminders Section
  ## First vertical lines
  annotate(geom = "segment", x = -5.2, xend = -5.2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = -2, xend = -2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 1.2, xend = 1.2, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 4.8, xend = 4.8, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 8, xend = 8, y =  -0.45, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 11.2, xend = 11.2, y =  -0.45, yend = -0.65, size = l_width) +
  ## Horizontal lines
  annotate(geom = "segment", x = -6.0, xend = -4.4, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = -2.8, xend = -1.2, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 0.4, xend = 2.0, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 4.0, xend = 5.6, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 7.2, xend = 8.8, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 10.4, xend = 12.0, y =  -0.65, yend = -0.65, size = l_width) +
  annotate(geom = "segment", x = 14.0, xend = 15.6, y =  -0.65, yend = -0.65, size = l_width) +
  ## Second split vertical lines
  annotate(geom = "segment", x = -6.0, xend = -6.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -4.4, xend = -4.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -2.8, xend = -2.8, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = -1.2, xend = -1.2, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 0.4, xend = 0.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 2.0, xend = 2.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 4.0, xend = 4.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 5.6, xend = 5.6, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 7.2, xend = 7.2, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 8.8, xend = 8.8, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 10.4, xend = 10.4, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 12.0, xend = 12.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 14.0, xend = 14.0, y =  -0.65, yend = -0.85, size = l_width) +
  annotate(geom = "segment", x = 15.6, xend = 15.6, y =  -0.65, yend = -0.85, size = l_width) +
  # annotate(geom = "segment", x = 0.88, xend = 0.36, y = 0, yend = 0, size = 2) +
  # annotate(geom = "segment", x = 0.88, xend = 0.36, y = -0.425, yend = -0.425, size = 2) +
  annotate(geom = "richtext", x = 4.8, y = 0.98, hjust = "middle", vjust = "top",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Experimental<br>Sample**</span><br>",
                          "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_total, big.mark=",")),"</span>"),
  ) +
  annotate(geom = "richtext", x = 4.8, y = 0.45, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Offer**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_offer, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -6.5, y = 0.45, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Control:<br>No Offer**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nooffer, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -2, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 8, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 14.8, y = 0.08, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Same-day<br>Deadline**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_24hdeadline, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -5.2, y = -0.37, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_noreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = -2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Unannounced<br>Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_unannouncedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 1.2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Announced<br>Reminder**</span>",
                          "<br>","<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_nodeadline_announcedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 4.8, y = -0.37, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**No Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_noreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 8, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Unannounced<br>Reminder**</span>",
                          "<br>", "<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_unannouncedreminder, big.mark=",")),"</span>")) +
  annotate(geom = "richtext", x = 11.2, y = -0.4, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**Announced<br>Reminder**</span>",
                          "<br>","<span style='font-size:",text_size_main,";font-family:",font_type,";'>*N* = ", as.character(format(n_deadline_announcedreminder, big.mark=",")),"</span>")) +
  # Fees under No Deadline/Reminders
  annotate(geom = "richtext", x = -6.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_nodeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -4.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_300_nodeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -2.8, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_nodeadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = -1.2, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_300_nodeadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 0.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_nodeadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 2.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_300_nodeadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  # Fees under Deadline/Reminders
  annotate(geom = "richtext", x = 4.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_deadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 5.6, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_300_deadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 7.2, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_deadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 8.8, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_300_deadline_unannouncedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 10.4, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_deadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 12.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>" , as.character(format(n_300_deadline_announcedreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 14.0, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**2.75%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>", as.character(format(n_275_24hdeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  annotate(geom = "richtext", x = 15.6, y = -0.85, hjust = "middle",
           label = paste0("<span style='font-size:",text_size_main,";font-family:",font_type,";'>**3.00%**</span>",
                          "<br>", "<span style='font-size:",text_size,";font-family:",font_type,";'>*N* = <br>" , as.character(format(n_300_24hdeadline_noreminder, big.mark=",")),"</span>"),
           label.padding = unit(feerow_padding, "lines")) +
  xlim(-7, 15.7) + ylim(-1, 1) + theme_void() #+ theme(plot.margin = margin(t=0,r=0,b=0,l=3, "cm"))
expdesign_paper

# Save flowchart figure
ggsave(here("results","figures","experimental_design_flowchart_paper_portrait_orientation.eps"),
       plot = expdesign_paper, width = 9, height = 4, scale = 1.1, device = cairo_ps)



