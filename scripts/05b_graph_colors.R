#--------------------------------------------------------------------------------------------
# File name: 		      05b_graph_colors.R
# Creation date:      2022-05-02
# Author:          		César Landín
# Files used:
# 	- (None)
# Files created:
#   - here("proc", "colors.csv")
# Purpose:
# 	- Generate colorblind-friendly color palette for graphs.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(rcartocolor)
library(scales)
source(here("scripts", "programs", "output_directory.R"))
#########################################################

####################################################
##    (1): Generate colorblind-friendly palette.  ##
####################################################
# (1.1): Show colorblind-friendly palette.
safe_cols <- carto_pal(12, "Safe")
show_col(safe_cols)

# (1.2): Generate own palette based on colorblind-friendly palette.
group_colors <- tribble(~group, ~col1,
                        "reminder", "#DD8800",
                        "no reminder", "#449944",
                        "anticipated reminder", "#88CCEE",
                        "unanticipated reminder", "#CC6677",
                        "deadline", "#DDCC77",
                        "no deadline", "#AA4499",
                        "2.75% fee", "#44AA99",
                        "3% fee", "#4477EE") %>% 
  as.data.frame()
show_palette <- function() {
  group_colors %>% 
    mutate(group = factor(group, levels = rev(c(group_colors$group)))) %>% 
    ggplot(aes(x = group, fill = group)) +
    geom_bar() +
    scale_fill_manual(name = NULL, values = rev(group_colors$col1)) +
    coord_flip() +
    ylab("") +
    xlab("") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12)) +
    guides(fill = "none")
}
show_palette()

# (1.3): Define function to modify colors (lighter/darker shades).
modify_col <- function(col, change) {
  # Define functions to convert letters to number and numbers to letters.
  l2n <- function(x) {ifelse(suppressWarnings(is.na(as.numeric(x))), utf8ToInt(x) - utf8ToInt("A") + 10, as.numeric(x))}
  n2l <- function(x) {ifelse(x > 9, LETTERS[x - 9], x)}
  # Define each channel's level.
  df <- tribble(~channel, ~current_level, 
                "r", l2n(str_sub(col, 2, 2)), 
                "g", l2n(str_sub(col, 4, 4)),
                "b", l2n(str_sub(col, 6, 6))) %>%
  mutate(desired_level = ifelse(current_level > 0, current_level + change, 0))
  if (max(df$desired_level) > 15) {
    stop("Desired color exceeds brightest level. Select a different change parameter.")
  }
  if (min(df$desired_level) < 0) {
    stop("Desired color exceeds darkest level. Select a different change parameter.")
  }
  df$final_level <- lapply(df$desired_level, n2l) %>% unlist()
  output <- str_c("#", paste(rep(df$final_level, each = 2), collapse = ""))
  return(output)
}

# (1.4): Modify current palette.
group_colors$col2 <- lapply(group_colors$col1, modify_col, -3) %>% unlist()
group_colors$col1 <- lapply(group_colors$col1, modify_col, 0) %>% unlist()
show_col(rbind(group_colors$col1, group_colors$col2))

# (1.5): Export color palette.
write_csv(group_colors, here("proc", "colors.csv"))

# https://www.stat.ubc.ca/~jenny/STAT545A/block14_colors.html

