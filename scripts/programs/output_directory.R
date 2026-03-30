#********************************************************************************************
# File name: 		      29_graph_colors.R
# Creation date:      2022-05-02
# Author:          		César Landín
# Files used:
# 	- (None)
# Files created:
#   - here("proc", "colors.csv")
# Purpose:
# 	- Heterogeneity by number of months in Zettle.
#********************************************************************************************

#***************** Import packages *****************#
# pacman::p_load(here, dplyr, stringr, rstudioapi,
#                install = FALSE)
library(here)
library(dplyr)
library(stringr)
library(rstudioapi)
#*************************************************** #

# ###############################################################
# ##    (1): Write functions that generate regression tables.  ##
# ###############################################################
# # (1.1): Check if output directory already exists, and create if not. #
# if (!file.exists(here("results", "output_directory.csv"))) {
#   file.create(here("results", "output_directory.csv"))
# }
# 
# # (1.2): Define saving function: export file name, data type and output dir to a CSV. #
# save_ <- function(filename) {
#   current_file_output <- tibble(file_name = str_split(filename, "/")[[1]] %>% last(),
#                                 script_name = str_split(getSourceEditorContext()$path, "/")[[1]] %>% last(),
#                                 data_type = str_split(filename, fixed("."))[[1]] %>% last(),
#                                 output_dir = str_split(filename, "/")[[1]] %>% nth(-2)) %>% 
#     mutate(output_dir = ifelse(is.na(output_dir), "notes", output_dir))
#   write.table(current_file_output, 
#               file = here("results", "output_directory.csv"), 
#               sep = ",", 
#               row.names = FALSE, col.names = FALSE, append = TRUE)
# }
# 
# # (1.3): Define specific saving functions. #
# ggsave_ <- function(filename, ...) {
#   save_(filename)
#   ggsave(filename, ...)
# }
# write_csv_ <- function(df, filename, ...) {
#   save_(filename)
#   write_csv(df, filename, ...)
# }
# write_ <- function(df, filename, ...) {
#   save_(filename)
#   write(df, filename, ...)
# }

# Ensure the results directory and the tracking CSV exist
results_dir <- here("results")
if (!dir.exists(results_dir)) {
  dir.create(results_dir, recursive = TRUE)
}
output_file <- here("results", "output_directory.csv")
if (!file.exists(output_file)) {
  # Create an empty file for tracking outputs
  file.create(output_file)
}

# Core saving function: logs the filename, script, data type, and output directory
save_ <- function(filename) {
  # Determine which script is running
  if (rstudioapi::isAvailable() && interactive()) {
    script_path <- rstudioapi::getSourceEditorContext()$path
  } else {
    # In Rscript, extract the --file argument
    cmd <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", cmd, value = TRUE)
    script_path <- if (length(file_arg)) {
      sub("^--file=", "", file_arg)
    } else {
      NA_character_
    }
  }
  
  # Build a one-row tibble of metadata
  current_file_output <- tibble(
    file_name   = basename(filename),
    script_name = basename(script_path),
    data_type   = tools::file_ext(filename),
    output_dir  = basename(dirname(filename))
  ) %>%
    mutate(
      output_dir = ifelse(is.na(output_dir) | output_dir == "", "notes", output_dir)
    )
  
  # Append the metadata to the tracking CSV
  write.table(
    current_file_output,
    file      = output_file,
    sep       = ",",
    row.names = FALSE,
    col.names = FALSE,
    append    = TRUE
  )
}

# Convenience wrappers that log and then save
ggsave_ <- function(filename, ...) {
  save_(filename)
  ggsave(filename, ...)
}
write_csv_ <- function(df, filename, ...) {
  save_(filename)
  write_csv(df, filename, ...)
}
write_ <- function(df, filename, ...) {
  save_(filename)
  write(df, filename, ...)
}
