#--------------------------------------------------------------------------------------------
# File name: 		      24a_elasticity_tables_functions.R
# Creation date:      2022-12-02
# Author:          		César Landín
# Files used:
# 	- (None)
# Files created:
#   - (None)
# Purpose:
# 	- Define functions for elasticity tables.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(tidyverse)
library(magrittr)
source(here("scripts", "programs", "table_functions.R"))
source(here("scripts", "programs", "number_functions.R"))
#########################################################

######################################################
##    (1): Define functions for elasticity tables.  ##
######################################################
# (1.1): Function for converting and saving coefficients.
get_coef <- function(type, var) {
  long_var <- case_when(var == "logsales" ~ wp_outcomes[1],
                        var == "logtransactions" ~ wp_outcomes[2],
                        var == "churn" ~ wp_outcomes[3])
  beta <- eval(as.name(str_c("reg_monthly_", type, "_dtm_adjusteddates")))[[long_var]][["coeftable"]][1]
  if (var %in% c("logsales", "logtransactions")) {
    output <- exp(beta) - 1
  } else {
    output <- beta
  }
  return(output)
}

# (1.2): Functions for getting treatment period control means.
get_control_mean <- function(df, var) {
  df %>% 
    filter(timestamp_month >= "2020-10-01" & treat_description == "Control") %>% 
    pull(all_of(var)) %>% 
    mean()
}
formatted_mean <- function(df, var) {
  get_control_mean(df, var) %>% round(3) %>% comma_format()
}

# (1.3): Function for getting number of observations.
get_n_firms <- function(df) {
  df %>% 
    select(organization_uuid) %>% 
    unique() %>% 
    count()
}

# (1.4): Function for getting additional rows.
get_add_rows <- function(df) {
  data.frame(rbind(c("Number of firms", rep(comma_format(get_n_firms(df)$n), 3)),
                   c("Cluster std. errors", rep("Firm", 3)),
                   c("Fixed effects", rep("Firm \\& month", 3)),
                   c("Control mean (levels)", 
                     formatted_mean(df, "valid_volume"),
                     formatted_mean(df, "nr_valid_payments"),
                     formatted_mean(df, "made_sale_after")),
                   c("Control mean (levels, winsorized)", 
                     formatted_mean(df, "valid_volume_w5"),
                     formatted_mean(df, "nr_valid_payments_w5"),
                     formatted_mean(df, "made_sale_after"))))
}

# (1.5): Function for generating single ITT or TOT table (including presentation version).
gen_save_table <- function(models, sname, lname, pres = FALSE) {
  # Define coefficient map
  cmap <- ifelse(str_detect(sname, "monthly_itt"), c("Post * Treated"), c("Post * Accepted"))
  if (str_detect(sname, "monthly_itt")) {
    cmap <- c("Post * Treated")
    names(cmap) <- "postXtreated"
  } else {
    cmap <- c("Post * Accepted")
    names(cmap) <- "fit_postXadopted"
  }

  # Generate and export tables
  output <- gen_base_table(models,
                           caption = str_c("Monthly Sales Elasticity: ", lname),
                           label = sname,
                           coef_names = cmap,
                           add_rows = additional_rows,
                           numobs = "Number of observations")
  write_(output, file = here("results", "tables", str_c(sname, ".tex")))
  if (pres) {convert_save_pres_table(str_c(sname, ".tex"))}
}

# (1.6): Function for generating combined ITT and TOT table.
gen_full_table <- function(itt_name, label = "Intent to Treat and Treatment on the Treated") {
  # file path
  itt_file <- here("results", "tables", paste0(itt_name, ".tex"))
  tot_file <- here("results", "tables", gsub("itt", "tot", paste0(itt_name, ".tex")))
  
  # read in the result blocks
  itt_lines <- readLines(itt_file, warn = FALSE)
  tot_lines <- readLines(tot_file, warn = FALSE)
  
  # extract ITT midrule
  midrule_indices <- which(grepl("\\\\midrule", itt_lines))
  
  if (length(midrule_indices) < 2) {
    stop("cannot find two \\midrule")
  }
  
  # extract ITT table
  itt_body_lines <- itt_lines[(midrule_indices[1] + 1):(midrule_indices[2] - 1)]
  
  # find the first \midrule in TOT
  tot_midrule_index <- which(grepl("\\\\midrule", tot_lines))[1]
  
  if (is.na(tot_midrule_index)) {
    stop("cannot find \\midrule")
  }
  
  # create new table
  new_tot_lines <- c(
    tot_lines[1:(tot_midrule_index - 1)],
    "\\toprule",
    "\\underline{Panel A: Intent to Treat} \\\\",
    itt_body_lines,
    "\\underline{Panel B: Treatment on the Treated} \\\\",
    tot_lines[(tot_midrule_index + 1):length(tot_lines)]
  )
  
  # title change
  for (i in seq_along(new_tot_lines)) {
    if (grepl("Monthly Sales Elasticity: ", new_tot_lines[i])) {
      new_tot_lines[i] <- gsub("Monthly Sales Elasticity: [^}]+", 
                               paste0("Monthly Sales Elasticity: ", label), 
                               new_tot_lines[i])
      break
    }
  }
  
  # label change
  new_tot_lines <- gsub("monthly_tot_shortterm_card", "monthly_itt_tot", new_tot_lines)
  
  # combine them
  combined <- paste(new_tot_lines, collapse = "\n")
  
  return(combined)
}