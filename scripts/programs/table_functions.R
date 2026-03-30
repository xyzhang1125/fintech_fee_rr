#********************************************************************************************
# File name: 		      table_functions.R
# Creation date:      2021-12-14
# Author:          		César Landín
# Purpose:
# 	- Function for formatting and saving footnotes.
#********************************************************************************************

#***************** Import packages *****************#
pacman::p_load(here, stringr, scales,
               install = FALSE)
options(modelsummary_format_numeric_latex = "plain",
        "modelsummary_stars_note" = FALSE)
source(here("scripts", "programs", "output_directory.R"))
#*************************************************** #

# https://kbroman.org/pkg_primer/pages/docs.html

#################################################
##    (1): Adjusting table size and position.  ##
#################################################
# (1.1): Define function for visualizing quickly regression outcomes. #
#' Visualize quickly regression outcomes.
#' 
#' @param models List of models.
#' @param ... Additional arguments passed to [modelsummary::modelsummary()].
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Show model
#' mshow(m1)
#' 
#' @export
mshow <- function(models, ...) { 
  modelsummary(models, 
               stars = c("*" = .1, "**" = .05, "***" = 0.01),
               gof_omit = "AIC|BIC|R2|R2 Adj.|R2 Pseudo|R2 Within|Log.Lik.|Std.Errors|FE|RMSE",
               ...)
}

# (1.2): Float here option. #
#' Float LaTeX tables here.
#' 
#' @param table LaTeX-formatted string.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table with float here option.
#' mshow(m1, output = "latex") %>%
#'   float_here() %>% 
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
float_here <- function(table) {
  # remove begin table as required by Sean
  return(table)
}

# (1.3): Use adjustbox for table. #
#' Fit LaTeX tables to text column width.
#' 
#' @param table LaTeX-formatted string.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export model adjusting table width.
#' mshow(m1, output = "latex") %>%
#'   adjust_box() %>%
#'   write(here("results", "tables", "current_table.tex"))
#'  
#' @export
adjust_box <- function(table) {
  # also remove additional settings
  return(table)
}

# (1.4): Use adjustbox for height adjustments. #
#' Adjust the height of a LaTeX table.
#' 
#' @param table LaTeX-formatted string.
#' @param height Height of the table in points.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table adjusting table width and height.
#' mshow(m1, output = "latex") %>%
#'    adjust_box() %>%
#'    adjust_height(height = 0.7) %>%
#'    write(here("results", "tables", "current_table.tex"))
#'    
#' @export
adjust_height <- function(table, height) {
  # remove settings as well.
  return(table)
}

# (1.5): Auxiliary function for inserting string lines in LaTeX tables. #
#' Insert string lines in a LaTeX table.
#' 
#' @param table LaTeX-formatted string.
#' @param pattern Reference string.
#' @param insert String to insert after pattern.
#' @param before Logical indicating whether string should be inserted before or after pattern. Default is FALSE (insert after).
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table inserting caption.
#' mshow(m1, output = "latex") %>%
#'   float_here() %>% 
#'   str_insert(pattern = "\\begin{table}[H]",
#'              insert =  "\\caption{Current Table Caption}") %>% 
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
str_insert <- function(table, pattern, insert, before = FALSE) {
  rep <- ifelse(before,
                str_c(fixed(insert), "\n", fixed(pattern)),
                str_c(fixed(pattern), "\n", fixed(insert)))
  table %>% 
    str_replace(pattern = fixed(pattern),
                replacement = rep)
}

# (1.6): Define spacing between columns. #
#' Define spacing between columns.
#' 
#' @param table LaTeX-formatted string.
#' @param spacing Spacing between columns in points.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table adjusting spacing.
#' mshow(m1, output = "latex") %>%
#'   adjust_col_spacing(3) %>%
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
adjust_col_spacing <- function(table, spacing) {
  table %>% 
    str_insert(pattern = "\\centering",
               insert = str_c("\\setlength{\\tabcolsep}{", spacing, "pt}"))
}

######################################################################
##    (2): Updating variable names and adding a caption and label.  ##
######################################################################
# (2.1): Get number of numeric columns. #
#' Get number of numeric columns.
#' 
#' @param table LaTeX-formatted string.
#' 
#' @return Number of numeric columns.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Get number of columns in table.
#' mshow(m1, output = "latex") %>%
#'   get_num_cols()
#' 
#' @export
get_num_cols <- function(table) {
  pos_start <- str_locate(table, fixed("tabular}[t]{"))[2] + 1 # Get starting position of column alignment
  pos_end <- str_sub(table, pos_start, pos_start + 15) %>%     # Extract end position
    str_locate(fixed("}"))
  pos_end <- pos_end[1] + pos_start - 2
  col_num <- (str_sub(table, pos_start, pos_end) %>% str_length()) - 1 # Exclude leftmost column
  return(col_num)
}

insert_col_numbers <- function(table, col_numbers) {
  if (col_numbers) {
    table %>% 
      str_insert(pattern = "\\midrule",
                 insert = str_c("& ", paste(str_c("(", 1:get_num_cols(table), ")"), collapse = " & "), "\\\\"),
                 before = TRUE) 
  } else {
    table
  }
}

# (2.2): Add main multi-column title. #
#' Add main multi-column title.
#' 
#' @param table LaTeX-formatted string.
#' @param title Title to add.
#' @param cline Logical indicating whether to add a cline after the title. Default is FALSE.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table adding main multi-column title.
#' mshow(m1, output = "latex") %>%
#'   multicol_title(title = "Current Table Title") %>%
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
# multicol_title <- function(table, title, cline = FALSE) {
#   proc_table <- table %>% 
#     str_insert(pattern = "\\toprule",
#                insert = str_c("& \\multicolumn{", get_num_cols(table), "}{c}{", title, "} \\\\\n%clh"))
#   if (cline) {
#     proc_table %<>% 
#       str_replace(pattern = "%clh",
#                   replacement = str_c(fixed("\\\\cline{2-"), get_num_cols(table) + 1, "}"))
#   }
#   return(proc_table)
# }


# multicol_title <- function(table, title, cline = FALSE, extra_space = NULL, cline_space = NULL) {
#   # Insert multicolumn title row
#   proc_table <- table %>%
#     str_insert(
#       pattern = "\\toprule",
#       insert = str_c("& \\multicolumn{", get_num_cols(table), "}{c}{", title, "} \\\\", extra_space %||% "", "\n%clh")
#     )
# 
#   # Add \cline if requested
#   if (cline) {
#     proc_table <- proc_table %>%
#       str_replace(
#         pattern = "%clh",
#         # replacement = str_c("\\cline{2-", get_num_cols(table) + 1, "}", cline_space %||% "")
#         #replacement = str_c("\\\\cline{2-", get_num_cols(table) + 1, "}", paste0("\\"), cline_space %||% ""))
#         replacement = str_c("\\\\cline{2-", get_num_cols(table) + 1, "} \\\n\\\\vspace{", cline_space %||% "0ex", "}", paste("\\\\"))
#         )
#   } else {
#     # Remove placeholder if no \cline is needed
#     proc_table <- proc_table %>%
#       str_replace("%clh", "")
#   }
# 
#   return(proc_table)
# }

multicol_title <- function(table, title, cline = FALSE, extra_space = NULL) {
  # Insert multicolumn title row
  proc_table <- table %>%
    str_insert(
      pattern = "\\toprule",
      insert = str_c("& \\multicolumn{", get_num_cols(table), "}{c}{", title, "} \\\\", extra_space %||% "", "\n%clh")
    )
  
  # Add \cline if requested
  if (cline) {
    proc_table <- proc_table %>%
      str_replace(
        pattern = "%clh",
        replacement = str_c("\\\\cline{2-", get_num_cols(table) + 1, "} \\\n")
      )
  } else {
    # Remove placeholder if no \cline is needed
    proc_table <- proc_table %>%
      str_replace("%clh", "")
  }
  
  return(proc_table)
}



# (2.3): Remove stock model names (e.g. Model 1, Model 2). #
#' Remove stock model names.
#' 
#' @param table LaTeX-formatted string.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table removing stock model names.
#' mshow(m1, output = "latex") %>%
#'  remove_names() %>%
#'  write(here("results", "tables", "current_table.tex"))
#' 
#' @export
remove_names <- function(table) {
  if (str_detect(table, "Model 1")) {
    table %>% 
      str_replace(pattern = fixed(str_c("& ", str_c("Model ", 1:get_num_cols(table), collapse = " & "), "\\\\\n")),
                  replacement = "")
  } else {
    table
  }
}

# (2.4): Add caption and label, and center if table does not included \centering. #
#' Add caption and label to LaTeX table.
#' 
#' @param table LaTeX-formatted string.
#' @param caption String to be used as caption.
#' @param label String to be used as label.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table adding caption and label.
#' mshow(m1, output = "latex") %>%
#'   add_caption_label_center(caption = "Current Table Caption",
#'                            label = "current_table_label") %>%
#'   write(here("results", "tables", "current_table.tex"))
#' @export
add_caption_label_center <- function(table, caption, label) {
  # Change: remove the \begin{table}[H] and \end{table} entirely
  # Just return the tabular content without table environment
  return(table)
}

######################################
##    (3): Final table formatting.  ##
######################################
# (3.1): Number formatting function. #
#' Number formatting function.
#' 
#' @param x Numeric vector.
#' @param override_right_digits Number of right digits to use. Default is NA.
#' 
#' @return Formatted character vector.
#' 
#' @examples
#' library(stringr)
#' 
#' lapply(c(0.098, 0.11, 3.1233, 45.968, 1949), comma_format) %>% unlist()
#' 
#' @export
comma_format <- function(x, override_right_digits = NA) {
  # Calculate number of right digits
  if (x <= 0) {num <- 1} else {num <- x}
  right_digits <- 3 - floor(log10(abs(num)))
  if (right_digits < 0) {right_digits <- 0}
  if (right_digits > 3) {right_digits <- 3}
  if (!is.na(override_right_digits)) {right_digits <- override_right_digits}
  # Calculate number of left digits
  left_digits <- 4 + floor(log10(abs(num)))
  if (left_digits <= 0) {left_digits <- 1}
  # Format number
  proc_num <- format(round(x, right_digits), nsmall = right_digits, digits =  left_digits, big.mark = ",")
  if (proc_num != "0" & as.numeric(str_replace(proc_num, fixed(","), "")) < 1) {
    proc_num <- str_pad(proc_num, right_digits + 2, "right", "0")
  }
  return(proc_num)
}

# (3.2): Replace number of observations with comma format in models. #
#' Replace number of observations with comma format in models.
#' 
#' @param models List of models.
#' 
#' @return List of models.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table replacing number of observations with comma format.
#' m1 %>%
#'   comma_numobs() %>%
#'   mshow(output = "latex") %>%
#' write(here("results", "tables", "current_table.tex"))
#' 
#' @export
comma_numobs <- function(models) {
  model_proc <- models
  for (i in 1:length(model_proc)) {
    model_proc[[i]]$nobs <- comma(model_proc[[i]]$nobs)
  }
  return(model_proc)
}

# (3.3): Replace "Num.Obs." with alternative text. #
#' Replace "Num.Obs." with alternative text.
#' 
#' @param table LaTeX-formatted string.
#' @param text String to be used as replacement for "Num.Obs.".
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table replacing "Num.Obs." with alternative text.
#' mshow(m1, output = "latex") %>%
#'   replace_numobs(text = "Number of observations") %>% 
#'   write(here("results", "tables", "current_table.tex"))
replace_numobs <- function(table, text) {
  table %>% 
    str_replace(pattern = "Num.Obs.",
                replacement = text) %>% 
    str_replace(pattern = "Num. Obs.",
                replacement = text)
}

# (3.4): Distribute coefficient names over two rows (when using "\\times"). #
#' Distribute coefficient names over two rows.
#' 
#' @param table LaTeX-formatted string.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary, magrittr)
#' 
#' # Add new variable to dataset.
#' # Insert new random variable that takes values 0 and 1 in iris dataset.
#' set.seed(123)
#' current_data <- iris %>%
#'   mutate(random = sample(0:1, nrow(.), replace = TRUE))
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length * random,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = current_data)
#' 
#' # Define coefficient names.
#' coef_names <- coef_names <- c("Sepal Length", "Random variable", "Sepal Length times Random variable") %>%
#'   set_names(c("Sepal.Length", "random", "Sepal.Length:random"))
#' 
#' # Export table with coefficient names over two rows.
#' mshow(m1, 
#'       coef_map = coef_names, 
#'       output = "latex") %>%
#'   coef_two_rows() %>%
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
coef_two_rows <- function(table) {
  # Check number of string locations
  string_loc <- str_locate_all(table, "times")[[1]]
  num_locations <- nrow(string_loc)
  # Generate temporary output
  output <- table
  # Tag words
  tagged_string <- "REPtimes"
  output %<>% str_replace_all("times", tagged_string)
  # Loop over string locations
  for (i in 1:nrow(string_loc)) {
    # Locate coefficient string to replace with ""
    pat_loc <- str_locate(output, fixed(tagged_string))[1]
    # Locate lower row to replace with coefficient string
    upper_row <- str_sub(output, pat_loc, str_length(output))
    lower_row <- upper_row %>% 
      str_sub(str_locate(upper_row, fixed("\\\\\n "))[2] + 1, str_length(upper_row))
    # Define pattern to replace in upper row
    pat_rep_upper <- str_split(upper_row, "&", n = 2)[[1]][1]
    # Define pattern to replace in lower row
    pat_rep_lower <- lower_row %>% 
      str_sub(1, str_locate(lower_row, fixed("\\\\\n"))[2])
    # Replace strings in table.
    output %<>%
      str_replace(pattern = pat_rep_upper,
                  replacement = "") %>% 
      str_replace(pattern = fixed(pat_rep_lower),
                  replacement = str_c("\\quad $\\times$", 
                                      str_replace(pat_rep_upper, tagged_string, ""),
                                      pat_rep_lower))
  }  
  return(output)
}

# (3.5): Insert space between variables (standard error and coefficients). #
#' Insert space between variables.
#' 
#' @param table LaTeX-formatted string.
#' @param space Spacing in cm.
#' 
#' @return LaTeX-formatted string.
#' 
#' @examples
#' # Load packages
#' library(stringr, fixest, modelsummary)
#' 
#' # Define model
#' m1 <- feols(Petal.Width ~ Sepal.Length,
#'             fixef = "Species",
#'             cluster = "Species",
#'             data = iris)
#' 
#' # Export table with space between variables.
#' mshow(m1, output = "latex") %>%
#'   add_space() %>%
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
add_line_space <- function(table, space = 0.3) {
  # Keep split rows after first parentheses
  keep_after_firstpar <- (table %>% 
                            str_sub(str_locate(table, fixed("("))[1], str_length(table)) %>% 
                            str_split("\\\n"))[[1]]
  # Keep rows with parentheses
  keep_after_firstpar <- keep_after_firstpar[str_detect(keep_after_firstpar, fixed("("))]
  # Drop first one (column numbers)
  keep_after_firstpar <- keep_after_firstpar[2:length(keep_after_firstpar)]
  # Replace with spacing
  proc_table <- table
  for (i in 1:length(keep_after_firstpar)) {
    proc_table %<>%
      str_replace(pattern = fixed(keep_after_firstpar[i]),
                  replacement = str_c(keep_after_firstpar[i], "[", space, "cm]"))
  }
  # Remove space between final row and midrule
  proc_table %<>%
    str_replace(pattern = "\\[0\\.[0-9]+cm\\]\n\\\\midrule",
                replacement = "\n\\\\midrule")
  return(proc_table)
}

##########################################################
##    (4): Master table function and adding footnotes.  ##
##########################################################
# (4.1.1): Base table function combining all functions. # General
gen_base_table <- function(models, caption, label, title = NULL, cline = FALSE, extra_space = NULL, coef_names = NULL, remove_col_names = FALSE, numobs = "Number of firms", ...) {
  # Get length of model. If large, it is a regression that needs to be put in a list
  if (length(models) > 10) {models <- list(models)}
  # Generate table
  output <- models %>% 
    set_names(case_when(!remove_col_names & !is.null(names(models)) ~ list(names(models)),
                        !remove_col_names & is.null(names(models)) ~ list(paste("Model", 1:length(models))), 
                        remove_col_names ~ list(paste("Model", 1:length(models)))) %>% 
                unlist()) %>% 
    # Fix number of observation number format
    comma_numobs() %>% 
    # Generate table with modelsummary
    modelsummary(coef_map = coef_names,
                 stars = c("*" = .1, "**" = .05, "***" = 0.01),
                 gof_omit = "AIC|BIC|R2|R2 Adj.|R2 Pseudo|R2 Within|Log.Lik.|Std.Errors|FE|RMSE",
                 escape = FALSE,
                 output = "latex",
                 ...) %>%
    # Insert float here option
    float_here() %>% 
    # Insert adjustbox
    adjust_box() %>% 
    # Add caption and label
    add_caption_label_center(caption = caption,
                             label = label) %>% 
    # Insert column numbers
    insert_col_numbers(ifelse(length(models) == 1, FALSE, TRUE)) %>% 
    # Remove model names if no unique column names given
    remove_names() %>% 
    # Replace number of observations (Num.Obs. with Num. Obs.)
    replace_numobs(numobs)
  if (!is.null(title)) {
    output %<>% multicol_title(title, cline = cline, extra_space = extra_space)
  }
  
  # make sure to remove begin table
  output <- gsub("\\\\begin\\{table\\}(\\[[^]]*\\])?\\s*", "", output)
  output <- gsub("\\\\centering\\s*", "", output)
  output <- gsub("\\\\end\\{table\\}", "", output)
  
  return(output)
}

# (4.1.2): Base table function combining all functions. # Login
gen_base_table_login <- function(models, caption, label, title = NULL, cline = FALSE, extra_space = NULL, coef_names = NULL, remove_col_names = FALSE, numobs = "Number of firms", only_tabular = FALSE, ...) {
  # Get length of model. If large, it is a regression that needs to be put in a list
  if (length(models) > 10) {models <- list(models)}
  
  # Generate table
  output <- models %>% 
    set_names(case_when(!remove_col_names & !is.null(names(models)) ~ list(names(models)),
                        !remove_col_names & is.null(names(models)) ~ list(paste("Model", 1:length(models))), 
                        remove_col_names ~ list(paste("Model", 1:length(models)))) %>% 
                unlist()) %>% 
    # Fix number of observation number format
    comma_numobs() %>% 
    # Generate table with modelsummary
    modelsummary(coef_map = coef_names,
                 stars = c("*" = .1, "**" = .05, "***" = 0.01),
                 gof_omit = "AIC|BIC|R2|R2 Adj.|R2 Pseudo|R2 Within|Log.Lik.|Std.Errors|FE|RMSE",
                 escape = FALSE,
                 output = "latex",
                 ...)
  
  # remove \vphantom
  output <- stringr::str_remove_all(output, "\\\\vphantom\\{[12]\\}")
  
  # If people only want to export the tabular part
  if (only_tabular) {
    # extract the tabular part from the whole table
    tabular_start <- stringr::str_locate(output, "\\\\begin\\{tabular\\}")[1]
    tabular_end <- stringr::str_locate(output, "\\\\end\\{tabular\\}")[2]
    
    if (!is.na(tabular_start) && !is.na(tabular_end)) {
      output <- stringr::str_sub(output, tabular_start, tabular_end)
    }
    
    # remove adjust box
    output <- stringr::str_remove_all(output, "\\\\adjustbox\\{max width=.*?\\}")
    output <- stringr::str_remove_all(output, "\\\\centering")
    output <- stringr::str_replace_all(output, "\\\\\\[\\\\baselineskip\\]", "")
    
    # add column number
    output <- add_column_numbers_to_tabular(output, length(models))
    
    # remove num. obs
    output <- stringr::str_replace(output, "Num\\.Obs\\.", numobs)
    
  } else {
    # create the whole table
    output <- output %>%
      # Insert float here option
      float_here() %>% 
      # Insert adjustbox
      adjust_box() %>% 
      # Add caption and label
      add_caption_label_center(caption = caption,
                               label = label) %>% 
      # Insert column numbers
      insert_col_numbers(ifelse(length(models) == 1, FALSE, TRUE)) %>% 
      # Remove model names if no unique column names given
      remove_names() %>% 
      # Replace number of observations (Num.Obs. with Num. Obs.)
      replace_numobs(numobs)
    
    if (!is.null(title)) {
      output %<>% multicol_title(title, cline = cline, extra_space = extra_space)
    }
  }
  
  return(output)
}

# add column number for tabular
add_column_numbers_to_tabular <- function(tabular_output, n_models) {
  lines <- strsplit(tabular_output, "\n")[[1]]
  
  toprule_line <- which(stringr::str_detect(lines, "\\\\toprule"))[1]
  if (!is.na(toprule_line)) {
    column_numbers <- paste0(" & (", paste(1:n_models, collapse = ") & ("), ")\\\\")
    
    # toprule_line + 2
    insert_position <- toprule_line + 2
    new_lines <- c(
      lines[1:(insert_position - 1)],
      column_numbers,
      lines[insert_position:length(lines)]
    )
    
    return(paste(new_lines, collapse = "\n"))
  }
  
  return(tabular_output)
}

# (4.2): Function for formatting and saving footnote. #
#' Save a LaTeX-formatted string as a .tex document to be included as a table footnote
#'
#' @param text LaTeX-formatted string.
#' @param filename File name of corresponding graph or table.
#' @param stars Indicates whether explanation for significance stars should be included or not. Default is FALSE.
#'
#' @return LaTeX-formatted string.
#'
#' @examples
#' # Load packages
#' library(stringr)
#' 
#' # Define footnote
#' footnote <- str_c("This is a footnote. \n", "We decided that this footnote would have two rows and include significance stars.")
#' 
#' # Save footnote with significance stars
#' format_save_footnote(footnote, "current_table.tex", TRUE)
#'
#' # In LaTeX document:
#' \input{../results/tables/current_table.tex}
#' \input{../results/notes/current_table.tex}
#' 
#' @export
format_save_footnote <- function(text = "", filename, stars = FALSE) {
  sig <- ifelse(stars, " * $p < 0.1$, ** $p < 0.05$, *** $p < 0.01$.", "")
  footnote <- str_c("\\footnotesize \n", 
                    "\\begin{justify} \n",
                    "\\emph{Note}: ", text, sig, "\n",
                    "\\end{justify} \n",
                    "\\normalsize \n")
  clean_filename <- str_split(filename, fixed("."))[[1]][1]
  write(footnote, here("results", "notes", str_c(clean_filename, ".tex")))
}

##################################################
##    (5): Preparing tables for presentations.  ##
##################################################
# (5.1): Generate presentation version of a table. #
#' Convert a LaTeX paper table to use in a presentation. Subsets content from \begin{tabular} to \end{tabular}.
#'
#' @param table_name Name of the table to convert.
#' @param table_path Folder path of the file to convert. Default is here("results", "tables").
#' @param output_path Folder path where converted file will be saved. Default is here("results", "tables").
#'
#' @examples
#' library(readr, stringr)
#' 
#' # Convert paper table to presentation table.
#' convert_save_pres_table("het_percent_sales.tex")
#'
#' @export
convert_save_pres_table <- function(table_name, table_path = here("results", "tables"), output_path = here("results", "tables")) {
  # Read table
  table <- read_file(here(table_path, table_name))
  # Keep content between tabulars
  table <- str_sub(table,
                   str_locate(table, fixed("\\begin{tabular}"))[1],
                   str_locate(table, fixed("\\end{tabular}"))[2])
  # Remove any additional spacing
  table %<>%
    str_remove_all("\\[0\\.[0-9]+cm\\]")
  # Save table
  pres_table_name <- str_c("pres_", table_name)
  write(table, here(output_path, pres_table_name))
  print(str_c("Presentation table saved to",
              str_remove(output_path, here()), "/", pres_table_name))
}

# (5.2): Remove standard errors and column numbers (useful for presentation tables). #
#' Remove standard errors and column numbers from a LaTeX table.
#' 
#' @param table LaTeX table.
#' 
#' @return LaTeX table without standard errors and column numbers.
#' 
#' @examples
#' library(readr, stringr)
#' 
#' # Read in table
#' read_file(here("results", "tables", "previous_table.tex")) %>% 
#'   remove_std_errors_col_nums() %>% 
#'   write(here("results", "tables", "current_table.tex"))
#' 
#' @export
remove_std_errors_col_nums <- function(table) {
  table %>%   
    str_split("\n") %>% 
    .[[1]] %>% 
    .[!(str_detect(., fixed("(")))] %>% 
    str_c(collapse = "\n")
}

# (5.3): Add note after bottomrule. #
#' Add a note to a LaTeX table after the bottomrule.
#' 
#' @param table LaTeX table.
#' @param note Note to add to table.
#' 
#' @return LaTeX table with note added after bottomrule.
#' 
#' @examples
#' library(readr, stringr)
#' 
#' # Read in table
#' read_file(here("results", "tables", "previous_table.tex")) %>%
#'  add_note("This is a note.") %>%
#'  write(here("results", "tables", "current_table.tex"))
#' 
#' @export
add_note <- function(table, note) {
  table %>% 
    str_insert(pattern = "\\bottomrule",
               insert = str_c("\\multicolumn{", get_num_cols(table) + 1, "}{c}{", note, "}"))
}

# add_vspace_after_cline <- function(table_output) {
#   # Replace \cline{2-7} with \cline{2-7}\vspace{-1.5ex} \\
#   table_output <- gsub("\\cline{2-7}", "\\cline{2-7}\n\\vspace{-1.5ex} \\\\", table_output)
#   return(table_output)
# }

# add_vspace_after_cline <- function(file_path) {
#   # Read the file content
#   table_content <- readLines(file_path)
#   
#   # Replace \cline{2-7} with \cline{2-7}\vspace{-1.5ex} \\
#   # Use double backslashes to escape the curly braces in the pattern
#   table_content <- gsub("\\\\cline\\{2-7\\}", "\\\\cline{2-7}\n\\\\vspace{-1.5ex} \\\\", table_content)
#   
#   # Write the modified content back to the file
#   writeLines(table_content, file_path)
# }

# Custom function to add \vspace after \cline{2-7}
add_vspace_after_cline <- function(file_path) {
  # Read the file content
  table_content <- readLines(file_path)
  
  # Identify the index of \cline{2-7}
  cline_index <- which(grepl("\\\\cline\\{2-7\\}", table_content))
  
  # If \cline{2-7} is found, insert \vspace{-1.5ex} \\ after it
  if (length(cline_index) > 0) {
    # Create the new line to insert
    vspace_line <- "\\vspace{-1.5ex} \\\\"
    
    # Insert the new line after each occurrence of \cline{2-7}
    for (index in rev(cline_index)) {
      table_content <- append(table_content, vspace_line, after = index)
    }
  }
  
  # Write the modified content back to the file
  writeLines(table_content, file_path)
}

# Custom function to add \vspace after \cline{2-7}
add_vspace_after_cline <- function(file_path) {
  # Read the file content
  table_content <- readLines(file_path)
  
  # Identify the index of \cline{2-4}
  cline_index <- which(grepl("\\\\cline\\{2-4\\}", table_content))
  
  # If \cline{2-7} is found, insert \vspace{-1.5ex} \\ after it
  if (length(cline_index) > 0) {
    # Create the new line to insert
    vspace_line <- "\\vspace{-1.5ex} \\\\"
    
    # Insert the new line after each occurrence of \cline{2-7}
    for (index in rev(cline_index)) {
      table_content <- append(table_content, vspace_line, after = index)
    }
  }
  
  # Write the modified content back to the file
  writeLines(table_content, file_path)
}
