#********************************************************************************************
# File name: 		      number_functions.R
# Creation date:      2022-01-10
# Author:          		César Landín
# Files used:
# 	- (none)
# Files created:
#   - (none)
# Purpose:
# 	- Functions for calculating and saving numbers.
#********************************************************************************************

#***************** Import packages *****************#
# pacman::p_load(stringr, magrittr)
library(here)
library(stringr)
library(magrittr)
#conflicts_prefer(dplyr::last)
source(here("scripts", "programs", "output_directory.R"))
#*************************************************** #

################################
##    (1): Define functions.  ##
################################
# (1.1): Check if a number is an integer.
num_int <- function(x) {
  x == round(x)
}

# (1.2): Number formatting function. #
comma_format <- function(x, override_right_digits = NA) {
  if (x <= 0) {num <- 1} else {num <- x}
  right_digits <- 3 - floor(log10(abs(num)))
  if (right_digits < 0) {right_digits <- 0}
  if (right_digits > 3) {right_digits <- 3}
  if (!is.na(override_right_digits)) {right_digits <- override_right_digits}
  left_digits <- 4 + floor(log10(abs(num)))
  if (left_digits <= 0) {left_digits <- 1}
  proc_num <- format(round(x, right_digits), nsmall = right_digits, digits =  left_digits, big.mark = ",")
  if (proc_num != "0" & as.numeric(str_replace_all(proc_num, fixed(","), "")) < 1) {
    proc_num <- str_pad(proc_num, right_digits + 2, "right", "0")
  }
  return(proc_num)
}

# (1.3): Print number to file.
print_n <- function(x, file, note = "", dig = 1) {
  # Round number to desired number of digits
  current_num <- as.numeric(x) %>% 
    round(dig)
  # Don't add 0 after decimal point if number is a whole number
  # https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics
  current_num <- ifelse(num_int(current_num),
                        comma_format(current_num, override_right_digits = 0),
                        comma_format(current_num, override_right_digits = dig))
 # Export number
  current_num %>% 
    print() %>% # prior to adding the % for Latex
    str_c("%") %>% # this % is to prevent space after number
    str_c(ifelse(note != "", str_c("\n% ", note), "")) %>%
    write_(here("results", "numbers", file))
}

# (1.4): Print percentage to file.
print_pct <- function(x, file, note = "", dig = 1) {
  # Round number to desired number of digits
  current_num <- as.numeric(x) %>% 
    "*"(100) %>% 
    round(dig)
  # Don't add 0 after decimal point if percentage is a whole number
  # https://support.jmir.org/hc/en-us/articles/360019690851-Guidelines-for-Reporting-Statistics
  current_num <- ifelse(num_int(current_num),
                        comma_format(current_num, override_right_digits = 0),
                        comma_format(current_num, override_right_digits = dig))
  # Export number
  current_num %>% 
    str_c("\\%%") %T>% # this % is to prevent space after number; not the \% in Latex
    print() %>% 
    str_c(ifelse(note != "", str_c("\n% ", note), "")) %>% 
    write_(here("results", "numbers", file))  # includes the "%" in printing since it's a percent
}

# (1.5): Get mean. #
num_mean <- function(df, variable, dig = 1) {
  df %>% 
    pull(eval(as.name(variable))) %>% 
    mean(na.rm = TRUE) %>% 
    round(digits = dig)
}

# (1.6): Get median. #
num_median <- function(df, variable, dig = 1) {
  df %>% 
    pull(eval(as.name(variable))) %>% 
    median(na.rm = TRUE) %>% 
    round(digits = dig)
}

# (1.7): Get SD. #
num_sd <- function(df, variable, dig = 1) {
  df %>% 
    pull(eval(as.name(variable))) %>% 
    sd(na.rm = TRUE) %>% 
    round(digits = dig)
}

# (1.8): Binomial proportion test
binom_test <- function(n1, N1, n2, N2) {
  # Define proportions
  p1 <- n1 / N1
  p2 <- n2 / N2
  # Get z-stat
  p <- (n1 * p1 + n2 * p2)/ (n1 + n2)
  z <- (p1 - p2) / sqrt(p * (1-p) * (1/n1 + 1/n2))
  # Two-tailed test
  z_p <- pnorm(z, lower.tail = TRUE)
  return(z_p)
}
