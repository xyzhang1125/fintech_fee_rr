#--------------------------------------------------------------------------------------------
# File name: 		      00_setup.R
# Creation date:      2022-01-03
# Author:          		Noah Forougi & César Landín
# Files used:
# 	- (None)
# Files created:
# 	- here("proc")
# 	- here("proc", "temp")
# Purpose:
# 	- This script installs necessary packages and creates the directories not set up in the 
#     Dropbox or GitHub.
#--------------------------------------------------------------------------------------------

#####################################################
##  (1): Install packages and set up directories.  ##
#####################################################
# (1.1): Install necessary packages.
# renv::snapshot()
# Question:
# It looks like you've called renv::snapshot() in a project that hasn't been activated yet
# How would you like to proceed?
# Answer: 
# 1: Activate the project and use the project library.

renv::restore()
# Question:
# The following package(s) will be updated: (...)
# Do you want to proceed? [Y/n]: 
# Answer: 
# Y

# (1.2): Create directories.
library(here)
sup_create <- function(file) {
  suppressWarnings(dir.create(file))
}
sup_create(here("proc"))
sup_create(here("proc", "temp"))

# (1.2): Create directories.
if (needs_julia) {
  library(JuliaCall)
  julia_setup(installJulia = TRUE)
  
  pkgs <- c("CairoMakie", "LaTeXStrings", "Memoize", "ThreadSafeDicts", "DelimitedFiles", "Printf")
  for (pkg in pkgs) julia_install_package(pkg)
  
  fig_dir <- here("results", "figures")
  num_dir <- here("results", "numbers")
  julia_assign("ARGS", c(fig_dir, num_dir))
}

