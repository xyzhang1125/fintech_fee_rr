#--------------------------------------------------------------------------------------------
# File name: 		      graph_functions.R
# Creation date:      2021-01-12
# Author:          		César Landín
# Files used:
# 	(none)
# Files created:
#   (none)
# Purpose:
# 	- Function for formatting and saving footnotes.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
pacman::p_load(here, stringr)
#########################################################

######################################
##    (1): Define table functions.  ##
######################################
# (1.1): Function for formatting and saving footnote. #
format_save_graph_footnote <- function(text = "", filename, size = "footnotesize", spacing = "onehalf") {
  footnote <- str_c(#"\\vspace{1mm} \n",
    "\\", size, "\n", 
    "\\begin{justify}\n",
    ifelse(spacing == "single", "\\singlespacing \n", ""),
    "\\emph{Note}: ", text, "\n",
    ifelse(spacing == "single", "\\onehalfspacing \n", ""),
    "\\end{justify}\n",
    "\\normalsize\n")
  #"\\vspace{1mm}")
  clean_filename <- str_split(filename, fixed("."))[[1]][1]
  write(footnote, here("results", "notes", paste0(clean_filename, ".tex")))
}
