#--------------------------------------------------------------------------------------------
# File name: 		      20d_survey_panel_figures.R
# Creation date:      2023-01-31
# Author:          		César Landín & Mohammad Atif Haidry
# Files used:
# (None)
# Files created:
# 	- here("results", "figures", "survey_exp_form_time_cat_alt_form_time_cat2.tex")
# 	- here("results", "figures", "survey_why_deadline_why_reminder.tex")
# 	- here("results", "figures", "survey_why_activate_firstday_why_activate_later.tex")
# Purpose:
#   - Creating .tex files that combine the two subfigures for figure C.3, figure C.6, and figure C.13.
# 	- Generate panel latex panel that can import figures into them.
#--------------------------------------------------------------------------------------------

#################### Import packages #################### 
library(here)
library(readr)
#########################################################

# Generating side-by-side latex figure .tex file:
generate_side_by_side_latex_figure <- function(caption1, label1, file1, caption2, label2, file2, output_file) {
  # Define the LaTeX code as a string with placeholders for input values
  latex_code <- sprintf("
    \\begin{subfigure}[t]{0.45\\textwidth}
        \\caption{%s}
        \\label{%s}
        \\centering
        \\vspace{-.4\\baselineskip}
        \\includegraphics[width=\\textwidth]{%s}
    \\end{subfigure}
    \\hspace{0.05\\textwidth} %% Space between the subfigures
    \\begin{subfigure}[t]{0.45\\textwidth}
        \\caption{%s}
        \\label{%s}
        \\centering
        \\vspace{-.4\\baselineskip}
        \\includegraphics[width=\\textwidth]{%s}
    \\end{subfigure}
",
                        caption1, label1, file1, caption2, label2, file2)
  
  # Write the LaTeX code to a file
  write_lines(latex_code, file = output_file)
}


generate_side_by_side_latex_figure(
  caption1 = "Expected Time",
  label1 = "fig:exp_form_time_cat_alt",
  file1 = "../results/figures/survey_exp_form_time_cat_alt_changed.eps",
  caption2 = "Actual Time",
  label2 = "fig:form_time_cat2",
  file2 = "../results/figures/survey_form_time_cat2_changed.eps",
  output_file = here("results", "figures", "survey_exp_form_time_cat_alt_form_time_cat2.tex")
)


generate_side_by_side_latex_figure(
  caption1 = "Reminder",
  label1 = "fig:why_reminder",
  file1 = "../results/figures/survey_why_reminder_changed.eps",
  caption2 = "Deadline",
  label2 = "fig:why_deadline",
  file2 = "../results/figures/survey_why_deadline_changed.eps",
  output_file = here("results", "figures", "survey_why_deadline_why_reminder.tex")
)


generate_side_by_side_latex_figure(
  caption1 = "On the First Day",
  label1 = "fig:why_activate_firstday",
  file1 = "../results/figures/survey_why_activate_firstday_changed.eps",
  caption2 = "After the First Day",
  label2 = "fig:why_activate_later",
  file2 = "../results/figures/survey_why_activate_later_changed.eps",
  output_file = here("results", "figures", "survey_why_activate_firstday_why_activate_later.tex")
)





