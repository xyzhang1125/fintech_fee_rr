# Run script for Fintech fee project (NBER version)

renv::restore()
# The following package(s) will be updated: (...)
# Do you want to proceed? [Y/n]: 
# Answer: 
# Y

start <- Sys.time()
# PACKAGES ------------------------------------------------------------------
# Setup packages
library(here)

# Mention where Stata is located on your PC to execute the stata file.
stata_location <- "C:/Program Files/Stata17/StataSE-64.exe"

# PRELIMINARIES -------------------------------------------------------------
# Control which scripts run
run_00_setup.R                         <- 1
run_01a_prepdata.R                     <- 0
run_01b_prepdata_updated.R             <- 0
run_02a_prepscenarios.R                <- 0 
run_03a_prep_randomization.R           <- 0
run_03b_randomization.R                <- 0
run_04a_cleanresults.R                 <- 0
run_05b_graph_colors.R                 <- 0
run_05d_survey_proc.R                  <- 0
run_06a_createdaily_panel.R            <- 0
run_06b_rolldaily.R                    <- 0
run_06c_rolldaily_adjustdates.R        <- 0 
run_06d_cleanpanel.R                   <- 0
run_06e_prepsalestrends.R              <- 0

run_07a_balance_table_data.R           <- 1
run_07c_admin_balance_tables.R         <- 0
run_07d_survey_balance_tables.R        <- 0
run_07e_admin_balance_attrition_table.R<- 0

run_08a_regressions.R                   <- 0
run_08b_panelregressions.R             <- 0

run_09_main_regression_result_table.R  <- 0
run_10_account_logins.R                <- 0 
run_11a_takeup_pooled_reminder.R       <- 0
run_11b_average_takeup_quintiles.R     <- 0
run_12_het_survey_pct_sales.R          <- 0
run_13_het_num_employee.R              <- 0
run_14_het_admin_data.R                <- 0

run_15a_survey_measure_tables.R        <- 0  
run_15b_survey_measure_tables.do        <- 0
run_15c_survey_measures_tables.R       <- 0
run_15d_survey_measures_figures.R      <- 0

run_16a_flowchart.R                     <- 0  
run_16b_timeline_figure.R               <- 0
run_17_het_months_using_tech.R         <- 0
run_18_n_emp_hist.R                    <- 0
run_19_non_adoption_bar_plot.R         <- 0
run_20a_reminder.R                        <- 0
run_20b_deadline.R                        <- 0
run_20c_form_time.R                      <- 0
run_20d_survey_panel_figures.R           <- 0
run_21b_survey_histograms.R              <- 0

run_22c_admin_graphs_output.R            <- 0
run_22d_admin_panels_output.R            <- 0

run_22e_admin_bar_plots.R                <- 0
run_23_appendix_ML_replication_extension.R <- 0 #run on a cluster as personal PC cannot handle the data load
run_24b_elasticity_tables_shortterm.R <- 0

run_25a_number_calculations_admin.R    <- 0
run_25b_number_calculations_survey.R   <- 0

run_26a_sim_barplot_initial_adoption_rho_hat.jl   <- 0
run_26b_sim_barplot_final_adoption_alpha.jl       <- 0 #run on a cluster as personal PC cannot handle the data load
run_26c_sim_barplot_final_adoption_y.jl           <- 0 #run on a cluster as personal PC cannot handle the data load
run_26d_sim_barplot_adoption_diff.jl              <- 0
run_26e_sim_heatmap_beta.jl                       <- 0
run_26f_sim_heatmap_y.jl                          <- 0 #run on a cluster as personal PC cannot handle the data load
run_26g_plot_barplots.jl                          <- 0
run_26h_plot_heatmaps.jl                          <- 0

needs_julia <- max(
  run_26a_sim_barplot_initial_adoption_rho_hat.jl   , 
  run_26b_sim_barplot_final_adoption_alpha.jl       , 
  run_26c_sim_barplot_final_adoption_y.jl           , 
  run_26d_sim_barplot_adoption_diff.jl              , 
  run_26e_sim_heatmap_beta.jl                       , 
  run_26f_sim_heatmap_y.jl                          , 
  run_26g_plot_barplots.jl                          , 
  run_26h_plot_heatmaps.jl                          
  
)

# RUN SCRIPTS ---------------------------------------------------------------
# 00_setup.R: setup directories for reproducibility
if (run_00_setup.R) source(here("scripts", "00_setup.R"), encoding = "UTF-8")
# INPUTS
# OUTPUTS
# here("proc")
# here("proc", "temp")

# 01a_prepdata.R: prepare first set of data Fintech sent about merchants (weekly to monthly format)
if (run_01a_prepdata.R) source(here("scripts", "01a_prepdata.R"), encoding = "UTF-8")
#   In this script, the weekly data is aggregated into months. The data is also prepared to be 
#   scaled by week to generate monthly values that will serve as inputs for the graphs.
# INPUTS
#   here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_WEEKLY_ACTIVITY.csv")
#   here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv")
# OUTPUTS 
#   here("proc", "weekly_grouped.rds")
#   here("proc", "monthly.rds")


# 01b_prepdata_updated.R: prepare data most recently sent over by fintech
if (run_01b_prepdata_updated.R) source(here("scripts", "01b_prepdata_updated.R"), encoding = "UTF-8")
# 	This script updates the monthly.rds produced in 01_prepdata to a completed dataset 
#   with the new data sent by fintech on 09-03-2020.
# INPUTS 
#   here("data", "Sent_20200903", "october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv")
#   here("data", "Sent_20200903", "october2020_pricing_ABtest_USERS_0903.csv")
#   here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv")
# OUTPUTS
#   here("proc", "users.rds")
#   here("proc", "monthly.rds") # Overwrite data from 01_prepdata.R


# 02a_prepscenarios.R: prepare input for sections 1 and 2 of 05_scenarios.Rmd
#   This script calculates expected costs, revenues and profits.
if (run_02a_prepscenarios.R) source(here("scripts", "02a_prepscenarios.R"), encoding = "UTF-8")
# INPUTS
#   here("proc", "monthly.rds") 
# OUTPUTS
#   here("proc", "benefits_data.rds")
#   here("proc", "benefits_data_all.rds")


# 03a_prep_randomization.R: prepare the randomization by computing baseline sales and creating panel of monthly transactions. Do power calculations (from Tiange).
if (run_03a_prep_randomization.R) source(here("scripts", "03a_prep_randomization.R"), encoding = "UTF-8")
# 	This script prepares the randomization for the fintech merchant fee reduction 
#   experiment. It creates a panel of monthly transaction data, creates 
#   identifying info for firms, and runs power calculations from pilot data to inform 
#   the relevant randomization proportions.
# INPUTS 
# 	here("data", "Sent_20200903", "october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv")
# 	here("data", "Sent_20200818", "pre_data_for_ABtest_princing_October2020", "october2020_pricing_ABtest_USERS.csv")
# 	here("data", "Sent_20200903", "october2020_pricing_ABtest_USERS_0903.csv")
# 	here("data", "AB_TestMay2019", "OpenRates_FilesSent20190523", "open_and_conversion_by_user_23052019.csv")
# 	here("data", "AB_TestMay2019", "FilesSent20191025", "results-20191023-164134.csv")
# 	here("data", "Sent_20200124", "all_users_24012020_activity.csv")
# 	here("data", "Received20190423", "iteration4", "first_new_price_abtesting_USER_INFO_iteration4.csv")
# OUTPUTS
# 	here("proc", "temp", "baseline_sales.Rds")
# 	here("proc", "temp", "optimal_ss.rds")
# 	here("proc", "temp", "07_prep_randomization.rds")

# 03b_randomization.R: conduct the randomization for the  merchant fee reduction
if (run_03b_randomization.R) source(here("scripts", "03b_randomization.R"), encoding = "UTF-8")
# 	This script creates the randomization for the fintech Merchant Fee experiment.
# INPUTS
#   here("proc", "temp", "07_prep_randomization.rds")
#   here("proc", "temp", "optimal_ss.rds")
# OUTPUTS
#   here("proc", "08_randomization.csv")
#   here("proc", "fintech_fee_randomization.xlsx")

# 04a_cleanresults.R
if (run_04a_cleanresults.R) source(here("scripts", "04a_cleanresults.R"))
# 	Clean the data that fintech sent with outcomes from the fee experiment. 
#   Create outcomes for regressions.
# INPUTS
#   here("proc", "08_randomization.csv")
#   here("data", "Sent_20201202", "daily", "daily.csv")
#   here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx")
#   here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx")
#   here("data", "Sent_20210409", "42_samplecheck (1).xlsx")
#   here("proc", "temp", "baseline_sales.Rds"))
#   here("proc", "benefits_data_all.rds")
#   here("proc", "monthly.rds")
#   here("data", "SMS_data_sent20201029", "Delivered Extra SMS UUID 10AM 29 sept.xlsx")
#   here("data", "SMS_data_sent20201029", "Delivered Extra SMS UUID 1005AM 29 sept.xlsx")
# OUTPUTS
#   here("proc", "monthly_panel.rds")
#   here("proc", "fintech_fee.csv")
#   here("proc", "fintech_fee_light.csv")

# 05b_graph_colors.R
if (run_05b_graph_colors.R) source(here("scripts", "05b_graph_colors.R"))
# 	Generate colorblind-friendly color palette for graphs.
# INPUTS
# 	(None)
# OUTPUTS
#   here("proc", "colors.csv")

# 05d_survey_proc.R
if (run_05d_survey_proc.R) source(here("scripts", "05d_survey_proc.R"))

# tryCatch({
#   source(here("scripts", "05d_survey_proc.R"), encoding = "UTF-8")
# }, error = function(e) {
#   print("Caught error:")
#   print(e)
# })
# 	Process raw survey data, prepare variables for histograms and regressions.
# INPUTS
#   here("data", "SurveyCTO_downloads", "2021_03_15", "Encuesta oferta fintech_WIDE.csv")
#   here("proc", "fintech_fee_light.csv")
#   here("data", "SurveyRecodes", "recode_7.2_why_deadline_other.csv")
#   here("data", "SurveyRecodes", "recode_8.1_why_activate_firstday.csv")
#   here("data", "SurveyRecodes", "recode_9.1_why_activate_later_other.csv")
#   here("data", "SurveyRecodes", "recode_10.3_why_reminder.csv")
#   here("data", "SurveyRecodes", "recode_10.5_reminder_feel_cat_missing.csv")
#   here("data", "SurveyRecodes", "recode_10.5_reminder_feel_cat.csv")
#   here("data", "SurveyRecodes", "recode_11.1_offer_impact_cat_missing.csv")
#   here("data", "SurveyRecodes", "recode_11.1_offer_impact_cat.csv")
# OUTPUTS
#   here("proc", "survey_all.csv")
#   here("proc", "survey_other_tables.xlsx")
#   here("proc", "survey_percent_sales.csv")
#   here("proc", "survey_successful.csv")

# 06a_create_daily_panel.R
if (run_06a_createdaily_panel.R) source(here("scripts", "06a_createdaily_panel.R"))
# 	Create daily panel dataset of transactions and sales volumes.
# INPUTS
# 	here("proc", "fintech_fee_light.csv")
# 	here("proc", "users.rds")
# 	here("data", "Sent_20210202", "02022021", "daily_02022021.csv")
# 	here("data", "Sent_20210122", "Archive_22012021", "daily.csv")
# 	here("data", "Sent_20201202", "daily", "daily.csv")
# 	here("data", "Sent_20210406", "daily_20200406.csv")
# 	here("data", "Sent_20210622", "bq-results-20210622-120549-378weo6d45d0.csv")
# 	here("data", "Sent_20210907", "bq-results-20210907-093808-j5m7o43y2wi1.csv")
# 	here("data", "Sent_20210906", "bq-results-20210906-102221-a0n8jfdtxzwd.csv")
# 	here("data", "Sent_20211005", "daily_activity_092021.csv")
# 	here("data", "Sent_20211102", "bq-results-20211102-191052-ft0ldw5edhtm.csv")
# 	here("data", "Sent_20220624", "transactions 20211101 to 20220621.csv")
# 	Files in here("data", "Sent_20221111")
# OUTPUTS
#   here("proc", "fintech_fee_daily.qs"))

# 06b_rolldaily.R
if (run_06b_rolldaily.R) source(here("scripts", "06b_rolldaily.R"))
# 	Create monthly panel dataset of transactions and sales volumes from daily data.
# INPUTS
# 	here("proc", "fintech_fee_daily.qs")
# 	here("proc", "users.rds")
# 	here("proc", "fintech_fee.csv")
# OUTPUTS
# 	here("proc", "fintech_fee_dtm_dataupdate.qs")
# 	here("proc", "fintech_fee_dtm.qs")

# 06c_rolldaily_adjustdates.R 
if (run_06c_rolldaily_adjustdates.R) source(here("scripts", "06c_rolldaily_adjustdates.R"))
# 	Convert daily to monthly/weekly data, while converting Sep 29/30 to October.
# INPUTS
# 	here("proc", "fintech_fee_daily.qs")
# 	here("proc", "users.rds")
# 	here("proc", "fintech_fee.csv")
# OUTPUTS
# 	here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
# 	here("proc", "fintech_fee_dtm_adjusteddates.qs")

# 06d_cleanpanel.R
if (run_06d_cleanpanel.R) source(here("scripts", "06d_cleanpanel.R"))
# 	Clean and append the datasets to create panel of sales.
# INPUTS
#   here("proc", "fintech_fee_light.csv")
#   here("proc", "monthly_panel.rds")
#   here("proc", "users.rds")
#   here("data", "Sent_20201105", "base_test_mensual_sept_oct.csv")
#   here("data", "Sent_20201202", "Archive", "monthly.csv")
#   here("data", "Sent_20210122", "Archive_22012021", "monthly.csv")
#   here("data", "Sent_20210202", "02022021", "monthly_02022021.csv")
#   here("data", "Sent_20210406", "monthly_20200406.csv")
#   here("data", "Sent_20201117", "base_test_semanal_20201115.csv")
#   here("data", "Sent_20201202", "Archive", "weekly.csv")
#   here("data", "Sent_20210122", "Archive_22012021", "weekly.csv")
#   here("data", "Sent_20210202", "02022021", "weekly_02022021.csv")
#   here("data", "Sent_20210406", "weekly_20200406.csv")
#   here("data", "Sent_20210622", "bq-results-20210622-120549-378weo6d45d0.csv")
# OUTPUTS
#   here("proc", "fintech_fee_monthly.qs")
#   here("proc", "fintech_fee_weekly.qs")

# 06e_prepsalestrends.R
if (run_06e_prepsalestrends.R) source(here("scripts", "06e_prepsalestrends.R"))
#   Process sales trends by week and month.
# INPUTS
#   here("proc", "fintech_fee_monthly.qs")
#   here("proc", "fintech_fee.csv")
#   here("proc", "fintech_fee_weekly.qs")
# OUTPUTS
#   here("proc", "24a_monthly_sales.csv")
#   here("proc", "24a_monthly_sales_het.csv")
#   here("proc", "24a_weekly_sales.csv")
#   here("proc", "24a_weekly_sales_het.csv")

# 07a_balance_table_data.R
if (run_07a_balance_table_data.R) source(here("scripts", "07a_balance_table_data.R"))
# 	Process data for balance tables and generate business-level dataset with 
#   data on baseline covariates and business characteristics.
# INPUTS
# 	here("data", "Banxico", "tipoCambio.xls")
# 	here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
#   here("proc", "users.rds")
# OUTPUTS
#   here("proc", "balance_table_data.csv")

# 07c_admin_balance_tables.R
if (run_07c_admin_balance_tables.R) source(here("scripts", "07c_admin_balance_tables.R"))
# Creates Table 1: Baseline Treatment Balance (Full Sample)
# INPUTS
#   here("proc", "balance_table_data.csv")
# OUTPUTS
#   here("results", "tables", "baseline_treatment_balance_fastdl.tex")
# 	here("numbers", "balance_valid_volume_usd_w5.tex")
# 	here("numbers", "balance_nr_valid_payments_w5.tex")

# 07d_survey_balance_tables.R
if (run_07d_survey_balance_tables.R) source(here("scripts", "07d_survey_balance_tables.R"))
#   Creates Table C.3: Survey Baseline Treatment Balance.
#   Creates Table C.4: Balance Between Survey Sample and Non-survey Sample.
# INPUTS
# 	here("proc", "survey_all.csv")
# 	here("proc", "survey_successful.csv")
# 	here("proc", "balance_table_data.csv")
# 	here("proc", "fintech_fee_light.csv")
# OUTPUTS
#   here("results", "tables", "survey_baseline_treatment_balance.tex")
#   here("results", "tables", "survey_balance_sample.tex")

# 07e_admin_balance_attrition_table.R
if (run_07e_admin_balance_attrition_table.R) source(here("scripts", "07e_admin_balance_attrition_table.R"))
#Table C.1: Treatment Balance (Attrition Test)
# INPUTS
# 	here("proc", "08_randomization.csv")
# 	here("data", "Sent_20201202", "daily", "daily.csv")
# 	here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx")
# 	here("data", "Sent_20210409", "42_samplecheck (1).xlsx")
# 	here("proc", "temp", "baseline_sales.Rds")
# 	here("proc", "benefits_data_all.rds")
# OUTPUTS
# 	here("results", "numbers", "initial_sample.tex")
# 	here("proc", "fintech_attrition_fee.csv")
# 	here("results", "tables", "table_attrition_fastdl.tex")


# 08a_regressions.R
if (run_08a_regressions.R) source(here("scripts", "08a_regressions.R"))
# Create the regressions laid out in the PAP.
# INPUTS
# 	here("proc", "fintech_fee.csv")
# 	here("proc", "survey_successful.csv")
#   here("proc", "balance_table_data.csv"))
# OUTPUTS
# 	here("proc", "regressions.qs")
# 	here("proc", "regs_for_figs.qs")

# 08b_panelregressions.R
if (run_08b_panelregressions.R) source(here("scripts", "08b_panelregressions.R"))
# Run the panel regressions on a monthly and weekly level.
# INPUTS
# 	here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
# OUTPUTS
# 	here("proc", "20_panel_regressions.qs")


# 09_main_regression_result_table.R
# Creates Table C.2: Main Regression Results: Average Treatment Effects
if (run_09_main_regression_result_table.R) source(here("scripts", "09_main_regression_result_table.R"))
# INPUTS
# 	here("proc", "fintech_fee_light.csv")
# OUTPUTS
#   here("results", "tables", "main_results_full.tex")

# 10_account_logins.R
if (run_10_account_logins.R) source(here("scripts", "10_account_logins.R"))
# Creates Table C.14: Account Log ins by Treatment
# INPUTS
# 	here("proc", "fintech_fee_light.csv")
#   here("data", "Sent_20210108", "daily_backoffice_data.csv")
#   here("proc", "balance_table_data.csv")
#   here("proc", "survey_successful.csv")
# OUTPUTS
#   here("results", "tables", "account_logins.tex")

# 11a_takeup_pooled_reminder.R
if (run_11a_takeup_pooled_reminder.R) source(here("scripts", "11a_takeup_pooled_reminder.R"))
# Creates Table C.5: Effects of Reminder, Announced Reminder, and Deadline on Days 1–6 vs. 7–8
# INPUTS
# 	here("proc", "fintech_fee.csv")
# OUTPUTS
#   here("results", "tables", "effect_timing_all_treatment.tex")

# 11b_average_takeup_quintiles.R
if (run_11b_average_takeup_quintiles.R) source(here("scripts", "11b_average_takeup_quintiles.R"))
#   Creates Figure C.2: Take-Up by Baseline Sales Quintiles for No-Deadline, No-Reminder Group
# INPUTS
# 	here("proc", "balance_table_data.csv")
#   here("proc", "fintech_fee_light.csv")
# OUTPUTS
#   here("results", "figures", "avg_takeup_baseline_sales_nodlnorem.eps")
#   here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_ontime.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q1.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q5.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q1_ontime.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_dlantrem_q5_ontime.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_test_q5_q1.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_test_q5_q1_ontime.tex")
#   here("results", "numbers", "avg_takeup_baseline_sales_nodlnorem_test_q5_mean.tex")
#   here("results", "numbers", "stat_n_no_dead_no_rem.tex")


# 12_het_survey_pct_sales.R 
if (run_12_het_survey_pct_sales.R) source(here("scripts", "12_het_survey_pct_sales.R"))
# Creates a table with version that combines table C.6 and table C.7 from within the NBER paper
# INPUTS
#   here("proc", "survey_successful.csv")
#   here("proc", "fintech_fee_light.csv")
# OUTPUTS
#   here("results", "numbers", "survey_num_value_transactions_abovezero.tex")
#   here("results", "numbers", "survey_num_value_transactions_missing.tex")
#   here("results", "numbers", "survey_num_value_transactions_zero.tex")
#   here("results", "numbers", "survey_num_value_transactions_does_not_know.tex")
#   here("results", "numbers", "survey_num_value_transactions_refuses_to_answer.tex")
#   here("results", "numbers", "survey_num_percent_sales_refuses_to_answer.tex")
#   here("results", "numbers", "survey_num_percent_sales_does_not_know.tex")
#   here("results", "numbers", "survey_num_percent_sales_valid_answer.tex")
#   here("results", "numbers", "survey_num_percent_sales_using_tech_full.tex")
#   here("results", "tables", "het_pct_sales_owner_takeup_ontime.tex")


# 13_het_num_employee.R 
if (run_13_het_num_employee.R) source(here("scripts", "13_het_num_employee.R"))
# Creates Table 2: Heterogeneous Effects by Number of Employees
# INPUTS
# 	here("proc", "survey_successful.csv"))
#   here("proc", "fintech_fee_light.csv"))
# OUTPUTS
#   here("results", "tables", "het_num_employee_all_inc_deadline_treat_ontime.tex")

# 14_het_admin_data.R 
if (run_14_het_admin_data.R) source(here("scripts", "14_het_admin_data.R"))
#   Creates Table C.8: Heterogeneous Effects by Firm Business Type
#   Creates a table with version that combines table C.9, table C.10, and table C.11 from within the NBER paper
# INPUTS
#   here("proc", "balance_table_data.csv")
#   here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
#   here("proc", "fintech_fee_light.csv")
# OUTPUTS
#   here("results", "numbers", "stat_pct_change_sale_median_rem.tex")
#   here("results", "numbers", "stat_pct_change_sale_median_antrem.tex")
#   here("results", "numbers", "stat_pct_change_sale_median_dl.tex")
#   here("results", "tables", "het_admin_data_buss_type_all.tex")
#   here("results", "tables", "het_admin_data_main_rem_antrem_dl_ontime.tex")

# 15a_survey_measure_tables.R
if (run_15a_survey_measure_tables.R) source(here("scripts", "15a_survey_measure_tables.R"), encoding = "UTF-8")
#   Creating .dta dataset for romanowolf analysis in .do file. 
# INPUTS
# 	here("proc", "survey_successful.csv")
# 	here("proc", "fintech_fee_light.csv")
# OUTPUTS
# 	here("proc", "survey_successful_heterogeneity.dta")

# 15b_survey_measure_tables.do
if (run_15b_survey_measure_tables.do) system2(
  command = stata_location,
  args    = c(
    "-e", "do",
    shQuote(here::here("scripts", "15b_survey_measure_tables.do"), type = "cmd")
  ),
  stdout  = TRUE,
  stderr  = TRUE
)
# Creates Table 2: Heterogeneous Effects by Number of Employees
# INPUTS
#   here("proc", "survey_successful_heterogeneity.dta"))
# OUTPUTS
#   here("results", "tables", "romano_p_antrem_unantrem.tex")
#   here("results", "tables", "romano_p_unantrem_norem.tex")

# 15c_survey_measures_tables.R
if (run_15c_survey_measures_tables.R) source(here("scripts", "15c_survey_measures_tables.R"), encoding = "UTF-8")
#   Creates Table C.12: Treatment Effect of Announced Reminder Concentrated Among Low Trust Firms.
#   Creates Table C.13: Heterogeneous Treatment Effects of Unannounced Reminder by Survey Measures
# INPUTS
# 	here("proc", "survey_successful.csv")
# 	here("proc", "fintech_fee_light.csv")
# 	here("results", "tables", "romano_p_antrem_unantrem.tex")
# 	here("results", "tables", "romano_p_unantrem_norem.tex")
# 	here("results", "tables", "het_survey_measures_antrem_late.tex")
# 	here("results", "tables", "het_survey_measures_unantrem_late.tex")
# OUTPUTS
# 	here("proc", "het_survey_measures_unantrem_late.csv")
# 	here("proc", "het_survey_measures_dl_late.csv")
# 	here("proc", "het_survey_measures_antrem_late.csv")
# 	here("results", "tables", "het_survey_measures_unantrem_late.tex")
# 	here("results", "tables", "het_survey_measures_dl_late.tex")
# 	here("results", "tables", "het_survey_measures_antrem_late.tex")
# 	here("results", "tables", "het_survey_measures_antrem_late_romano.tex")
# 	here("results", "tables", "het_survey_measures_unantrem_late_romano.tex")



# 15d_survey_measure_figures.R
if (run_15d_survey_measures_figures.R) source(here("scripts", "15d_survey_measures_figures.R"), encoding = "UTF-8")
# Creates Figure 10: Heterogeneous Effects of Announced Reminder by GSS Measures.
# Creates Figure C.16: Heterogeneous Effect of Unannounced Reminder by Survey Measures.
# INPUTS
#   here("proc", "survey_successful.csv")
#   here("proc", "het_survey_measures_unantrem_late.csv")
#   here("proc", "het_survey_measures_dl_late.csv")
#   here("proc", "het_survey_measures_antrem_late.csv")
#   here("results", "tables", "romano_p_antrem_unantrem.tex")
# OUTPUTS
#   here("results", "figures", "het_survey_measures_antrem_late_romano.eps")

# 16a_flowchart.R
if (run_16a_flowchart.R) source(here("scripts", "16a_flowchart.R"))
# Creates Figure 2: Experimental Design
# INPUTS
#   here("proc", "fintech_fee.csv"))
# OUTPUTS
#   here("results","figures","experimental_design_flowchart_paper_portrait_orientation.eps")

# 16b_timeline_figure.R  
if (run_16b_timeline_figure.R) source(here("scripts", "16b_timeline_figure.R"))
# Creates Figure 3: Timeline
# INPUTS
#   (None)
# OUTPUTS
#   here("results", "figures", "experiment_timeline.eps")

# 17_het_months_using_tech.R
if (run_17_het_months_using_tech.R) source(here("scripts", "17_het_months_using_tech.R"))
# Creates Figure 11: Effect of Announced Reminders by Length of Business Relationship
# INPUTS
# 	here("proc", "colors.csv")
# 	here("proc", "balance_table_data.csv")
# 	here("proc", "fintech_fee.csv")
# 	here("proc", "fintech_fee_light.csv")
# OUTPUTS
# 	here("results", "figures", "usage_het_trim5_nonres_late.pdf")

# 18_n_emp_hist.R
if (run_18_n_emp_hist.R) source(here("scripts", "18_n_emp_hist.R"))
# Creates Figure 4: Number of Employees
# INPUTS
#   here("data", "INEGI", "LM 833 - 174  Revisión 17 Enero 2023", "n_emp_dist_inc_2023-01-17.xlsx")
#   here("data", "INEGI", "LM 833 - 174  Revisión 17 Enero 2023", "n_emp_ini_2023-01-17.csv")
#   here("data", "INEGI", "LM 833 - 174  Revisión 17 Enero 2023", "n_firms.tex")
#   here("proc", "survey_successful.csv")
# OUTPUTS
#   here("results", "figures", "hist_survey_nemployees_comp.pdf")
#   here("results", "numbers", "n_emp_n_max.tex")
#   here("results", "numbers", "n_emp_pct_census_leq_5.tex")
#   here("results", "numbers", "n_emp_pct_survey_leq_5.tex")
#   here("results", "numbers", "n_emp_pct_census_leq_20.tex")
#   here("results", "numbers", "n_emp_pct_survey_leq_20.tex")
#   here("results", "numbers", "n_emp_pct_census_leq_150.tex")
#   here("results", "numbers", "n_emp_census_n_firms.tex")
#   here("results", "numbers", "n_emp_n_omit.tex")
#   here("results", "numbers", "n_q_n_omit.tex")

# 19_non_adoption_bar_plot.R
if (run_19_non_adoption_bar_plot.R) source(here("scripts", "19_non_adoption_bar_plot.R")) 
# 	Creates Figure C.4: Reasons Why Firms Did Not Adopt Offer
# INPUTS
#   here("proc", "colors.csv")
#   here("proc", "survey_successful.csv")
# OUTPUTS
#   here("proc", "why_firm_did_not_adopt_data.csv")
#   here("proc", "survey_non_adoption_pct.csv")
#   here("results", "figures", "survey_non_adoption_all_pct.eps")
#   here("results", "numbers", "survey_non_adoption_other.tex")
#   here("results", "numbers", "survey_non_adoption_dna.tex")
#   here("results", "numbers", "survey_non_adoption_n.tex")
#   here("results", "numbers", "survey_non_adoption_forgot.tex")
#   here("results", "numbers", "survey_non_adoption_open_email_dont_remember.tex")


# 20a_reminder.R
if (run_20a_reminder.R) source(here("scripts", "20a_reminder.R")) 
# Creates Figure C.13a: Why Firms Thought the Offer Had a Deadline and Reminder (Only reminder graph)
# Creates Figure C.18: How Firms Felt About Receiving Reminder
# Creates Figure 9: Effect of Announced Reminder on Perceived Offer Value
# INPUTS
# 	here("proc", "survey_successful.csv")
#   here("data", "Survey response mappings", "mapping_10.3_why_reminder.csv")
#   here("data", "Survey response mappings", "mapping_10.5_reminder_feel_cat.csv") 
# OUTPUTS
# 	here("proc", "survey_why_reminder_changed.qs")
# 	here("results", "figures", "survey_why_reminder_changed.eps")
# 	here("results", "numbers", "survey_num_why_reminder.tex")
# 	here("results", "numbers", "survey_pct_why_reminder_normal.tex")
# 	here("results", "numbers", "survey_pct_why_reminder_suspicious.tex")
# 	here("results", "figures", "survey_reminder_feel_changed.eps")
# 	here("results", "numbers", "survey_num_reminder_feel.tex")
# 	here("results", "numbers", "survey_pct_reminder_feel_cat_irritated.tex")
# 	here("results", "numbers", "pres_survey_pct_reminder_feel_cat_irritated.tex")
# 	here("results", "numbers", "survey_pct_reminder_feel_cat_irritated_antrem.tex")
# 	here("results", "numbers", "survey_pct_reminder_feel_cat_irritated_unantrem.tex")
# 	here("results", "numbers", "survey_pct_reminder_feel_cat_irritated_diff.tex")
# 	here("results", "figures", "survey_offer_value_change.eps")
# 	here("results", "numbers", "offer_perception_change.tex")
# 	here("results", "numbers", "coef_offer_value_change.tex")
# 	here("results", "numbers", "mean_offer_value_change_antrem.tex")
# 	here("results", "numbers", "mean_offer_value_change_unantrem.tex")

# 20b_deadline.R
if (run_20b_deadline.R) source(here("scripts", "20b_deadline.R")) 
#   Figure C.13b: Why Firms Thought the Offer Had a Deadline and Reminder (Only deadline graph)
#   Figure C.6a: Why Firms Accepted the Offer On or After the First Day
#   Figure C.6b: Why Firms Accepted the Offer On or After the First Day
# INPUTS
# 	here("proc", "survey_successful.csv")
#   here("data", "Survey response mappings", "mapping_7.2_why_deadline.csv")
#   here("data", "Survey response mappings", "mapping_8.1_why_activate_firstday.csv")
#   here("data", "Survey response mappings", "mapping_9.1_why_activate_later.csv")
# OUTPUTS
#   here("proc", "survey_why_deadline_changed.qs")
#   here("results", "figures", "survey_why_deadline_changed.eps")
#   here("results", "numbers", "survey_num_why_deadline.tex")
#   here("results", "numbers", "survey_pct_why_deadline_scarcity.tex")
#   here("proc", "survey_why_activate_firstday_changed.qs")
#   here("results", "figures", "survey_why_activate_firstday_changed.eps")
#   here("results", "numbers", "survey_num_why_activate_firstday.tex")
#   here("results", "numbers", "survey_pct_why_activate_firstday_had_time.tex")
#   here("proc", "survey_why_activate_later_changed.qs")
#   here("results", "figures", "survey_why_activate_later_changed.eps")
#   here("results", "numbers", "survey_num_why_activate_later.tex")
#   here("results", "numbers", "survey_pct_why_activate_later_too_busy.tex")
#   here("results", "numbers", "survey_pct_why_activate_later_discuss_think_offer.tex")
#   here("results", "numbers", "survey_pct_why_activate_later_discuss_offer_antrem.tex")
#   here("results", "numbers", "survey_pct_why_activate_later_discuss_offer_unantrem.tex")
#   here("results", "numbers", "survey_p_why_activate_later_discuss_offer.tex")


# 20c_form_time.R
if (run_20c_form_time.R) source(here("scripts", "20c_form_time.R")) 
#   Creates Figure C.3a: Self-Reported Time Cost of Accepting the Offer (Expected Time graph)
#   Creates Figure C.3b: Self-Reported Time Cost of Accepting the Offer (Actual Time graph)
#INPUTS
# 	here("proc", "survey_successful.csv")
# 	here("data", "Survey response mappings", "mapping_6.12_form_time_cat_alt.csv")
# 	here("data", "Survey response mappings", "mapping_6.22_exp_form_time_cat_alt.csv")
# 	here("proc", "survey_non_adoption_pct.csv")
#OUTPUTS
# 	here("proc", "survey_form_time_cat2_changed.qs")
# 	here("results", "figures", "survey_form_time_cat2_changed.eps")
# 	here("results", "numbers", "survey_num_form_time_cat_alt.tex")
# 	here("proc", "survey_exp_form_time_cat_alt_changed.qs")
# 	here("results", "figures", "survey_exp_form_time_cat_alt_changed.eps")
# 	here("results", "numbers", "survey_num_exp_form_time_cat_alt.tex")
# 	here("results", "numbers", "survey_non_adoption_too_long.tex")

# 20d_survey_panel_figures.R
if (run_20d_survey_panel_figures.R) source(here("scripts", "20d_survey_panel_figures.R")) 
#   Creates .tex files that combines the two subfigures each for figure C.3, figure C.6, and figure C.13.
#INPUTS
#   (None)
#OUTPUTS
# 	here("results", "figures", "survey_exp_form_time_cat_alt_form_time_cat2.tex")
# 	here("results", "figures", "survey_why_deadline_why_reminder.tex")
# 	here("results", "figures", "survey_why_activate_firstday_why_activate_later.tex")


# 21b_survey_histograms.R
if (run_21b_survey_histograms.R) source(here("scripts", "21b_survey_histograms.R"))
#   Creates Figure C.5: Percent of Sales Made Through FinTech Provider in Prior Week
#   Creates Figure C.17: Difference in Pre-Treatment Actual Fee and Perceived Fee
# INPUTS
# 	here("proc", "survey_successful.csv")
# OUTPUTS
# 	here("results", "figures", "survey_percent_sales.eps")
# 	here("results", "numbers", "survey_num_data_percent_sales.tex")
# 	here("results", "numbers", "survey_num_data_percent_sales_response_sample.tex")
# 	here("results", "figures", "survey_fee_diff.eps")
# 	here("results", "numbers", "fee_prior_receive_offer.tex")


# 22c_admin_graphs_output.R
if (run_22c_admin_graphs_output.R) source(here("scripts", "22c_admin_graphs_output.R"))
# INPUTS
# OUTPUTS


# 22d_admin_panels_output.R
if (run_22d_admin_panels_output.R) source(here("scripts", "22d_admin_panels_output.R"))
# INPUTS
# OUTPUTS


# 22e_admin_bar_plots.R
if (run_22e_admin_bar_plots.R) source(here("scripts", "22e_admin_bar_plots.R"))
# 	Creates Figure C.7: Take-up by Treatment Arm
# INPUTS
# 	- here("proc", "regs_for_figs.qs")
#   - here("proc", "fintech_fee_light.csv")
#   - here("proc", "colors.csv")
# OUTPUTS
#   - here("results", "numbers", "stat_n_no_control_no_24.tex")
#   - here("results", "figures", "cum_takeup_treat_fee_barplot.eps")
#   - here("results", "figures", "cum_takeup_treat_fee_barplot.pdf")

# 23_appendix_ML_replication_extension.R (Run this on KLC)
if (run_23_appendix_ML_replication_extension.R) source(here("scripts", "23_appendix_ML_replication_extension.R"))
# 	Figure D.1: Percent of OOS RMSEs lower than not winsorized or standardized (1,1) OOS RMS
# INPUTS
# 	here("proc", "balance_table_data.csv")
# 	here("proc", "fintech_fee_light.csv")
# 	here("proc", "rmse_ridge_nowinsor_std_agemeanimpute.csv")
# 	here("proc", "rmse_ridge_winsortopX1_std_agemeanimpute.csv")
# 	here("proc", "rmse_ridge_winsortopX1_nostd_agemeanimpute.csv")
# 	here("proc", "rmse_ridge_nowinsor_nostd_agemeanimpute.csv")
# OUTPUTS
# 	here("proc", "rmse_ridge_winsortopX1_std_agemeanimpute.csv")
# 	here("proc", "rmse_ridge_nowinsor_std_agemeanimpute.csv")
# 	here("proc", "rmse_ridge_winsortopX1_nostd_agemeanimpute.csv")
# 	here("proc", "rmse_ridge_nowinsor_nostd_agemeanimpute.csv")
# 	here("results", "figures", "heatmap_percent_betterrmse.eps")
# 	here("results", "numbers", "percent_to_number_heatmap_4_1.tex")
# 	here("results", "numbers", "percent_heatmap_4_1.tex")
# 	here("results", "numbers", "length_25.tex")
# 	here("results", "numbers", "length_med.tex")
# 	here("results", "numbers", "length_mean.tex")
# 	here("results", "numbers", "length_75.tex")
# 	here("results", "numbers", "percent_negative_25.tex")
# 	here("results", "numbers", "percent_negative_med.tex")
# 	here("results", "numbers", "percent_negative_mean.tex")
# 	here("results", "numbers", "percent_negative_75.tex")


# 24a_elasticity_tables_functions.R (Script doesn't need to be run here)
# Define functions for elasticity tables.
# INPUTS
#   (None)
# OUTPUTS
#   (None)

# 24b_elasticity_tables_shortterm.R 
if (run_24b_elasticity_tables_shortterm.R) source(here("scripts", "24b_elasticity_tables_shortterm.R"))
# Creates Table 3: Monthly Sales Elasticity
# INPUTS
#   here("proc", "07b_panel_regressions.qs")
#   here("proc", "fintech_fee_dtm_adjusteddates_dataupdate.qs")
#   here("proc", "fintech_fee_light.csv")
# OUTPUTS
#   Numbers in here("results", "numbers")
#   here("results", "tables", "monthly_itt_tot.tex")

# 25a_number_calculations_admin.R
if (run_25a_number_calculations_admin.R) source(here("scripts", "25a_number_calculations_admin.R"))
#   Calculate numbers and statistics for admin data and export to tex files.
# INPUTS
# 	here("proc", "fintech_fee_light.csv")
# 	here("proc", "fintech_fee.csv")
# 	here("proc", "users.rds")
# 	here("proc", "balance_table_data.csv")
# 	here("proc", "regs_for_figs.qs")
# 	here("data", "Sent_20201020", "Reporte cambio de comision_20201019.xlsx")
# OUTPUTS
# 	here("results", "numbers", "stat_sample_size.tex")
# 	here("results", "numbers", "stat_sample_size_abstract.tex")
# 	here("results", "numbers", "stat_number_control.tex")
# 	here("results", "numbers", "stat_pct_fem.tex")
# 	here("results", "numbers", "stat_pct_fem_rem.tex")
# 	here("results", "numbers", "stat_pct_fem_antrem.tex")
# 	here("results", "numbers", "stat_pct_fem_dl.tex")
# 	here("results", "numbers", "stat_age_median_rem.tex")
# 	here("results", "numbers", "stat_age_median_antrem.tex")
# 	here("results", "numbers", "stat_age_median_dl.tex")
# 	here("results", "numbers", "stat_pct_small_retailer.tex")
# 	here("results", "numbers", "stat_pct_prof.tex")
# 	here("results", "numbers", "avg_takeup_open_email_norem.tex")
# 	here("results", "numbers", "avg_takeup_open_email_rem.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_3_d1.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_2.75_d1.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_3_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_2.75_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_2.75_d8_pct.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_long.tex")
# 	here("results", "numbers", "mean_accepted_offer_2.75.tex")
# 	here("results", "numbers", "mean_accepted_offer_3.00.tex")
# 	here("results", "numbers", "coef_accepted_offer_rem_d7.tex")
# 	here("results", "numbers", "coef_accepted_offer_rem_d8.tex")
# 	here("results", "numbers", "pres_coef_accepted_offer_rem_d8.tex")
# 	here("results", "numbers", "mean_accepted_offer_norem.tex")
# 	here("results", "numbers", "pres_mean_accepted_offer_norem.tex")
# 	here("results", "numbers", "pct_inc_accepted_offer_rem_d8.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_rem_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_antrem_d7.tex")
# 	here("results", "numbers", "pct_inc_accepted_offer_antrem_d7.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d7.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d7_abstract.tex")
# 	here("results", "numbers", "pct_inc_accepted_offer_antrem_d8.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d8.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_antrem_d8_abstract.tex")
# 	here("results", "numbers", "coef_accepted_offer_antrem_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_unantrem_d8.tex")
# 	here("results", "numbers", "pct_inc_accepted_offer_unantrem_d8.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_unantrem_d8.tex")
# 	here("results", "numbers", "pres_pct_inc_accepted_offer_unantrem_d8_abstract.tex")
# 	here("results", "numbers", "coef_accepted_offer_dl_d1.tex")
# 	here("results", "numbers", "coef_accepted_offer_dl_d1_abs.tex")
# 	here("results", "numbers", "p_value_dl_d1.tex")
# 	here("results", "numbers", "coef_accepted_offer_fee_late_dl.tex")
# 	here("results", "numbers", "coef_effect_dl_small.tex")
# 	here("results", "numbers", "coef_effect_dl_small_abstract.tex")
# 	here("results", "numbers", "coef_effect_dl_small_pp.tex")
# 	here("results", "numbers", "coef_effect_dl_lower_fee.tex")
# 	here("results", "numbers", "coef_effect_dl_lower_fee_pp.tex")
# 	here("results", "numbers", "pct_dl_lower_fee.tex")
# 	here("results", "numbers", "p_value_dl_fee_interaction.tex")
# 	here("results", "numbers", "p_value_dl_fsize_interaction.tex")
# 	here("results", "numbers", "p_value_dl.tex")
# 	here("results", "numbers", "coef_accepted_offer_t2_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_t3_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_t4_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_t5_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_t6_d8.tex")
# 	here("results", "numbers", "coef_accepted_offer_t7_d8.tex")
# 	here("results", "numbers", "coef_takeup_dl.tex")
# 	here("results", "numbers", "catch_up_nodl_days.tex")
# 	here("results", "numbers", "catch_up_nodl_fee2.75_days.tex")
# 	here("results", "numbers", "catch_up_nodl_fee3_days.tex")
# 	here("results", "numbers", "catch_up_nodl_fee3_weeks.tex")
# 	here("results", "numbers", "catch_up_nodl_abovemed_days.tex")
# 	here("results", "numbers", "catch_up_nodl_belowmed_days.tex")
# 	here("results", "numbers", "catch_up_nodl_belowmed_weeks.tex") 
# 	here("results", "numbers", "fintech_marketshare_total.tex")
# 	here("results", "numbers", "fintech_marketshare_mpos.tex")
# 	here("results", "numbers", "stat_pct_open_terms.tex")


# 25b_number_calculations_survey.R
if (run_25b_number_calculations_survey.R) source(here("scripts", "25b_number_calculations_survey.R"))
#   Calculate numbers and statistics for survey data and export to tex files.
# INPUTS
#   here("proc", "survey_successful.csv")
#   here("proc", "fintech_fee_light.csv")
#   here("results", "tables", "romano_p_antrem_unantrem.tex")
# OUTPUTS
#   here("results", "numbers", "survey_sample_size.tex")
#   here("results", "numbers", "trust_unantrem_norem.tex")
#   here("results", "numbers", "trust_unantrem_antrem.tex")
#   here("results", "numbers", "trust_antrem_no_rem.tex")
#   here("results", "numbers", "survey_pct_owner_receives_emails.tex")
#   here("results", "numbers", "pres_survey_pct_owner_receives_emails.tex")
#   here("results", "numbers", "survey_nb_employees_mean.tex")
#   here("results", "numbers", "survey_nb_employees_median.tex")
#   here("results", "numbers", "survey_nb_employees_sd.tex")
#   here("results", "numbers", "survey_pct_budget_costs.tex")
#   here("results", "numbers", "survey_value_transactions_mean.tex")
#   here("results", "numbers", "survey_value_transactions_median.tex")
#   here("results", "numbers", "survey_value_transactions_sd.tex")
#   here("results", "numbers", "survey_percent_sales_mean.tex")
#   here("results", "numbers", "survey_percent_sales_median.tex")
#   here("results", "numbers", "survey_percent_sales_sd.tex")
#   here("results", "numbers", "survey_fee_diff_mean.tex")
#   here("results", "numbers", "survey_fee_diff_median.tex")
#   here("results", "numbers", "survey_fee_diff_sd.tex")
#   here("results", "numbers", "survey_pct_recall_old_fee_4.tex")
#   here("results", "numbers", "survey_pct_fee_diff_0.tex")
#   here("results", "numbers", "survey_pct_firstemail_recall.tex")
#   here("results", "numbers", "survey_n_firstemail_recall.tex")
#   here("results", "numbers", "survey_pct_recall_sms.tex")
#   here("results", "numbers", "survey_n_recall_sms.tex")
#   here("results", "numbers", "survey_pct_read_sms.tex")
#   here("results", "numbers", "survey_n_read_sms.tex")
#   here("results", "numbers", "survey_pct_click_recall.tex")
#   here("results", "numbers", "survey_n_click_recall.tex")
#   here("results", "numbers", "survey_pct_accept_offer_recall.tex")
#   here("results", "numbers", "survey_n_accept_offer_recall.tex")
#   here("results", "numbers", "survey_offer_effect_time_mean.tex")
#   here("results", "numbers", "survey_offer_effect_time_median.tex")
#   here("results", "numbers", "survey_offer_effect_time_sd.tex")
#   here("results", "numbers", "survey_pct_noticed_deadline.tex")
#   here("results", "numbers", "survey_n_noticed_deadline.tex")
#   here("results", "numbers", "survey_pct_reminder_recall.tex")
#   here("results", "numbers", "survey_n_reminder_recall.tex")
#   here("results", "numbers", "survey_pct_open_reminder_recall.tex")
#   here("results", "numbers", "survey_n_open_reminder_recall.tex")
#   here("results", "numbers", "survey_pct_offer_value_change.tex")
#   here("results", "numbers", "survey_n_offer_value_change.tex")
#   here("results", "numbers", "survey_n_a_reminder_takeup.tex")
#   here("results", "numbers", "survey_pct_a_reminder_takeup_0.tex")
#   here("results", "numbers", "pres_survey_pct_a_reminder_takeup_0.tex")
#   here("results", "numbers", "survey_pct_a_reminder_takeup_1.tex")
#   here("results", "numbers", "survey_num_a_reminder_takeup_1.tex")
#   here("results", "numbers", "survey_num_a_reminder_takeup_calculate.tex")
#   here("results", "numbers", "p_value_het_trust_antrem_interaction_late.tex")
#   here("results", "numbers", "p_value_het_overconfidence_antrem_interaction_late.tex")
#   here("results", "numbers", "romano_p_value_het_trust_antrem_interaction_late.tex")
#   here("results", "numbers", "romano_p_value_het_overconfidence_antrem_interaction_late.tex")


##################### Running JULIA scripts ###############################
#26a_sim_barplot_initial_adoption_rho_hat.jl
if (run_26a_sim_barplot_initial_adoption_rho_hat.jl) julia_source(here("scripts", "26a_sim_barplot_initial_adoption_rho_hat.jl"))
# Creates .csv file to be used to create the combined plot
# INPUTS
#  (None)
# OUTPUTS
#   here("results", "figures", "data_barplot_initial_adoption_rho_hat.csv")

#26b_sim_barplot_final_adoption_alpha.jl
if (run_26b_sim_barplot_final_adoption_alpha.jl) julia_source(here("scripts", "26b_sim_barplot_final_adoption_alpha.jl"))
# Creates .csv file to be used to create the combined plot
# INPUTS
#  (None)
# OUTPUTS
#   here("results", "figures", "data_barplot_final_adoption_alpha.csv")

#26c_sim_barplot_final_adoption_y.jl
if (run_26c_sim_barplot_final_adoption_y.jl) julia_source(here("scripts", "26c_sim_barplot_final_adoption_y.jl"))
# Creates .csv file to be used to create the combined plot
# INPUTS
#  (None)
# OUTPUTS
#  here("results", "figures", "data_barplot_final_adoption_y.csv")

#26d_sim_barplot_adoption_diff.jl
if (run_26d_sim_barplot_adoption_diff.jl) julia_source(here("scripts", "26d_sim_barplot_adoption_diff.jl"))
# Creates .csv file to be used to create the combined plot
# INPUTS
#  (None)
# OUTPUTS
#  here("results", "figures", "data_barplot_adoption_diff.csv")

#26e_sim_heatmap_beta.jl
if (run_26e_sim_heatmap_beta.jl) julia_source(here("scripts", "26e_sim_heatmap_beta.jl"))
# Creates .csv file to be used to create the combined plot
# INPUTS
#  (None)
# OUTPUTS
#  here("results", "figures", "data_heatmap_beta_t1.csv")
#  here("results", "figures", "data_heatmap_beta_t2.csv")
#  here("results", "figures", "data_heatmap_beta_t6.csv")
#  here("results", "figures", "data_heatmap_beta_t8.csv")

#26f_sim_heatmap_y.jl
if (run_26f_sim_heatmap_y.jl) julia_source(here("scripts", "26f_sim_heatmap_y.jl"))
# Creates .csv file to be used to create the combined plot
# INPUTS
#  (None)
# OUTPUTS
#  here("results", "figures", "data_heatmap_y_t1.csv")
#  here("results", "figures", "data_heatmap_y_t2.csv")
#  here("results", "figures", "data_heatmap_y_t6.csv")
#  here("results", "figures", "data_heatmap_y_t8.csv")


#26g_plot_barplots.jl
if (run_26g_plot_barplots.jl) julia_source(here("scripts", "26g_plot_barplots.jl"))
# Creates Figure B.1: Proportion of Simulations where Predictions 1, 3, 4, and 6 Hold
# INPUTS
#  here("results", "figures", "data_barplot_final_adoption_y.csv")
#  here("results", "figures", "data_barplot_initial_adoption_rho_hat.csv")
#  here("results", "figures", "data_barplot_final_adoption_alpha.csv")
#  here("results", "figures", "data_barplot_adoption_diff.csv")
# OUTPUTS
#  here("results", "figures", "barplot_combined.eps")
#  here("results", "numbers", "barplot_final_adoption_y_min_beta_hat.tex")
#  here("results", "numbers", "barplot_final_adoption_y_max_beta_hat.tex")
#  here("results", "numbers", "barplot_final_adoption_y_argmin_ratio.tex")
#  here("results", "numbers", "barplot_final_adoption_y_min_ratio.tex")
#  here("results", "numbers", "barplot_initial_adoption_rho_hat_min_beta_hat.tex")
#  here("results", "numbers", "barplot_initial_adoption_rho_hat_max_beta_hat.tex")
#  here("results", "numbers", "barplot_initial_adoption_rho_hat_argmin_ratio.tex")
#  here("results", "numbers", "barplot_initial_adoption_rho_hat_min_ratio.tex")
#  here("results", "numbers", "barplot_final_adoption_alpha_min_beta_hat.tex")
#  here("results", "numbers", "barplot_final_adoption_alpha_max_beta_hat.tex")
#  here("results", "numbers", "barplot_final_adoption_alpha_argmin_ratio.tex")
#  here("results", "numbers", "barplot_final_adoption_alpha_min_ratio.tex")
#  here("results", "numbers", "barplot_adoption_diff_min_beta_hat.tex")
#  here("results", "numbers", "barplot_adoption_diff_max_beta_hat.tex")
#  here("results", "numbers", "barplot_adoption_diff_argmin_ratio.tex")
#  here("results", "numbers", "barplot_adoption_diff_min_ratio.tex")

#26h_plot_heatmaps.jl
if (run_26h_plot_heatmaps.jl) julia_source(here("scripts", "26h_plot_heatmaps.jl"))
# Creates Figure B.2: Proportion of Simulations where Prediction 7 Holds for Various T in Deadline Arm.
# Creates Figure B.3: Proportion of Simulations where Prediction 5 Holds for Various T in Deadline Arm.
# INPUTS
#  here("results", "figures", "data_heatmap_beta_t1.csv")
#  here("results", "figures", "data_heatmap_beta_t2.csv")
#  here("results", "figures", "data_heatmap_beta_t6.csv")
#  here("results", "figures", "data_heatmap_beta_t8.csv")
#  here("results", "figures", "data_heatmap_y_t1.csv")
#  here("results", "figures", "data_heatmap_y_t2.csv")
#  here("results", "figures", "data_heatmap_y_t6.csv")
#  here("results", "figures", "data_heatmap_y_t8.csv")
# OUTPUTS
#  here("results", "figures", "heatmap_beta_combined.eps")
#  here("results", "figures", "heatmap_y_combined.eps")
#  here("results", "numbers", "heatmap_beta_t8_max_beta_hat.tex")
#  here("results", "numbers", "heatmap_y_t8_max_beta_hat.tex")



# 
end <- Sys.time()
message("Total elapsed time: ", round(difftime(end, start, units = "secs"), 2), " seconds")
