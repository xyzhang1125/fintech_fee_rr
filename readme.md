# README

## Replication Project: iZettle Merchant Fee Reduction Experiment

### Overview
This repository contains all scripts and data necessary to replicate the analysis of the iZettle merchant fee reduction experiment. Scripts are organized numerically; for each, all INPUT files and OUTPUT files are listed.

### Prerequisites
- **R** (version 4.0+)
- R packages: `here`, `dplyr`, `readr`, `ggplot2`, `data.table`, `lubridate`, plus others per script.
- Directory structure:
  ```
  ├── data/       # Raw inputs
  ├── scripts/    # R scripts
  ├── proc/       # Processed data outputs
  ├── results/    # Tables (.tex) & figures (.eps, .pdf)
  └── README.md   # This file
  ```

### Usage
1. Clone repository.
2. Place raw files under `data/` matching paths below.
3. In R, set `run_*.R <- TRUE` then:
   ```r
   if (run_00_setup.R) source(here("scripts","00_setup.R"),encoding="UTF-8")
   ```
4. Execute scripts in ascending order.

## Scripts and File Lists

### 00_setup.R
- **Inputs**: None
- **Outputs**:
  - `proc/` (directory)
  - `proc/temp/` (directory)

### 01a_prepdata.R
- **Inputs**:
  - `data/Sent_20200818/pre_data_for_ABtest_princing_October2020/october2020_pricing_ABtest_WEEKLY_ACTIVITY.csv`
  - `data/Sent_20200818/pre_data_for_ABtest_princing_October2020/october2020_pricing_ABtest_USERS.csv`
- **Outputs**:
  - `proc/weekly_grouped.rds`
  - `proc/monthly.rds`

### 01b_prepdata_updated.R
- **Inputs**:
  - `data/Sent_20200903/october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv`
  - `data/Sent_20200903/october2020_pricing_ABtest_USERS_0903.csv`
  - `data/Sent_20200818/pre_data_for_ABtest_princing_October2020/october2020_pricing_ABtest_USERS.csv`
- **Outputs**:
  - `proc/users.rds`
  - `proc/monthly.rds` (overwritten)

### 02a_prepscenarios.R
- **Inputs**:
  - `proc/monthly.rds`
- **Outputs**:
  - `proc/benefits_data.rds`
  - `proc/benefits_data_all.rds`

### 02b_prepscenarios.R (not run by default)
- **Inputs**:
  - `proc/benefits_data_all.rds`
- **Outputs**:
  - `proc/benefits_ve_all.rds`
  - `proc/benefits_ve.rds`

### 03a_prep_randomization.R
- **Inputs**:
  - `data/Sent_20200903/october2020_pricing_ABtest_MONTHLY_ACTIVITY_0903.csv`
  - `data/Sent_20200818/.../october2020_pricing_ABtest_USERS.csv`
  - `data/AB_TestMay2019/OpenRates_FilesSent20190523/open_and_conversion_by_user_23052019.csv`
  - `data/AB_TestMay2019/FilesSent20191025/results-20191023-164134.csv`
  - `data/Sent_20200124/all_users_24012020_activity.csv`
  - `data/Received20190423/iteration4/first_new_price_abtesting_USER_INFO_iteration4.csv`
- **Outputs**:
  - `proc/temp/baseline_sales.Rds`
  - `proc/temp/optimal_ss.rds`
  - `proc/temp/07_prep_randomization.rds`

### 03b_randomization.R
- **Inputs**:
  - `proc/temp/07_prep_randomization.rds`
  - `proc/temp/optimal_ss.rds`
- **Outputs**:
  - `proc/08_randomization.csv`
  - `proc/izettle_fee_randomization.xlsx`

### 04a_cleanresults.R
- **Inputs**:
  - `proc/08_randomization.csv`
  - `data/Sent_20201202/daily/daily.csv`
  - `data/Sent_20201020/Reporte cambio de comision_20201019.xlsx`
  - `data/Sent_20210409/42_samplecheck (1).xlsx`
  - `data/SMS_data_sent20201029/Delivered Extra SMS UUID 10AM 29 sept.xlsx`
  - `data/SMS_data_sent20201029/Delivered Extra SMS UUID 1005AM 29 sept.xlsx`
  - `proc/temp/baseline_sales.Rds`
  - `proc/benefits_data_all.rds`
  - `proc/monthly.rds`
- **Outputs**:
  - `proc/monthly_panel.rds`
  - `proc/fintech_fee.csv`
  - `proc/fintech_fee_light.csv`

### 04b_cleanresults_restore.R (not run by default)
- **Inputs**: same as 04a_cleanresults
- **Outputs**:
  - `proc/izettle_fee_restore.csv`

### 05a_survey_functions.R
- **Inputs**: None
- **Outputs**: None

### 05b_graph_colors.R
- **Inputs**: None
- **Outputs**:
  - `proc/colors.csv`

### 05c_barplot_functions.R
- **Inputs**: None
- **Outputs**: None

### 05d_survey_proc.R
- **Inputs**:
  - `data/SurveyCTO_downloads/2021_03_15/Encuesta oferta iZettle_WIDE.csv`
  - `proc/fintech_fee_light.csv`
  - `data/SurveyRecodes/recode_7.2_why_deadline_other.csv`
  - `data/SurveyRecodes/recode_8.1_why_activate_firstday.csv`
  - `data/SurveyRecodes/recode_9.1_why_activate_later_other.csv`
  - `data/SurveyRecodes/recode_10.3_why_reminder.csv`
  - `data/SurveyRecodes/recode_10.4_reminder_feel_cat_missing.csv`
  - `data/SurveyRecodes/recode_10.4_reminder_feel_cat.csv`
  - `data/SurveyRecodes/recode_11.1_offer_impact_cat_missing.csv`
  - `data/SurveyRecodes/recode_11.1_offer_impact_cat.csv`
- **Outputs**:
  - `proc/survey_all.csv`
  - `proc/survey_other_tables.xlsx`
  - `proc/survey_percent_sales.csv`
  - `proc/survey_successful.csv`

### 06a_create_daily_panel.R
- **Inputs**:
  - `proc/fintech_fee_light.csv`
  - `proc/users.rds`
  - daily CSVs under `data/Sent_*` from 2020-02-02 to 2022-06-24
- **Outputs**:
  - `proc/fintech_fee_daily.qs`

### 06b_rolldaily.R
- **Inputs**:
  - `proc/fintech_fee_daily.qs`
  - `proc/users.rds`
  - `proc/fintech_fee.csv`
- **Outputs**:
  - `proc/fintech_fee_dtm_dataupdate.qs`
  - `proc/fintech_fee_dtm.qs`

### 06d_cleanpanel.R
- **Inputs**:
  - `proc/fintech_fee_light.csv`
  - `proc/monthly_panel.rds`
  - `proc/users.rds`
  - archived monthly/weekly CSVs under `data/Sent_*`
- **Outputs**:
  - `proc/fintech_fee_monthly.qs`
  - `proc/fintech_fee_weekly.qs`

### 06e_prepsalestrends.R
- **Inputs**:
  - `proc/fintech_fee_monthly.qs`
  - `proc/fintech_fee.csv`
  - `proc/fintech_fee_weekly.qs`
- **Outputs**:
  - `proc/24a_monthly_sales.csv`
  - `proc/24a_monthly_sales_het.csv`
  - `proc/24a_weekly_sales.csv`
  - `proc/24a_weekly_sales_het.csv`

### 07a_regressions.R
- **Inputs**:
  - `proc/fintech_fee.csv`
  - `proc/survey_successful.csv`
  - `proc/balance_table_data.csv`
- **Outputs**:
  - `proc/regressions.qs`
  - `proc/regs_for_figs.qs`

### 07b_panelregressions.R
- **Inputs**:
  - `proc/fintech_fee_dtm_adjusteddates_dataupdate.qs`
- **Outputs**:
  - `proc/20_panel_regressions.qs`

### 08a_balance_table_data.R
- **Inputs**:
  - `data/Banxico/tipoCambio.xls`
  - `proc/fintech_fee_dtm_adjusteddates_dataupdate.qs`
  - `proc/users.rds`
- **Outputs**:
  - `proc/balance_table_data.csv`

### 08b_balance_table_functions.R
- **Inputs**: None
- **Outputs**: None

### 08c_admin_balance_tables.R
- **Inputs**:
  - `proc/balance_table_data.csv`
- **Outputs**:
  - `results/tables/baseline_treatment_balance_fastdl.tex`
  - `numbers/balance_valid_volume_usd_w5.tex`
  - `numbers/balance_nr_valid_payments_w5.tex`

### 08d_survey_balance_tables.R
- **Inputs**:
  - `proc/survey_all.csv`
  - `proc/survey_successful.csv`
  - `proc/balance_table_data.csv`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - `results/tables/survey_baseline_treatment_balance.tex`
  - `results/tables/survey_balance_sample.tex`

### 08e_admin_balance_attrition_table.R
- **Inputs**:
  - `proc/08_randomization.csv`
  - `data/Sent_20201202/daily/daily.csv`
  - `data/Sent_20201020/Reporte cambio de comision_20201019.xlsx`
  - `data/Sent_20210409/42_samplecheck (1).xlsx`
  - `proc/temp/baseline_sales.Rds`
  - `proc/benefits_data_all.rds`
- **Outputs**:
  - `results/numbers/initial_sample.tex`
  - `proc/izettle_attrition_fee.csv`
  - `results/tables/table_attrition_fastdl.tex`

### 09_main_regression_result_table.R
- **Inputs**:
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - `results/tables/main_results_full.tex`

### 10_account_logins.R
- **Inputs**:
  - `proc/fintech_fee_light.csv`
  - `data/Sent_20210108/daily_backoffice_data.csv`
  - `proc/balance_table_data.csv`
  - `proc/survey_successful.csv`
- **Outputs**:
  - `results/tables/account_logins.tex`

### 11a_takeup_pooled_reminder.R
- **Inputs**:
  - `proc/fintech_fee.csv`
- **Outputs**:
  - `results/tables/effect_timing_all_treatment.tex`

### 11b_average_takeup_quintiles.R
- **Inputs**:
  - `proc/balance_table_data.csv`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - `results/figures/avg_takeup_baseline_sales_nodlnorem.eps`
  - multiple `results/numbers/*.tex` files

### 12_het_survey_pct_sales.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - multiple `results/numbers/*.tex`
  - `results/tables/het_pct_sales_owner_takeup_ontime.tex`

### 13_het_num_employee.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - `results/tables/het_num_employee_all_inc_deadline_treat_ontime.tex`

### 14_het_admin_data.R
- **Inputs**:
  - `proc/balance_table_data.csv`
  - `proc/fintech_fee_dtm_adjusteddates_dataupdate.qs`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - multiple `results/numbers/*.tex`
  - multiple `results/tables/*.tex`

### 15a_survey_measure_tables.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - `proc/fintech_fee_light.csv`
  - `proc/survey_successful_heterogeneity.dta`
  - existing `results/tables/romano_*.tex`
- **Outputs**:
  - `proc/*.csv`
  - updated `results/tables/*.tex`

### 15b_survey_measure_tables.do
- **Inputs**:
  - `proc/survey_successful_heterogeneity.dta`
- **Outputs**:
  - `results/tables/romano_*.tex`

### 15c_survey_measure_figures.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - multiple `proc/*.csv`
  - `results/tables/romano_*.tex`
- **Outputs**:
  - `results/figures/het_survey_measures_antrem_late_romano.eps`

### 16a_flowchart.R
- **Inputs**:
  - `proc/fintech_fee.csv`
- **Outputs**:
  - `results/figures/experimental_design_flowchart_paper_portrait_orientation.eps`

### 16b_timeline_figure.R
- **Inputs**: None
- **Outputs**:
  - `results/figures/experiment_timeline.eps`

### 17_het_months_using_tech.R
- **Inputs**:
  - `proc/colors.csv`
  - `proc/balance_table_data.csv`
  - `proc/fintech_fee.csv`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - `results/figures/usage_het_trim5_nonres_late.pdf`

### 18_n_emp_hist.R
- **Inputs**:
  - `data/INEGI/LM 833 - 174 Revisión 17 Enero 2023/n_emp_dist_inc_2023-01-17.xlsx`
  - `data/INEGI/.../n_emp_ini_2023-01-17.csv`
  - `data/INEGI/.../n_firms.tex`
  - `proc/survey_successful.csv`
- **Outputs**:
  - `results/figures/hist_survey_nemployees_comp.pdf`
  - multiple `results/numbers/*.tex`

### 19_non_adoption_bar_plot.R
- **Inputs**:
  - `proc/colors.csv`
  - `proc/survey_successful.csv`
- **Outputs**:
  - `proc/why_firm_did_not_adopt_data.csv`
  - `proc/survey_non_adoption_pct.csv`
  - `results/figures/survey_non_adoption_all_pct.eps`
  - multiple `results/numbers/*.tex`

### 20a_reminder.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - mapping CSVs under `data/Survey response mappings/`
- **Outputs**:
  - `proc/survey_why_reminder_changed.qs`
  - `results/figures/*.eps`
  - multiple `results/numbers/*.tex`

### 20b_deadline.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - mapping CSVs under `data/Survey response mappings/`
- **Outputs**:
  - `proc/survey_why_deadline_changed.qs`
  - `proc/survey_why_activate_firstday_changed.qs`
  - `proc/survey_why_activate_later_changed.qs`
  - `results/figures/*.eps`
  - multiple `results/numbers/*.tex`

### 20c_form_time.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - mapping CSVs under `data/Survey response mappings/`
  - `proc/survey_non_adoption_pct.csv`
- **Outputs**:
  - `proc/*.qs`
  - `results/figures/*.eps`
  - `results/numbers/*.tex`

### 20d_survey_panel_figures.R
- **Inputs**: None
- **Outputs**:
  - combined panel `.tex` files under `results/figures/`

### 21a_survey_histogram_functions.R
- **Inputs**: None
- **Outputs**: None

### 21b_survey_histograms.R
- **Inputs**:
  - `proc/survey_successful.csv`
- **Outputs**:
  - `results/figures/survey_percent_sales.eps`
  - `results/figures/survey_fee_diff.eps`
  - `results/numbers/*.tex`

### 22a_admin_graphs_functions.R
- **Inputs**: None
- **Outputs**: None

### 22b_admin_panels_functions.R
- **Inputs**: None
- **Outputs**: None

### 22e_admin_bar_plots.R
- **Inputs**:
  - `proc/regs_for_figs.qs`
  - `proc/fintech_fee_light.csv`
  - `proc/colors.csv`
- **Outputs**:
  - `results/numbers/stat_n_no_control_no_24.tex`
  - `results/figures/cum_takeup_treat_fee_barplot.eps`
  - `results/figures/cum_takeup_treat_fee_barplot.pdf`

### 23_appendix_ML_replication_extension.R
- **Inputs**:
  - `proc/balance_table_data.csv`
  - `proc/fintech_fee_light.csv`
  - multiple `proc/rmse_*.csv`
- **Outputs**:
  - same `proc/rmse_*.csv` (oversight)
  - `results/figures/heatmap_percent_betterrmse.eps`
  - `results/numbers/*.tex`

### 24a_elasticity_tables_functions.R
- **Inputs**: None
- **Outputs**: None

### 24b_elasticity_tables_shortterm.R
- **Inputs**:
  - `proc/07b_panel_regressions.qs`
  - `proc/fintech_fee_dtm_adjusteddates_dataupdate.qs`
  - `proc/fintech_fee_light.csv`
- **Outputs**:
  - `results/tables/monthly_itt_tot.tex`

### 25a_number_calculations_admin.R
- **Inputs**:
  - `proc/fintech_fee_light.csv`
  - `proc/fintech_fee.csv`
  - `proc/users.rds`
  - `proc/balance_table_data.csv`
  - `proc/regs_for_figs.qs`
  - `data/Sent_20201020/Reporte cambio de comision_20201019.xlsx`
- **Outputs**:
  - `results/numbers/*.tex`

### 25b_number_calculations_survey.R
- **Inputs**:
  - `proc/survey_successful.csv`
  - `proc/fintech_fee_light.csv`
  - `results/tables/romano_*.tex`
- **Outputs**:
  - `results/numbers/*.tex`

### 26_Julia/
- **Inputs**: None
- **Outputs**: None

---
For questions or issues, please open an issue or contact the maintainer.

