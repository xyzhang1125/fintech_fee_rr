********************************************************************************
* Author: Mohammad Atif Haidry
* File name: 15b_survey_measures_tables.do 
* Files used:
*    proc/survey_successful_heterogeneity.dta
* Files created:
*    results/tables/romano_p_antrem_unantrem.tex
*    results/tables/romano_p_unantrem_norem.tex
* Purpose:
*    Run Romano–Wolf corrections on survey‐measure × treatment interactions
*    and export adjusted p-values to LaTeX tables.
********************************************************************************

clear

// 1. Fixing path for exact stata packages
tokenize `"$S_ADO"', parse(";")
while `"`1'"' != "" {
    if `"`1'"'!="BASE" cap adopath - `"`1'"'
    macro shift
}

adopath ++ "`c(pwd)'/scripts/programs/"

use "proc/survey_successful_heterogeneity.dta", clear

generate accepted = accepted_offer_late
generate trust = trust_scale_1_binary
generate anticipated = anticipated_reminder
generate unanticipated = unanticipated_reminder
generate trust_anticipated = trust*anticipated
generate trust_unanticipated = trust*unanticipated

generate reciprocity = reciprocity_scale_binary
generate recip_anticipated = reciprocity*anticipated
generate recip_unanticipated = reciprocity*unanticipated

generate procrastination = procrastination_scale_binary
generate procr_anticipated = procrastination*anticipated
generate procr_unanticipated = procrastination*unanticipated


generate memory = memory_scale_1_binary
generate memor_anticipated = memory*anticipated
generate memor_unanticipated = memory*unanticipated

generate overconfidence = memory_scale_2_binary
generate overc_anticipated = overconfidence*anticipated
generate overc_unanticipated = overconfidence*unanticipated

generate attention = attention_scale_binary
generate atten_anticipated = attention*anticipated
generate atten_unanticipated = attention*unanticipated



rwolf2 (reghdfe accepted trust anticipated trust_anticipated if anticipated == 1 | unanticipated == 1, noabsorb vce(robust)) (reghdfe accepted reciprocity anticipated recip_anticipated if anticipated == 1 | unanticipated == 1, noabsorb vce(robust)) (reghdfe accepted procrastination anticipated procr_anticipated if anticipated == 1 | unanticipated == 1, noabsorb vce(robust)) (reghdfe accepted memory anticipated memor_anticipated if anticipated == 1 | unanticipated == 1, noabsorb vce(robust)) (reghdfe accepted overconfidence anticipated overc_anticipated if anticipated == 1 | unanticipated == 1, noabsorb vce(robust)) (reghdfe accepted attention anticipated atten_anticipated if anticipated == 1 | unanticipated == 1, noabsorb vce(robust)), indepvars(trust_anticipated, recip_anticipated, procr_anticipated, memor_anticipated, overc_anticipated, atten_anticipated) reps(1000) seed(8888)

matrix RW_pvalues = e(RW)
matrix list RW_pvalues
matrix colnames RW_pvalues = "Model p-value" "Resample p-value" "Romano-Wolf p-value"
matrix rownames RW_pvalues = "Trust x Announced" "Reciprocity x Announced" "Procrastination x Announced" "Memory x Announced" "Overconfidence x Announced" "Attention x Announced"

matrix list RW_pvalues
esttab matrix(RW_pvalues) using "results/tables/romano_p_antrem_unantrem.tex"


rwolf2 (reghdfe accepted trust unanticipated trust_unanticipated if unanticipated == 1 | no_reminder == 1, noabsorb vce(robust)) (reghdfe accepted reciprocity unanticipated recip_unanticipated if unanticipated == 1 | no_reminder == 1, noabsorb vce(robust)) (reghdfe accepted procrastination unanticipated procr_unanticipated if unanticipated == 1 | no_reminder == 1, noabsorb vce(robust)) (reghdfe accepted memory unanticipated memor_unanticipated if unanticipated == 1 | no_reminder == 1, noabsorb vce(robust)) (reghdfe accepted overconfidence unanticipated overc_unanticipated if unanticipated == 1 | no_reminder == 1, noabsorb vce(robust)) (reghdfe accepted attention unanticipated atten_unanticipated if unanticipated == 1 | no_reminder == 1, noabsorb vce(robust)), indepvars(trust_unanticipated, recip_unanticipated, procr_unanticipated, memor_unanticipated, overc_unanticipated, atten_unanticipated) usevalid reps(1000) seed(8888)

matrix RW_pvalues = e(RW)
matrix list RW_pvalues
matrix colnames RW_pvalues = "Model p-value" "Resample p-value" "Romano-Wolf p-value"
matrix rownames RW_pvalues = "Trust x Unannounced" "Reciprocity x Unannounced" "Procrastination x Unannounced" "Memory x Unannounced" "Overconfidence x Unannounced" "Attention x Unannounced"

matrix list RW_pvalues
esttab matrix(RW_pvalues) using "results/tables/romano_p_unantrem_norem.tex"


