
global tobit_annual 1
global impute_annual 0
global compare_annual 0
global tobit_quarter 1
global impute_quarter 0
global compare_quarter 0

global ROOT "/scratch/network/cb8007/BBH1"

global data "$ROOT/data"
global results "$ROOT/results"
*cd "/scratch/network/cb8007/BBH1"

cap set maxvar 32767
*set matsize 2000

*** ANNUAL ANALYSIS

* one of the variable names is "int" which stata cant process
use "$data/lobbying_df_annual_REVISE_normal_stata", clear
*sample 20

gen log_CLI_amount_annual = log(CLI_amount_annual + 1)
*gen ebit_at = ebit/at
*egen year_industry = group(year industry)
*destring year, replace
egen industry_n = group(industry)
egen industry_year_n = group(industry_year)
egen isin_n = group(isin)

su log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit at us_dummy

label var op_expo_ew "Opportunity Exposure"
label var rg_expo_ew "Regulatory Exposure"
label var ph_expo_ew "Physical Exposure"
label var ebit "EBIT"
label var ebit_at "EBIT/Assets"
label var us_dummy "US HQ"
label var total_lobby_annual "Total Lobbying ($)"

set more off

*****

* tobit results (annual)

if $tobit_annual {

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual.tex", replace eqdrop(sigma) noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N) 
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual.tex", append eqdrop(sigma) noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N) 
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual.tex", append eqdrop(sigma) noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N) drop(i.year)
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual.tex", append eqdrop(sigma) noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N) drop(i.year i.industry_n)
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual.tex", append eqdrop(sigma) noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit
*vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.isin_n, ll(0) ul(.) cluster(year isin)
*outreg2 using "$results/tobit_results_annual.tex", append eqdrop(sigma) noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n) noomit

}

*****

* multiple imputation results (annual)


keep if !missing(cc_expo_ew)
* cannot do imputation with missing covariates. We dont want to impute missing values
* for the exposure variables, because in many cases this might reflect that a firm
* did not EXIST in a particular year-quarter. All firms in the data have some non-missing
* exposure data over the years, but we risk imputing data for a non-existent company
* if we impute the exposure variables.


*egen year_qtr = group(year qtr)

egen ebit_z = std(ebit)
egen at_z = std(at)
egen ebit_at_z = std(ebit_at)

if $impute_annual {

mi set flong

*mi xtset gvkey_n year_qtr
mi xtset isin_n year
*mi register imputed ebit at ebit_at
mi register imputed ebit_z at_z ebit_at_z CLI_annual CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew us_dummy total_lobby_annual

*mi impute mvn ebit at ebit_at = CLI_annual op_expo_ew rg_expo_ew ph_expo_ew us_dummy total_lobby_annual, add(100) prior(ridge, df(0.5))
mi impute mvn ebit_z at_z CLI_annual CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew us_dummy total_lobby_annual, add(100)

mi estimate, post: logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew, vce(cluster isin_n)
est store m1
mi estimate, post: logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_annual, vce(cluster isin_n)
est store m2
mi estimate, post: logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_annual i.year, vce(cluster isin_n)
est store m3
mi estimate, post: logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_annual i.year i.industry_n, vce(cluster isin_n)
est store m4
mi estimate, post: logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_annual i.year i.industry_n i.industry_year_n, vce(cluster isin_n)
est store m5

esttab m1 m2 m3 m4 m5 using "$results/imputation_model_annual.tex", replace tex r2 la nocons noomit drop(*.year *.industry_n *.industry_year_n )

}

***************

* non-imputed logit results (annual) - same specification as imputed for comparison

use "$data/lobbying_df_annual_REVISE_normal_stata", clear

gen log_CLI_amount_annual = log(CLI_amount_annual + 1)
*gen ebit_at = ebit/at
*egen year_industry = group(year industry)
*destring year, replace
egen industry_n = group(industry)
egen industry_year_n = group(industry_year)
egen isin_n = group(isin)

su log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew

label var op_expo_ew "Opportunity Exposure"
label var rg_expo_ew "Regulatory Exposure"
label var ph_expo_ew "Physical Exposure"
label var ebit "EBIT"
label var ebit_at "EBIT/Assets"
label var us_dummy "US HQ"
label var total_lobby_annual "Total Lobbying ($)"

if $compare_annual {

logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew, vce(cluster isin_n)
est store m1_2
logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual, vce(cluster isin_n)
est store m2_2
logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year, vce(cluster isin_n)
est store m3_2
logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n, vce(cluster isin_n)
est store m4_2
logit CLI_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n i.industry_year_n, vce(cluster isin_n)
est store m5_2

esttab m1_2 m2_2 m3_2 m4_2 m5_2 using "$results/imputation_model_BASENOIMPT_annual.tex", replace tex la nocons noomit drop(*.year *.industry_n *.industry_year_n )

}


********************************************************************************

*** QUARTERLY ANALYSIS

* one of the variable names is "int" which stata cant process
use "$data/lobbying_df_quarterly_REVISE_normal_stata", clear
*sample 5

gen log_CLI_amount_quarter = log(CLI_amount_quarter + 1)
*gen ebit_at = ebit/at
*egen year_industry = group(year industry)
*destring year, replace
egen industry_n = group(industry)
egen industry_year_n = group(industry_year)
egen isin_n = group(isin)
destring year, replace

su log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit at ebit_at us_dummy total_lobby_quarter

label var op_expo_ew "Opportunity Exposure"
label var rg_expo_ew "Regulatory Exposure"
label var ph_expo_ew "Physical Exposure"
label var ebit "EBIT"
label var ebit_at "EBIT/Assets"
label var us_dummy "US HQ"
label var total_lobby_quarter "Total Lobbying ($)"

set more off

*****

* tobit results (quarterly)

if $tobit_quarter {

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly.tex", replace eqdrop(sigma) noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N) drop(i.year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N) drop(i.year i.industry_n)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit
*vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.isin_n, ll(0) ul(.) cluster(year isin)
*outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n) noomit

}

*****

* multiple imputation results (quarterly)



keep if !missing(cc_expo_ew)
* cannot do imputation with missing covariates. We dont want to impute missing values
* for the exposure variables, because in many cases this might reflect that a firm
* did not EXIST in a particular year-quarter. All firms in the data have some non-missing
* exposure data over the years, but we risk imputing data for a non-existent company
* if we impute the exposure variables.


*egen year_qtr = group(year qtr)

egen ebit_z = std(ebit)
egen at_z = std(at)
egen ebit_at_z = std(ebit_at)

if $impute_quarter {

mi set flong

*mi xtset gvkey_n year_qtr
mi xtset isin_n year_qtr
*mi register imputed ebit at ebit_at
mi register imputed ebit_z ebit_at_z CLI_quarter CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew us_dummy total_lobby_quarter

*mi impute mvn ebit at ebit_at = CLI_annual op_expo_ew rg_expo_ew ph_expo_ew us_dummy total_lobby_annual, add(100) prior(ridge, df(0.5))
mi impute mvn ebit_z at_z CLI_quarter CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew us_dummy total_lobby_quarter, add(100)

set more off
mi estimate, post: logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew, vce(cluster isin_n)
est store m1
mi estimate, post: logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_quarter, vce(cluster isin_n)
est store m2
mi estimate, post: logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_quarter i.year, vce(cluster isin_n)
est store m3
mi estimate, post: logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_quarter i.year i.industry_n, vce(cluster isin_n)
est store m4
mi estimate, post: logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit_z ebit_at_z us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, vce(cluster isin_n)
est store m5

esttab m1 m2 m3 m4 m5 using "$results/imputation_model_quarterly.tex", replace tex r2 la nocons noomit drop(*.year *.industry_n *.industry_year_n )

}

***************

* non-imputed logit results (quarterly) - same specification as imputed for comparison



use "$data/lobbying_df_quarterly_REVISE_normal_stata", clear

gen log_CLI_amount_quarter = log(CLI_amount_quarter + 1)
*gen ebit_at = ebit/at
*egen year_industry = group(year industry)
*destring year, replace
egen industry_n = group(industry)
egen industry_year_n = group(industry_year)
egen isin_n = group(isin)

su log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew

label var op_expo_ew "Opportunity Exposure"
label var rg_expo_ew "Regulatory Exposure"
label var ph_expo_ew "Physical Exposure"
label var ebit "EBIT"
label var ebit_at "EBIT/Assets"
label var us_dummy "US HQ"
label var total_lobby_quarter "Total Lobbying ($)"

if $compare_quarter {

logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew, vce(cluster isin_n)
est store m1_2
logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter, vce(cluster isin_n)
est store m2_2
logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year, vce(cluster isin_n)
est store m3_2
logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n, vce(cluster isin_n)
est store m4_2
logit CLI_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, vce(cluster isin_n)
est store m5_2

esttab m1_2 m2_2 m3_2 m4_2 m5_2 using "$results/imputation_model_BASENOIMPT_quarterly.tex", replace tex la nocons noomit drop(*.year *.industry_n *.industry_year_n )

}










