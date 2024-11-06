
global tobit_annual 1
global impute_annual 0
global compare_annual 0
global tobit_quarter 1
global impute_quarter 0
global compare_quarter 0
global tobit_quarter_target 0
global tobit_quarter_issue 0
global tobit_quarter_fe 0

global ROOT "/scratch/network/cb8007/BBH1"

global data "$ROOT/data"
global results "$ROOT/results"

cap set maxvar 32767
cap set matsize 11000

*** ANNUAL ANALYSIS

* one of the variable names is "int" which stata cant process
use "$data/lobbying_df_annual_REVISE_normal_stata", clear
*sample 20

gen log_CLI_amount_annual = log(CLI_amount_annual + 1)
egen industry_n = group(industry)
egen industry_year_n = group(industry_year)
egen firm_n = group(isin)

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
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew

outreg2 using "$results/tobit_results_annual_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew

outreg2 using "$results/tobit_results_annual_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year)
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n)
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

preserve
egen meanCLI = mean(CLI_annual), by(isin)
drop if meanCLI==0
*tabulate year, generate(yeard)
*tabulate isin, generate(isind)

*tobit2 log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual yeard* isind*, ll(0) fcluster(isin) tcluster(year)
vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_annual i.year i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n o.firm_n i.firm_n yeard isind) noomit

restore

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
egen firm_n = group(isin)
destring year, replace

*keep if conm=="TOYOTA MOTOR CORPORATION" & year==2019 & qtr==4

su log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit at ebit_at us_dummy total_lobby_quarter

label var op_expo_ew "Opportunity Exposure"
label var rg_expo_ew "Regulatory Exposure"
label var ph_expo_ew "Physical Exposure"
label var ebit "EBIT"
label var ebit_at "EBIT/Assets"
label var us_dummy "US HQ"
label var total_lobby_quarter "Total Lobbying ($)"

set more off

*keep if conm=="GENERAL MOTORS COMPANY" | conm=="FORD MOTOR CO" | conm=="TOYOTA MOTOR CORPORATION"
*export delimited "/scratch/network/cb8007/tobit_prediction_data_REVISION.csv", replace

*****

* tobit results (quarterly)

if $tobit_quarter {


set more off
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew, ll(0) ul(.) cluster(year isin)
local sig1 = _b[/sigma]
di `sig1'
*di _b[/sigma]
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
est sto hi1
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter, ll(0) ul(.) cluster(year isin)
local sig2 = _b[/sigma]
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
est sto hi2
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year, ll(0) ul(.) cluster(year isin)
local sig3 = _b[/sigma]
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year)
est sto hi3
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n, ll(0) ul(.) cluster(year isin)
local sig4 = _b[/sigma]
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n)
est sto hi4
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
local sig5 = _b[/sigma]
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit
est sto hi5
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
local sig5 = _b[/sigma]
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit
est sto hi5

preserve
egen meanCLI = mean(CLI_quarter), by(isin)
drop if meanCLI==0
*tabulate year, generate(yeard)
*tabulate isin, generate(isind)

*tobit2 log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual yeard* isind*, ll(0) fcluster(isin) tcluster(year)
*vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_quarter i.year i.firm_n, ll(0) ul(.) cluster(year isin)
*test op_expo_ew rg_expo_ew ph_expo_ew
*outreg2 using "$results/tobit_results_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit
restore

*predictnl phat = normal(xb())

*putexcel set "$results/tobit_model_sigmas.csv", sheet("Sheet1") replace
*putexcel A1 = (`sig1')
*putexcel A2 = (`sig2')
*putexcel A3 = (`sig3')
*putexcel A4 = (`sig4')
*putexcel A5 = (`sig5')
*putexcel close


estadd scalar sig1 = `sig1'
estadd scalar sig2 = `sig2'
estadd scalar sig3 = `sig3'
estadd scalar sig4 = `sig4'
estadd scalar sig5 = `sig5'

esttab hi1 hi2 hi3 hi4 hi5 using "$results/tobit_model_coefs.csv", replace la nocons noomit stats(sig1 sig2 sig3 sig4 sig5)

preserve
keep if year==2019
keep if qtr==4
keep if industry=="Transport Manufacturing"
export delimited "$results/tobit_model_prediction_data.csv", replace
restore

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
gen log_CLI_EPA_amount_quarter = log(CLI_EPA_amount_quarter + 1)
gen log_CLI_DOE_amount_quarter = log(CLI_DOE_amount_quarter + 1)
*gen ebit_at = ebit/at
*egen year_industry = group(year industry)
*destring year, replace
egen industry_n = group(industry)
egen industry_year_n = group(industry_year)
egen isin_n = group(isin)

egen yearqtr_n = group(yearqtr)
egen industry_yearqtr_n = group(yearqtr_n industry_n)

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



if $tobit_quarter_target {

vcemway tobit log_CLI_EPA_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_targets_quarterly.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("EPA") drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

vcemway tobit log_CLI_DOE_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_targets_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("DOE") drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

}



if $tobit_quarter_issue {
gen log_CLI_FUE_amount_quarter = log(CLI_FUE_amount_quarter + 1)
gen log_CLI_ENV_amount_quarter = log(CLI_ENV_amount_quarter + 1)
gen log_CLI_CAW_amount_quarter = log(CLI_CAW_amount_quarter + 1)
gen log_CLI_ENG_amount_quarter = log(CLI_ENG_amount_quarter + 1)

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_issue_quarterly.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("CLI") drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

vcemway tobit log_CLI_CAW_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_issue_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("CAW") drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

vcemway tobit log_CLI_ENG_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_issue_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("ENG") label drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

vcemway tobit log_CLI_ENV_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_issue_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("ENV") label drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

vcemway tobit log_CLI_FUE_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/climate_tobit_issue_quarterly.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("FUE") label drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n) addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y) 

}

if $tobit_quarter_fe {

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_qtrFE.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_qtrFE.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.yearqtr_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_qtrFE.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.yearqtr_n o.yearqtr_n)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.yearqtr_n i.industry_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_qtrFE.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n i.yearqtr_n o.yearqtr_n i.industry_yearqtr_n o.industry_yearqtr_n)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.yearqtr_n i.industry_n i.industry_yearqtr_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_qtrFE.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.industry_yearqtr_n o.industry_yearqtr_n) noomit

preserve
egen meanCLI = mean(CLI_quarter), by(isin)
drop if meanCLI==0

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_quarter i.yearqtr_n i.isin_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_qtrFE.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.yearqtr_n o.yearqtr_n i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit
restore

}





