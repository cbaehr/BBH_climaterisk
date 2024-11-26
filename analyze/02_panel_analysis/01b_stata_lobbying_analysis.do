
global tobit_annual 0
global tobit_annual_sent 0
global impute_annual 0
global compare_annual 0
global tobit_quarter 0
global tobit_quarter_sent 1
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
*test op_expo_ew rg_expo_ew ph_expo_ew
test op_expo_ew = rg_expo_ew
local f1a = r(F)
test op_expo_ew = ph_expo_ew
local f1b = r(F)
test rg_expo_ew = ph_expo_ew
local f1c = r(F)
*outreg2 using "$results/tobit_results_annual_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
outreg2 using "$results/tobit_results_annual_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', Wald F-Stat (Op-Rg), `f1a', Wald F-Stat (Op-Ph), `f1b', Wald F-Stat (Rg-Ph), `f1c') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 

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

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_annual i.industry_year_n i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n o.firm_n i.firm_n yeard isind) noomit

*** Interactions

gen opp_reg = op_expo_ew*rg_expo_ew
gen opp_phy = op_expo_ew*ph_expo_ew
gen reg_phy = rg_expo_ew*ph_expo_ew
label var opp_reg "Opp. x Reg."
label var opp_phy "Opp. x Phy."
label var reg_phy "Reg. x Phy."

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_annual, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_annual i.year, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year)

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n)

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at total_lobby_annual i.year i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n o.firm_n i.firm_n yeard isind) noomit

vcemway tobit log_CLI_amount_annual op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at total_lobby_annual i.industry_year_n i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_annual_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n o.firm_n i.firm_n yeard isind) noomit

}

if $tobit_annual_sent {
	
gen opp_sent = op_expo_ew*op_sent_ew
gen reg_sent = rg_expo_ew*rg_sent_ew
gen phy_sent = ph_expo_ew*ph_sent_ew
label var op_sent_ew "Opportunity Sentiment"
label var rg_sent_ew "Regulatory Sentiment"
label var ph_sent_ew "Physical Sentiment"
label var opp_sent "Opp. x Sent."
label var reg_sent "Reg. x Sent."
label var phy_sent "Phy. x Sent."

gen opp_pos = op_expo_ew*op_pos_ew
gen reg_pos = rg_expo_ew*rg_pos_ew
gen phy_pos = ph_expo_ew*ph_pos_ew
label var op_pos_ew "Pos. Opportunity Sent."
label var rg_pos_ew "Pos. Regulatory Sent."
label var ph_pos_ew "Pos. Physical Sent."
label var opp_pos "Opp. x Pos. Sent."
label var reg_pos "Reg. x Pos. Sent."
label var phy_pos "Phy. x Pos. Sent."

gen opp_neg = op_expo_ew*op_neg_ew
gen reg_neg = rg_expo_ew*rg_neg_ew
gen phy_neg = ph_expo_ew*ph_neg_ew
label var op_neg_ew "Neg. Opportunity Sent."
label var rg_neg_ew "Neg. Regulatory Sent."
label var ph_neg_ew "Neg. Physical Sent."
label var opp_neg "Opp. x Neg. Sent."
label var reg_neg "Reg. x Neg. Sent."
label var phy_neg "Phy. x Neg. Sent."

label var op_risk_ew "Opportunity Risk"
label var rg_risk_ew "Regulatory Risk"
label var ph_risk_ew "Physical Risk"

vcemway tobit log_CLI_amount_annual op_pos_ew rg_pos_ew ph_pos_ew op_neg_ew rg_neg_ew ph_neg_ew ebit ebit_at us_dummy total_lobby_annual i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual_sentiment_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

vcemway tobit log_CLI_amount_annual op_sent_ew rg_sent_ew ph_sent_ew ebit ebit_at us_dummy total_lobby_annual i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual_sentiment_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

vcemway tobit log_CLI_amount_annual op_risk_ew rg_risk_ew ph_risk_ew ebit ebit_at us_dummy total_lobby_annual i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_annual_sentiment_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)') noni nocons ctitle("(3)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit


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
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

preserve
egen meanCLI = mean(CLI_quarter), by(isin)
drop if meanCLI==0
*tabulate year, generate(yeard)
*tabulate isin, generate(isind)

*tobit2 log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual yeard* isind*, ll(0) fcluster(isin) tcluster(year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_quarter i.year i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_quarter i.industry_year_n i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

restore


** Interactions ----------------------------------------------------------------

gen opp_reg = op_expo_ew*rg_expo_ew
gen opp_phy = op_expo_ew*ph_expo_ew
gen reg_phy = rg_expo_ew*ph_expo_ew
label var opp_reg "Opp. x Reg."
label var opp_phy "Opp. x Phy."
label var reg_phy "Reg. x Phy."

sort isin year
by isin: gen CLI_amount_quarter_l1 = CLI_amount_quarter[_n-1] if year==year[_n-1]+1

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_quarter i.year, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_quarter i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

preserve
egen meanCLI = mean(CLI_quarter), by(isin)
drop if meanCLI==0
*tabulate year, generate(yeard)
*tabulate isin, generate(isind)

*tobit2 log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual yeard* isind*, ll(0) fcluster(isin) tcluster(year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at total_lobby_quarter i.year i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at total_lobby_quarter i.industry_year_n i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

restore

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at us_dummy total_lobby_quarter CLI_amount_quarter_l1 i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit



preserve
egen meanCLI = mean(CLI_quarter), by(isin)
drop if meanCLI==0
*tabulate year, generate(yeard)
*tabulate isin, generate(isind)

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew opp_reg opp_phy reg_phy ebit ebit_at total_lobby_quarter CLI_amount_quarter_l1 i.industry_year_n i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_interactions_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

restore

** Lagged DV ----------------------------------------------------------------



vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew CLI_amount_quarter_l1, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter CLI_amount_quarter_l1, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) 
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter CLI_amount_quarter_l1 i.year, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(3)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", N) drop(i.year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter CLI_amount_quarter_l1 i.year i.industry_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(4)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", N, "Firm FE", N) drop(i.year i.industry_n)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_quarter CLI_amount_quarter_l1 i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(5)") label addtext("Year FE", Y, "Industry FE", Y, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

preserve
egen meanCLI = mean(CLI_quarter), by(isin)
drop if meanCLI==0
*tabulate year, generate(yeard)
*tabulate isin, generate(isind)

*tobit2 log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual yeard* isind*, ll(0) fcluster(isin) tcluster(year)
vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_quarter CLI_amount_quarter_l1 i.year i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", Y, "Industry FE", N, "Year*Industry FE", N, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

vcemway tobit log_CLI_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at total_lobby_quarter CLI_amount_quarter_l1 i.industry_year_n i.firm_n, ll(0) ul(.) cluster(year isin)
test op_expo_ew rg_expo_ew ph_expo_ew
outreg2 using "$results/tobit_results_quarterly_laggeddv_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)', F-stat, `r(F)', F p-val, `r(p)') noni nocons ctitle("(6)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", Y) drop(i.year i.industry_n i.industry_year_n o.industry_year_n i.isin_n o.isin_n o.firm_n i.firm_n yeard isind) noomit

restore

}


if $tobit_quarter_sent {
	
gen opp_sent = op_expo_ew*op_sent_ew
gen reg_sent = rg_expo_ew*rg_sent_ew
gen phy_sent = ph_expo_ew*ph_sent_ew
label var op_sent_ew "Opportunity Sentiment"
label var rg_sent_ew "Regulatory Sentiment"
label var ph_sent_ew "Physical Sentiment"
label var opp_sent "Opp. x Sent."
label var reg_sent "Reg. x Sent."
label var phy_sent "Phy. x Sent."

gen opp_pos = op_expo_ew*op_pos_ew
gen reg_pos = rg_expo_ew*rg_pos_ew
gen phy_pos = ph_expo_ew*ph_pos_ew
label var op_pos_ew "Pos. Opportunity Sent."
label var rg_pos_ew "Pos. Regulatory Sent."
label var ph_pos_ew "Pos. Physical Sent."
label var opp_pos "Opp. x Pos. Sent."
label var reg_pos "Reg. x Pos. Sent."
label var phy_pos "Phy. x Pos. Sent."

gen opp_neg = op_expo_ew*op_neg_ew
gen reg_neg = rg_expo_ew*rg_neg_ew
gen phy_neg = ph_expo_ew*ph_neg_ew
label var op_neg_ew "Neg. Opportunity Sent."
label var rg_neg_ew "Neg. Regulatory Sent."
label var ph_neg_ew "Neg. Physical Sent."
label var opp_neg "Opp. x Neg. Sent."
label var reg_neg "Reg. x Neg. Sent."
label var phy_neg "Phy. x Neg. Sent."

label var op_risk_ew "Opportunity Risk"
label var rg_risk_ew "Regulatory Risk"
label var ph_risk_ew "Physical Risk"

vcemway tobit log_CLI_amount_quarter op_pos_ew rg_pos_ew ph_pos_ew op_neg_ew rg_neg_ew ph_neg_ew ebit ebit_at us_dummy total_lobby_quarter i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly_sentiment_REVISION.tex", replace eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)') noni nocons ctitle("(1)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

vcemway tobit log_CLI_amount_quarter op_sent_ew rg_sent_ew ph_sent_ew ebit ebit_at us_dummy total_lobby_quarter i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly_sentiment_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)') noni nocons ctitle("(2)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

vcemway tobit log_CLI_amount_quarter op_risk_ew rg_risk_ew ph_risk_ew ebit ebit_at us_dummy total_lobby_quarter i.industry_year_n, ll(0) ul(.) cluster(year isin)
outreg2 using "$results/tobit_results_quarterly_sentiment_REVISION.tex", append eqdrop(sigma) addstat(Adjusted R-Squared, `e(r2_p)') noni nocons ctitle("(3)") label addtext("Year FE", N, "Industry FE", N, "Year*Industry FE", Y, "Firm FE", N) drop(i.year i.industry_n i.industry_year_n o.industry_year_n) noomit

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





