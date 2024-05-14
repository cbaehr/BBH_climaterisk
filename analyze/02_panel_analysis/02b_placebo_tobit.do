cd "/scratch/network/cb8007/BBH1"

cap set maxvar 32767
cap set matsize 2000

set more off

use "data/lobbying_df_quarterly_REVISE_normal_stata.dta", clear
*sample 10

egen industry_n = group(industry)
egen industry_year_n = group(industry_year)

putexcel set "results.xlsx", replace
putexcel A1=("b_op")
putexcel B1=("se_op")
putexcel C1=("b_rg")
putexcel D1=("se_rg")
putexcel E1=("b_ph")
putexcel F1=("se_ph")
putexcel G1=("issue")

local issues `" "ACC" "ADV" "AER" "AGR" "ALC" "ANI" "APP" "ART" "AUT" "AVI" "BAN" "BEV" "BNK" "BUD" "CDT" "CHM" "CIV" "COM" "CON" "CPI" "CPT" "CSP" "DEF" "DIS" "DOC" "ECN" "EDU" "ENG" "FAM" "FIN" "FIR" "FOO" "FOR" "GAM" "GOV" "HCR" "HOM" "HOU" "IMM" "IND" "INS" "INT" "LAW" "LBR" "MAN" "MAR" "MED" "MIA" "MMM" "MON" "NAT" "PHA" "POS" "RES" "RET" "ROD" "RRR" "SCI" "SMB" "SPO" "TAR" "TAX" "TEC" "TOB" "TOR" "TOU" "TRA" "TRD" "TRU" "UNM" "URB" "UTI" "VET" "WAS" "WEL" "'
local j = 1
foreach i of local issues {
	*display "`i'"
	gen log_CLI_`i'_amount_quarter = log(CLI_`i'_amount_quarter + 1)
	vcemway tobit log_CLI_`i'_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year i.industry_n i.industry_year_n, ll(0) ul(.) cluster(year isin)
	*vcemway tobit log_CLI_`i'_amount_quarter op_expo_ew rg_expo_ew ph_expo_ew ebit ebit_at us_dummy total_lobby_annual i.year, ll(0) ul(.) cluster(year isin)
	
	scalar b_op_`i' = _b[op_expo_ew]
	scalar se_op_`i' = _se[op_expo_ew] 
	scalar b_rg_`i' = _b[rg_expo_ew]
	scalar se_rg_`i' = _se[rg_expo_ew]
	scalar b_ph_`i' = _b[ph_expo_ew]
	scalar se_ph_`i' = _se[ph_expo_ew]
	
	local row = `j'+1
	
	putexcel A`row'=(b_op_`i')
	putexcel B`row'=(se_op_`i')
	putexcel C`row'=(b_rg_`i')
	putexcel D`row'=(se_rg_`i')
	putexcel E`row'=(b_ph_`i')
	putexcel F`row'=(se_ph_`i')
	putexcel G`row'=("`i'")
	local j = `j' + 1
}
