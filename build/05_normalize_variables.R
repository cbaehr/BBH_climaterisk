### Firms & Lobbying
### Data normalization

rm(list=ls())

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("C:/Users/fiona/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

# load packages
pacman::p_load(data.table, tidyverse, haven)

#Load data
df <- fread("data/03_final/lobbying_df_quarterly_REVISE.csv")
df <- data.frame(df)

#Normalize variables for interpretation 

## continuous variables in regression models
df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")
## pull from main data
df_wide_cont <- df[, df_wide_cont_vars]
## rescale to standard normal
df_wide_cont <- data.frame(apply(df_wide_cont, 2, as.numeric))
df_wide_cont <- scale(df_wide_cont)
## slot back into main df_wide
df[, df_wide_cont_vars] <- df_wide_cont

to_numeric <- c("CLI_annual", "cc_expo_ew", "at", "n_employees")
for(i in to_numeric) {
  df[ , i] <- as.numeric(df[ , i])
}

# write csv
fwrite(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.csv")

df_dta <- df[ , !names(df) %in% c("isin_all", "gov_entity", "issue_code", "issue_text",
                                  "report_uuid", "report_quarter_code", "registrant_uuid", "registrant_name",
                                  "naics_2022_primary", "naics_2022_secondary", "naics_2017_primary",
                                  "naics_2017_secondary", "bvdaccount", "client_name", "nace_primary",
                                  "nace_secondary", "nace_main_section", "sic_secondary", "sic_primary",
                                  "bvdsector", "primary_naics", "amount_num")]
write_dta(df_dta, path="data/03_final/lobbying_df_quarterly_REVISE_normal_stata.dta")

### End

rm(list = ls())

#Load data
df <- fread("data/03_final/lobbying_df_annual_REVISE.csv")
df <- data.frame(df)
#Normalize variables for interpretation 

## continuous variables in regression models
df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")

## pull from main data
df_wide_cont <- df[, df_wide_cont_vars]
df_wide_cont <- data.frame(apply(df_wide_cont, 2, as.numeric))

## rescale to standard normal

df_wide_cont <- scale(df_wide_cont)
## slot back into main df_wide
df[, df_wide_cont_vars] <- df_wide_cont

to_numeric <- c("CLI_annual", "cc_expo_ew", "at", "n_employees")
for(i in to_numeric) {
  df[ , i] <- as.numeric(df[ , i])
}

# write csv
fwrite(df, file="data/03_final/lobbying_df_annual_REVISE_normal.csv")

df_dta <- df[ , !names(df) %in% c("isin_all", "gov_entity", "issue_code", "issue_text",
                                  "report_uuid", "report_quarter_code", "registrant_uuid", "registrant_name",
                                  "naics_2022_primary", "naics_2022_secondary", "naics_2017_primary",
                                  "naics_2017_secondary", "bvdaccount", "client_name", "nace_primary",
                                  "nace_secondary", "nace_main_section", "sic_secondary", "sic_primary",
                                  "bvdsector", "primary_naics", "amount_num")]
write_dta(df_dta, path="data/03_final/lobbying_df_annual_REVISE_normal_stata.dta")
### End
