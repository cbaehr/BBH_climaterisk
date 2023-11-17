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

glimpse(df)

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

# Transform some to numeric
df <- df %>%
  mutate(
    across(c(ends_with("_ew"), 
             "total_assets_usd", "n_employees", "operating_rev_usd", "P_L_b4tax_usd", "amount_num"
             ), as.numeric)
    )

# check
table(df$cc_expo_ew, useNA = "ifany") # looks fine
table(df$op_expo_ew, useNA = "ifany") # looks fine
table(df$total_assets_usd, useNA = "ifany") # looks fine
table(df$n_employees, useNA = "ifany") # looks fine
table(df$operating_rev_usd, useNA = "ifany") # looks fine
table(df$P_L_b4tax_usd, useNA = "ifany") # looks fine
table(df$amount_num, useNA = "ifany") # looks fine

# write csv
fwrite(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.csv")

df_dta <- df[ , !names(df) %in% c("isin_all", "gov_entity", "issue_code", "issue_text",
                                  "report_uuid", "report_quarter_code", "registrant_uuid", "registrant_name",
                                  "naics_2022_primary", "naics_2022_secondary", "naics_2017_primary",
                                  "naics_2017_secondary", "bvdaccount", "client_name", "nace_primary",
                                  "nace_secondary", "nace_main_section", "sic_secondary", "sic_primary",
                                  "bvdsector", "primary_naics", "amount_num")]
write_dta(df_dta, path="data/03_final/lobbying_df_quarterly_REVISE_normal_stata.dta")



### End Quarterly

### Start Annual

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

# Check class
glimpse(df)

# Transform some to numeric
df <- df %>%
  mutate(
    across(c(ends_with("_ew"), 
             "total_assets_usd", "n_employees", "operating_rev_usd", "P_L_b4tax_usd", "amount_num"
    ), as.numeric)
  )

# Check class
glimpse(df)


# Transform some empty to NA
df <- df %>%
  mutate(across(where(is.numeric), ~ ifelse(. == "", NA, .)))

# Check class
glimpse(df)

# check
table(df$cc_expo_ew, useNA = "ifany") # looks fine
table(is.na(df$cc_expo_ew))
class(df$cc_expo_ew)
summary(df$cc_expo_ew)
table(df$op_expo_ew, useNA = "ifany") # looks fine
table(df$total_assets_usd, useNA = "ifany") # looks fine
table(df$n_employees, useNA = "ifany") # looks fine
table(df$operating_rev_usd, useNA = "ifany") # looks fine
table(df$P_L_b4tax_usd, useNA = "ifany") # looks fine
table(df$amount_num, useNA = "ifany") # looks fine

glimpse(df)

# write rds
write_rds(df, file="data/03_final/lobbying_df_annual_REVISE_normal.rds")

# write csv
fwrite(df, file="data/03_final/lobbying_df_annual_REVISE_normal.csv", row.names = FALSE)

df_dta <- df[ , !names(df) %in% c("isin_all", "gov_entity", "issue_code", "issue_text",
                                  "report_uuid", "report_quarter_code", "registrant_uuid", "registrant_name",
                                  "naics_2022_primary", "naics_2022_secondary", "naics_2017_primary",
                                  "naics_2017_secondary", "bvdaccount", "client_name", "nace_primary",
                                  "nace_secondary", "nace_main_section", "sic_secondary", "sic_primary",
                                  "bvdsector", "primary_naics", "amount_num")]
write_dta(df_dta, path="data/03_final/lobbying_df_annual_REVISE_normal_stata.dta")
### End
