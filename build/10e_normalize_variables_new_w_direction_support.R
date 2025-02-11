### Firms & Lobbying
### Data normalization

rm(list=ls())

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("C:/Users/fiona/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

# load packages
pacman::p_load(data.table, tidyverse, haven)



# Quarterly ---------------------------------------------------------------



#Load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_NEW_altclimatebills_support.rds")
df <- data.frame(df)

glimpse(df)

#Normalize variables for interpretation 

## continuous variables in regression models
df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", 
                       "cc_sent_ew", "op_sent_ew", "rg_sent_ew", "ph_sent_ew",
                       "cc_pos_ew", "op_pos_ew", "rg_pos_ew", "ph_pos_ew",
                       "cc_neg_ew", "op_neg_ew", "rg_neg_ew", "ph_neg_ew",
                       "cc_risk_ew", "op_risk_ew", "rg_risk_ew", "ph_risk_ew")
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

# List of additional variables to standardize
additional_vars <- c("ebit", "ebit_at", "us_dummy", "total_lobby_quarter")

# Standardize and create new columns with '_scaled' suffix
for (var in additional_vars) {
  df[[paste0(var, "_scaled")]] <- as.numeric(scale(df[[var]], center = TRUE, scale = TRUE))
}

# check
table(df$cc_expo_ew, useNA = "ifany") # looks fine
table(df$op_expo_ew, useNA = "ifany") # looks fine
table(df$total_assets_usd, useNA = "ifany") # looks fine
table(df$n_employees, useNA = "ifany") # looks fine
table(df$operating_rev_usd, useNA = "ifany") # looks fine
table(df$P_L_b4tax_usd, useNA = "ifany") # looks fine
table(df$amount_num, useNA = "ifany") # looks fine

# # Drop quarter "2020_2" - first covid quarters
# df <- df %>%
#   filter(!(yearqtr %in% c("2020_2", "2020_3", "2020_4", 
#                          "2021_1", "2021_2", "2021_3", "2021_4")))

# write csv
fwrite(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills_support.csv")

names(df)

df_dta <- df[ , !names(df) %in% c("isin_all", "gov_entity", "issue_code", "issue_text",
                                  "report_uuid", "report_quarter_code", "registrant_id", "registrant_name",
                                  "naics_2022_primary", "naics_2022_secondary", "naics_2017_primary",
                                  "naics_2017_secondary", "bvdaccount", "client_name", "nace_primary",
                                  "nace_secondary", "nace_main_section", "sic_secondary", "sic_primary",
                                  "bvdsector", "primary_naics", "amount_num")]


write_rds(df, "data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills_support.rds")

write_dta(df_dta, path="data/03_final/lobbying_df_quarterly_REVISE_normal_stata_NEW_altclimatebills_support.dta")


### End Quarterly


