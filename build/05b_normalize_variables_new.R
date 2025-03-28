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
df <- arrow::read_parquet("data/03_final/lobbying_df_quarterly_REVISE_NEW.parquet")
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
names(df)

arrow::write_parquet(df, "data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")


# Inspect missingness

df <- arrow::read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")

names(df)

df$CLI <- as.numeric(df$CLI_quarter)

df$log_CLI_amount <- log(df$CLI_amount_quarter + 1)

vars <- c("CLI", "log_CLI_amount", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "total_lobby_quarter", "us_dummy")

df |>
  filter(!is.na(op_expo_ew)) |>
  haschaR::check_missings_plot(vars, "yearqtr")

ggsave("results/figures/descriptives/missingness_quarterly_NEW.pdf", width = 10, height = 10)



### End Quarterly



# # Annual ------------------------------------------------------------------


# rm(list = ls())

# #Load data
# df <- read_rds("data/03_final/lobbying_df_annual_REVISE_NEW.rds")
# df <- data.frame(df)
# #Normalize variables for interpretation 

# ## continuous variables in regression models
# df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", 
#                        "cc_sent_ew", "op_sent_ew", "rg_sent_ew", "ph_sent_ew",
#                        "cc_pos_ew", "op_pos_ew", "rg_pos_ew", "ph_pos_ew",
#                        "cc_neg_ew", "op_neg_ew", "rg_neg_ew", "ph_neg_ew")

# ## pull from main data
# df_wide_cont <- df[, df_wide_cont_vars]
# df_wide_cont <- data.frame(apply(df_wide_cont, 2, as.numeric))

# ## rescale to standard normal

# df_wide_cont <- scale(df_wide_cont)
# ## slot back into main df_wide
# df[, df_wide_cont_vars] <- df_wide_cont

# # Check class
# glimpse(df)

# # Transform some to numeric
# df <- df %>%
#   mutate(
#     across(c(ends_with("_ew"), 
#              "total_assets_usd", "n_employees", "operating_rev_usd", "P_L_b4tax_usd", "amount_num"
#     ), as.numeric)
#   )


# # List of additional variables to standardize
# additional_vars <- c("ebit", "ebit_at", "us_dummy", "total_lobby_annual")

# # Standardize and create new columns with '_scaled' suffix
# for (var in additional_vars) {
#   df[[paste0(var, "_scaled")]] <- scale(df[[var]], center = TRUE, scale = TRUE)
# }

# # Check class
# glimpse(df)

# # check
# table(df$op_expo_ew, useNA = "ifany") # looks fine
# table(is.na(df$op_expo_ew))
# class(df$op_expo_ew)
# summary(df$cc_expo_ew)
# table(df$total_assets_usd, useNA = "ifany") # looks fine
# table(df$n_employees, useNA = "ifany") # looks fine
# table(df$operating_rev_usd, useNA = "ifany") # looks fine
# table(df$P_L_b4tax_usd, useNA = "ifany") # looks fine
# table(df$amount_num, useNA = "ifany") # looks fine

# # Drop quarter "2020_2" - first covid quarters
# df <- df %>%
#   filter(!(year %in% c("2020", "2021")))

# table(df$year)


# # write rds
# write_rds(df, file="data/03_final/lobbying_df_annual_REVISE_normal_NEW.rds")

# # write csv
# fwrite(df, file="data/03_final/lobbying_df_annual_REVISE_normal_NEW.csv", row.names = FALSE)

# df_dta <- df[ , !names(df) %in% c("isin_all", "gov_entity", "issue_code", "issue_text",
#                                   "report_uuid", "report_quarter_code", "registrant_uuid", "registrant_name",
#                                   "naics_2022_primary", "naics_2022_secondary", "naics_2017_primary",
#                                   "naics_2017_secondary", "bvdaccount", "client_name", "nace_primary",
#                                   "nace_secondary", "nace_main_section", "sic_secondary", "sic_primary",
#                                   "bvdsector", "primary_naics", "amount_num")]
# write_dta(df_dta, path="data/03_final/lobbying_df_annual_REVISE_normal_stata.dta")


### End