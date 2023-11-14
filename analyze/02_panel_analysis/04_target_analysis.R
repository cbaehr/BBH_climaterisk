### Firms & Lobbying
### Target Analysis

rm(list=ls())

# install_github("insongkim/concordance", dependencies=TRUE)
# Load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary, readxl)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")


df <- df |>
  mutate(
    EPA_CLI = ifelse(
      CLI == 1 & 
        (EPA_ENV == 1 |
           EPA_CAW == 1 |
           EPA_ENG == 1 |
           EPA_FUE == 1),
      1, 0),
    DOE_CLI = ifelse(
      CLI == 1 & 
        (DOE_ENV == 1 |
           DOE_CAW == 1 |
           DOE_ENG == 1 |
           DOE_FUE == 1),
      1, 0),
  )

# # inspect
# inspect <- df |> select(CLI, EPA_CLI, EPA_ENV, EPA_CAW, EPA_ENG, EPA_FUE)
# # works!



# Run models --------------------------------------------------------------

# names
cm <- c(
  "op_expo_ew_y" = "Opportunity Exposure",
  "rg_expo_ew_y" = "Regulatory Exposure",
  "ph_expo_ew_y" = "Physical Exposure",
  "ebit" = "EBIT",
  "I(ebit/at)" = "EBIT/Assets",
  "log_co2_l1" = "Log(Total CO2 Emissions)",
  "us_dummy" = "US HQ",
  "total_lobby" = "Total Lobbying ($)"
)

models <- list(
  "(EPA)" = feglm(EPA_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(DOE)" = feglm(DOE_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/Tables/climate_logit_targets.tex"
)

### END