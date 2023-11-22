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
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

# Rename fixed effects variables
df <- df |>
  rename(
    Firm = gvkey,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

# Run models --------------------------------------------------------------

# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure", 
        "cc_expo_ew" = "Overall Exposure",
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_quarter" = "Total Lobbying ($)"
)

models <- list(
  "EPA" = feglm(CLI_EPA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry + `Industry x Year`, family = "binomial", df),
  "DOE" = feglm(CLI_DOE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry + `Industry x Year`, family = "binomial", df)
)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  ,coef_map = cm
  ,vcov = ~ Year + Industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  #,output = "results/Tables/climate_logit_qrt_targets.tex"
)

### END