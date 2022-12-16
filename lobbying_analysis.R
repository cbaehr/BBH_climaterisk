### Firms & Lobbying
### Analysis

rm(list=ls())

# load packages
library(data.table)
library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(kableExtra)
library(fixest)
library(janitor)

# set working directory
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")
setwd("~/Dropbox (Princeton)/BBH/BBH1/")
# for Vincent
# setwd("~/Dropbox (Privat)/BBH/BBH1/")

# load data
df <- fread("data/lobbying_df_reduced_fb.csv")

## Who lobbies on climate issues? ------------------------------------------

# dummy variable: climate issues
df <- df |>
  mutate(cleanair_water = ifelse(issue_code == "CAW",1,0),
         energy_nuclear = ifelse(issue_code == "ENG",1,0),
         environment = ifelse(issue_code == "ENV",1,0),
         fuel_gas_oil = ifelse(issue_code == "FUE",1,0),
         climate = ifelse(cleanair_water == 1 | 
                            energy_nuclear == 1 |
                            environment == 1 |
                            fuel_gas_oil == 1,
                          1,0))

#summary stats for lobbying dummy variables
climate_table <- df |> 
  tabyl(climate)

cleanair_table <- df |> 
  tabyl(cleanair_water)

energy_table <- df |> 
  tabyl(energy_nuclear)

fuel_table <- df |>
  tabyl(fuel_gas_oil)

env_table <- df |>
  tabyl(environment)


## Control variables -------------------------------------------------------

#US dummy variable 
df <- df |>
  mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

#Rename CO2 emissions variable 
df <- df |>
  rename(co2_emissions = En_En_ER_DP023)

#Average lobbying
df <- df |>    
  group_by(gvkey, year) |>
  mutate(total_lobby = n_distinct(report_uuid))

##Summary statistics for control variables 
datasummary((`Total CO2 Emissions (Tonnes)` = co2_emissions) + (`Earnings Before Interest and Taxes ($M)` = ebit) + (`Total Assets ($M)` = at) + (`Total Lobbying Per Year(#)` = total_lobby) ~ Mean + SD + P25 + P75 + N,
            data = df,
            title = 'Control Variables Summary Statistics',
            align = 'lccccc',
            fmt = 0,
            output = 'latex')

#Lag emissions variable 
df <- df |>
  mutate(log_co2 = log(co2_emissions + 1))

df <- df |>
  # group by unit (in our case: firm)
  group_by(gvkey) |>
  # arrange by year
  arrange(year) |>
  # for one year
  mutate(log_co2_l1 = lag(log_co2, 1)) |>
  #ungroup
  ungroup()


## Logit models ------------------------------------------------------------

## Overall climate lobbying, overall exposure for annual and quarterly
models <- list(
  "Model 1" = feglm(climate ~ cc_expo_ew_y | year, family = "binomial", df),
  "Model 2" = feglm(climate ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "Model 3" = feglm(climate ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 4" = feglm(climate ~ cc_expo_ew_q | year, family = "binomial", df),
  "Model 5" = feglm(climate ~ cc_expo_ew_q + ebit + I(ebit/at) | year, family = "binomial", df),
  "Model 6" = feglm(climate ~ cc_expo_ew_q + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df))

# names
cm <- c("cc_expo_ew_y" = "Overall Attention", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying",
        "cc_expo_ew_q" = "Overall Attention")

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Climate Change Attention on Lobbying on Climate Issues',
  coef_map = cm
  # ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  ,output = "latex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Yearly" = 3,
    "Quarterly" = 3
  ))

## Disaggregated lobby issues, overall climate exposure, annual
models2 <- list(
  "Model 7" = feglm(cleanair_water ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 8" = feglm(energy_nuclear ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 9" = feglm(environment ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 10" = feglm(fuel_gas_oil ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df)
)

# names
cm2 <- c("cc_expo_ew_y" = "Overall Attention", 
         "ebit" = "EBIT",
         "I(ebit/at)" = "EBIT/Assets",
         "log_co2_l1" = "Log(Total CO2 Emissions)",
         "us_dummy" = "US HQ",
         "total_lobby" = "Total Lobbying",
         "cc_expo_ew_q" = "Overall Attention")

modelsummary(
  models2,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Climate Change Attention on Lobbying Across Disaggregated Climate Issues',
  coef_map = cm2
  # ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  ,output = "latex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))

##Aggreate and disaggregated lobby issues, disaggregated exposure types, annual
models3 <- list(
  "Model 11" = feglm(climate ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 12" = feglm(cleanair_water ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 13" = feglm(energy_nuclear ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 14" = feglm(environment ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 15" = feglm(fuel_gas_oil ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df)
  )

cm3 <- c( "op_expo_ew_y" = "Opportunity Attention",
         "rg_expo_ew_y" = "Regulatory Attention",
         "ph_expo_ew_y" = "Physical Attention",
         "ebit" = "EBIT",
         "I(ebit/at)" = "EBIT/Assets",
         "log_co2_l1" = "Log(Total CO2 Emissions)",
         "us_dummy" = "US HQ",
         "total_lobby" = "Total Lobbying")

modelsummary(
  models3,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Different Climate Change Attention on Lobbying on Climate Issues',
  coef_map = cm3
  # ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  ,output = "latex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Climate" = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))

################################################################################

### LOBBYVIEW MODELS WITH NO LAG IN CO2

models <- list(
  "Model 1" = feglm(climate ~ cc_expo_ew_y | year, family = "binomial", df),
  "Model 2" = feglm(climate ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "Model 3" = feglm(climate ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 4" = feglm(climate ~ cc_expo_ew_q | year, family = "binomial", df),
  "Model 5" = feglm(climate ~ cc_expo_ew_q + ebit + I(ebit/at) | year, family = "binomial", df),
  "Model 6" = feglm(climate ~ cc_expo_ew_q + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df))

# names
cm <- c("cc_expo_ew_y" = "Overall Attention", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying",
        "cc_expo_ew_q" = "Overall Attention")

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Climate Change Attention on Lobbying on Climate Issues',
  coef_map = cm,
  gof_omit = 'R2 Adj.|R2 Within',
  output = "latex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Yearly" = 3,
    "Quarterly" = 3
  ))

## Disaggregated lobby issues, overall climate exposure, annual
models2 <- list(
  "Model 7" = feglm(cleanair_water ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 8" = feglm(energy_nuclear ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 9" = feglm(environment ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 10" = feglm(fuel_gas_oil ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df)
)

# names
cm2 <- c("cc_expo_ew_y" = "Overall Attention", 
         "ebit" = "EBIT",
         "I(ebit/at)" = "EBIT/Assets",
         "log_co2" = "Log(Total CO2 Emissions)",
         "us_dummy" = "US HQ",
         "total_lobby" = "Total Lobbying",
         "cc_expo_ew_q" = "Overall Attention")

modelsummary(
  models2,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Climate Change Attention on Lobbying Across Disaggregated Climate Issues',
  coef_map = cm2,
  gof_omit = 'R2 Adj.|R2 Within',
  output = "latex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))

##Aggreate and disaggregated lobby issues, disaggregated exposure types, annual
models3 <- list(
  "Model 11" = feglm(climate ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 12" = feglm(cleanair_water ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 13" = feglm(energy_nuclear ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 14" = feglm(environment ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 15" = feglm(fuel_gas_oil ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2 + us_dummy + total_lobby | year, family = "binomial", df)
)

cm3 <- c( "op_expo_ew_y" = "Opportunity Attention",
          "rg_expo_ew_y" = "Regulatory Attention",
          "ph_expo_ew_y" = "Physical Attention",
          "ebit" = "EBIT",
          "I(ebit/at)" = "EBIT/Assets",
          "log_co2" = "Log(Total CO2 Emissions)",
          "us_dummy" = "US HQ",
          "total_lobby" = "Total Lobbying")

modelsummary(
  models3,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Different Climate Change Attention on Lobbying on Climate Issues',
  coef_map = cm3,
  gof_omit = 'R2 Adj.|R2 Within',
  output = "latex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Climate" = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))



