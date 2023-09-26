### Firms & Lobbying
### Mobility Analysis

rm(list=ls())

# install_github("insongkim/concordance", dependencies=TRUE)
# Load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary, concordance)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")

# # Focus on electric utilities ---------------------------------------------
# 
# # Reduce to Industry Group 491: Electric Services
# elec <- df |> filter(sic == "491")
# 
# 
# # Run models
# 
# # names
# cm <- c(
#   "op_expo_ew_y" = "Opportunity Exposure",
#   "rg_expo_ew_y" = "Regulatory Exposure",
#   "ph_expo_ew_y" = "Physical Exposure",
#   "cc_expo_ew_y" = "Overall Exposure", 
#         "ebit" = "EBIT",
#         "I(ebit/at)" = "EBIT/Assets",
#         "log_co2_l1" = "Log(Total CO2 Emissions)",
#         "us_dummy" = "US HQ",
#         "total_lobby" = "Total Lobbying ($)",
#         "cc_expo_ew_q" = "Overall Exposure"
#         )
# 
# ## Overall climate lobbying, overall exposure for annual by specific attention component
# models <- list(
#   "(Occurrence)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", elec),
#   "(Expenditure)" = feols(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, elec)
# )
# 
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/Tables/mobility/electric.tex"
# )
# 
# 
# 
# # HQ interaction ----------------------------------------------------------
# 
# # Run models
# 
# # names
# cm <- c(
#   "op_expo_ew_y:us_dummy" = "Opportunity x US Headquarter",
#   "us_dummy:rg_expo_ew_y" = "Regulatory x US Headquarter",
#   "us_dummy:ph_expo_ew_y" = "Physical x US Headquarter",
#   "op_expo_ew_y" = "Opportunity Exposure",
#   "rg_expo_ew_y" = "Regulatory Exposure",
#   "ph_expo_ew_y" = "Physical Exposure",
#   "cc_expo_ew_y" = "Overall Exposure", 
#   "ebit" = "EBIT",
#   "I(ebit/at)" = "EBIT/Assets",
#   "log_co2_l1" = "Log(Total CO2 Emissions)",
#   "us_dummy" = "US HQ",
#   "total_lobby" = "Total Lobbying ($)",
#   "cc_expo_ew_q" = "Overall Exposure"
# )
# 
# ## Overall climate lobbying, overall exposure for annual by specific attention component
# models <- list(
#   "(Occurrence)" = feglm(CLI ~ op_expo_ew_y*us_dummy+ rg_expo_ew_y*us_dummy + ph_expo_ew_y*us_dummy + ebit + I(ebit/at) + us_dummy + total_lobby | year +  industry + industry_year, family = "binomial", df),
#   "(Expenditure)" = feols(CLI ~ op_expo_ew_y*us_dummy + rg_expo_ew_y*us_dummy + ph_expo_ew_y*us_dummy + ebit + I(ebit/at) + us_dummy + total_lobby | year +  industry + industry_year, df)
# )
# 
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/Tables/mobility/headquarter_interaction.tex"
# )




# EC classification -------------------------------------------------------

# Load ec leakage list
ec <- read.xlsx("data/01_raw/mobility/EC leakage/sectors_risk_carbon_leakage2019.xlsx")
# Load conversion table
conv <- read.xlsx("data/01_raw/mobility/EC leakage/copy-of-fe-listing-sic-2007-matched-to-nace-rev2.xlsx") |>
  mutate(nace = as.numeric(str_extract(NACE.code.and.description, "^\\d{4}"))) |>
  select(-c(NACE.code.and.description, Description)) |>
  rename(sic = SIC.Code)

# Merge with ec
ec <- ec |> left_join(conv, by="nace")

df <- df |>
  mutate(naics = concord(origin = ))





### END