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
library(viridis)

# set working directory
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")
setwd("/Users/fiona/Dropbox (Princeton)/BBH/BBH1")
# for Vincent
# setwd("~/Dropbox (Privat)/BBH/BBH1/")

# load data
df <- fread("data/lobbying_df_wide_reduced.csv")

## Who lobbies on climate issues? ------------------------------------------

# dummy variable: climate issues
df <- df |>
  mutate(CLI = ifelse(ENV == 1 | 
                            CAW == 1 |
                            ENG == 1 |
                            FUE == 1 | 
                        ENV == 1,
                          1,0))

#summary stats for lobbying dummy variables
climate_table <- df |> 
  tabyl(CLI)

cleanair_table <- df |> 
  tabyl(CAW)

energy_table <- df |> 
  tabyl(ENG)

fuel_table <- df |>
  tabyl(FUE)

env_table <- df |>
  tabyl(ENV)


## Control variables -------------------------------------------------------

#US dummy variable 
df <- df |>
  mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

#Rename CO2 emissions variable 
df <- df |>
  rename(co2_emissions = En_En_ER_DP023)

#Total annual lobbying (# reports)
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

#Log and lag emissions variable 
df <- df |>
  mutate(log_co2 = log(co2_emissions + 1))

df <- df |>
  # group by unit (in our case: firm)
  group_by(gvkey) |>
  # arrange by year
  arrange(year) |>
  # for one year
  mutate(log_co2_l1 = lag(log_co2, 1),
         total_lobby_l1 = lag(total_lobby, 1)) |>
  #ungroup
  ungroup()


## Logit models (lagged CO2 ------------------------------------------------------------

## Overall climate lobbying, overall exposure for annual and quarterly
models <- list(
  "Model 1" = feglm(CLI ~ cc_expo_ew_y | year, family = "binomial", df),
  "Model 2" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "Model 3" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 4" = feglm(CLI ~ cc_expo_ew_q | year, family = "binomial", df),
  "Model 5" = feglm(CLI ~ cc_expo_ew_q + ebit + I(ebit/at) | year, family = "binomial", df),
  "Model 6" = feglm(CLI ~ cc_expo_ew_q + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df))

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
  ,output = "climate_logit.tex"
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Yearly" = 3,
    "Quarterly" = 3
  ))











## Disaggregated lobby issues, overall climate exposure, annual
models2 <- list(
  "Model 7" = feglm(CAW ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 8" = feglm(ENG ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 9" = feglm(ENV ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 10" = feglm(FUE ~ cc_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df)
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
  "Model 11" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 12" = feglm(CAW ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 13" = feglm(ENG ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 14" = feglm(ENV ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df),
  "Model 15" = feglm(FUE ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", df)
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


# Logit models (no lag for CO2) ---------------------------------------------------

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


# Plots -------------------------------------------------------------------

###Lobbying compared across time for top 10 industries by total attention 
#filter industries 
top10ind_total <- df |> 
  filter(industry %in% c("Automotive Dealers and Gasoline Service Stations", "Coal Mining", "Construction - General Contractors & Operative Builders", "Electric, Gas, and Sanitary Services", "Electronic & Other Electrical Equipment and Components", "Heavy Construction, Except Building Construction and Contractors", "Local & Suburban Transit and Interurban Highway Transportation", "Petroleum Refining and Related Industries", "Primary Metal Industries", "Transportation Equipment"))

#plot total lobbying reports on climate for each industry year 
top10ind_lobby <- top10ind_total |>
  group_by(year, industry) |>
  summarise(total_climate_reports = sum(CLI), na.rm=TRUE)

ggplot(data = top10ind_lobby, aes(x = year, y = total_climate_reports, group = industry)) +
  geom_line(aes(color = industry)) +
  scale_color_viridis(discrete=TRUE) + 
  labs(title = " ", x = "Year", y = "Total Climate Lobbying (# Reports)", color = "Industry") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

ggsave("results/Figures/lobbing_timeseries.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

#plot total spending in reports tagged to climate for each industry year - needs to be updated for correct amount variable
#top10ind_spend <- top10ind_total |>
#  filter(CLI == 1)|>
 # group_by(year, industry) |> 
  #summarise(total_climate_spend = sum(amount))

#ggplot(data = top10ind_spend, aes(x = year, y = total_climate_spend, group = industry)) +
  #geom_line(aes(color = industry)) +
  #scale_color_viridis(discrete=TRUE) + 
  #labs(title = " ", x = "Year", y = "Total Climate Lobbying ($)", color = "Industry") +
  #theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

#ggsave("../results/Figures/lobbingspend_timeseries_industry.pdf", width=unit(8, units="in"), height=unit(6, units="in"))


###Scatterplot of climate attention and total lobbying 

