### Firms & Lobbying
### Mobility Analysis

rm(list=ls())

# Load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")



# Transform Variables -----------------------------------------------------

# dummy variable: climate issues
df <- df |>
  mutate(CLI = ifelse(ENV == 1 | 
                        CAW == 1 |
                        ENG == 1 |
                        FUE == 1,
                      1,0))

df$CLI_dollars <- apply(df[, c("amount_num_ENV", "amount_num_CAW", "amount_num_ENG", "amount_num_FUE")],
                        1, function(x) sum(x, na.rm=T) / 1000000)



# Control variables -------------------------------------------------------

#US dummy variable 
df <- df |>
  mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

#Total annual lobbying (total dollars)
df <- df |>
  group_by(gvkey, year) |>
  # mutate(total_lobby = n_distinct(report_uuid))
  mutate(total_lobby = sum(c_across(grep("amount_num", names(df), value=T))))

df <- df |>
  # group by unit (in our case: firm)
  group_by(gvkey) |>
  # arrange by year
  arrange(year) |>
  # for one year
  mutate(#log_co2_l1 = lag(log_co2, 1),
    total_lobby_l1 = lag(total_lobby, 1)) |>
  #ungroup
  ungroup()


df$industry <- df$bvd_sector
df <- df[which(df$industry!=""), ]
df$industry_year <- paste(df$industry, df$year)

sum(duplicated(df[, c("year", "report_quarter_code", "gvkey")]))

## continuous variables in regression models
df_cont_vars <- c("cc_expo_ew_y", "cc_expo_ew_q", "op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y",
                  "ebit", "at", "total_lobby")
## pull from main data
df_cont <- df[, df_cont_vars]
## rescale to standard normal
df_cont <- scale(df_cont)
## slot back into main df
df[, df_cont_vars] <- df_cont
rm(df_cont, df_cont_vars)

# Transform variabls for regression
df <- df |>
  mutate(
    ebit_asset = ebit/at,
    CLI_dollars = log(CLI_dollars +1)
  )


df$sic2 <- as.numeric(substr(df$sic, 1, 2))
# merge


# Focus on electric utilities ---------------------------------------------

# Reduce to Industry Group 491: Electric Services
elec <- df |> filter(sic == "491")


# Run models

# names
cm <- c(
  "op_expo_ew_y" = "Opportunity Exposure",
  "rg_expo_ew_y" = "Regulatory Exposure",
  "ph_expo_ew_y" = "Physical Exposure",
  "cc_expo_ew_y" = "Overall Exposure", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying ($)",
        "cc_expo_ew_q" = "Overall Exposure"
        )

## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "(Occurrence)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", elec),
  "(Expenditure)" = feols(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, elec)
)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/Tables/mobility/electric.tex"
)



# HQ interaction ----------------------------------------------------------

# Run models

# names
cm <- c(
  "op_expo_ew_y:us_dummy" = "Opportunity x US Headquarter",
  "us_dummy:rg_expo_ew_y" = "Regulatory x US Headquarter",
  "us_dummy:ph_expo_ew_y" = "Physical x US Headquarter",
  "op_expo_ew_y" = "Opportunity Exposure",
  "rg_expo_ew_y" = "Regulatory Exposure",
  "ph_expo_ew_y" = "Physical Exposure",
  "cc_expo_ew_y" = "Overall Exposure", 
  "ebit" = "EBIT",
  "I(ebit/at)" = "EBIT/Assets",
  "log_co2_l1" = "Log(Total CO2 Emissions)",
  "us_dummy" = "US HQ",
  "total_lobby" = "Total Lobbying ($)",
  "cc_expo_ew_q" = "Overall Exposure"
)

## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "(Occurrence)" = feglm(CLI ~ op_expo_ew_y*us_dummy+ rg_expo_ew_y*us_dummy + ph_expo_ew_y*us_dummy + ebit + I(ebit/at) + us_dummy + total_lobby | year +  industry + industry_year, family = "binomial", df),
  "(Expenditure)" = feols(CLI ~ op_expo_ew_y*us_dummy + rg_expo_ew_y*us_dummy + ph_expo_ew_y*us_dummy + ebit + I(ebit/at) + us_dummy + total_lobby | year +  industry + industry_year, df)
)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/Tables/mobility/headquarter_interaction.tex"
)




# EC classification -------------------------------------------------------


