### Firms & Lobbying
### Analysis

### Quarterly

rm(list=ls())

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, corrplot, janitor)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure",
        "op_sent_ew" = "Opportunity Sentiment",
        "rg_sent_ew" = "Regulatory Sentiment",
        "ph_sent_ew" = "Physical Sentiment",
        "op_pos_ew" = "Opportunity Pos. Sent.",
        "rg_pos_ew" = "Regulatory Pos. Sent.",
        "ph_pos_ew" = "Physical Pos. Sent.",
        "op_neg_ew" = "Opportunity Neg. Sent.",
        "rg_neg_ew" = "Regulatory Neg. Sent.",
        "ph_neg_ew" = "Physical Neg. Sent.",
        "op_risk_ew" = "Opportunity Risk",
        "rg_risk_ew" = "Regulatory Risk",
        "ph_risk_ew" = "Physical Risk",
        "cc_expo_ew" = "Overall Exposure",
        "op_expo_ew:rg_expo_ew" = "Opp. x Reg.",
        "op_expo_ew:ph_expo_ew" = "Opp. x Phy.",
        "rg_expo_ew:ph_expo_ew" = "Reg. x Phy.",
        "op_expo_ew:op_sent_ew" = "Opp. x Sent.",
        "rg_expo_ew:rg_sent_ew" = "Reg. x Sent.",
        "ph_expo_ew:ph_sent_ew" = "Phy. x Sent.",
        "op_expo_ew:cc_sent_ew" = "Opp. x General Sent.",
        "cc_sent_ew:rg_expo_ew" = "Reg. x General Sent.",
        "cc_sent_ew:ph_expo_ew" = "Phy. x General Sent.",
        "op_expo_ew:op_pos_ew" = "Opp. x Pos.",
        "rg_expo_ew:rg_pos_ew" = "Reg. x Pos.",
        "ph_expo_ew:ph_pos_ew" = "Phy. x Pos.",
        "op_expo_ew:op_neg_ew" = "Opp. x Neg.",
        "rg_expo_ew:rg_neg_ew" = "Reg. x Neg.",
        "ph_expo_ew:ph_neg_ew" = "Phy. x Neg.",
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_quarter" = "Total Lobbying (\\$)",
        "CLI_l1" = "Lagged DV",
        "log_CLI_amount_l1" = "Lagged DV",
        "n_employees" = "Employees",
        "cso_exists" = "CSO Inplace",
        "cdp_report" = "CDP Reporter"
)

process_df <- function(data) {
  # Rename fixed effects variables
  df <- data |>
    mutate(
      Firm = isin,
      Year = year,
      Industry = industry,
      `Industry x Year` = industry_year
    )
  
  df <- df[!is.na(df$industry) , ]
  
  df$CLI <- as.numeric(df$CLI_quarter)
  
  df$log_CLI_amount <- log(df$CLI_amount_quarter + 1)
  
  df <- df %>%
    group_by(Firm) %>%
    mutate(CLI_l1 = lag(CLI, n=1, order_by=yearqtr),
           log_CLI_amount_l1 = lag(log_CLI_amount, n=1, order_by=yearqtr),
           op_expo_ew_l1 = lag(op_expo_ew, n=1, order_by=yearqtr),
           rg_expo_ew_l1 = lag(rg_expo_ew, n=1, order_by=yearqtr),
           ph_expo_ew_l1 = lag(ph_expo_ew, n=1, order_by=yearqtr))
  
  df$CLI_chg <- df$CLI - df$CLI_l1
  df$log_CLI_amount_chg <- df$log_CLI_amount - df$log_CLI_amount_l1
  df$op_expo_ew_chg <- df$op_expo_ew - df$op_expo_ew_l1
  df$rg_expo_ew_chg <- df$rg_expo_ew - df$rg_expo_ew_l1
  df$ph_expo_ew_chg <- df$ph_expo_ew - df$ph_expo_ew_l1
  return(df)
}






## OLS -----------------------------------------------------

df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")
df <- process_df(df)

glimpse(df)

# inspect
df %>%
  filter(isin == "BMG0450A1053" & year == 2002 & qtr == 1) %>%
  select(isin, year, qtr, CLI_TAX_amount_quarter)

df %>%
  filter(isin == "CA0089161081" & year == 2015 & qtr == 2) %>%
  select(isin, year, qtr, CLI_ENV_amount_quarter)

summary(df$CLI_TAX_amount_quarter)

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm),
  "(8)" = feols(CLI ~ op_pos_ew + rg_pos_ew + ph_pos_ew + op_neg_ew + rg_neg_ew + ph_neg_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(9)" = feols(CLI ~ op_sent_ew + rg_sent_ew + ph_sent_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(10)" = feols(CLI ~ op_risk_ew + rg_risk_ew + ph_risk_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm),
  "(8)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(9)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_interaction_MODELS_REVISION_NEW.RData")

## OLS - Lagged DV -----------------------------------------------------
models <- list(
  "(1)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + CLI_l1, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_laggeddv_MODELS_REVISION_NEW.RData")

## OLS - Amount -----------------------------------------------------
models <- list(
  "(1)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm),
  "(8)" = feols(log_CLI_amount ~ op_pos_ew + rg_pos_ew + ph_pos_ew + op_neg_ew + rg_neg_ew + ph_neg_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(9)" = feols(log_CLI_amount ~ op_sent_ew + rg_sent_ew + ph_sent_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(10)" = feols(log_CLI_amount ~ op_risk_ew + rg_risk_ew + ph_risk_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")

## OLS - Interactions - Amount -----------------------------------------------------
models <- list(
  "(1)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm),
  "(8)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(9)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_interaction_amount_MODELS_REVISION_NEW.RData")

## OLS - Lagged DV - Amount -----------------------------------------------------
models <- list(
  "(1)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + log_CLI_amount_l1, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + log_CLI_amount_l1 | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_laggeddv_amount_MODELS_REVISION_NEW.RData")

## OLS - Error Correction -----------------------------------------------------
models <- list(
  "(1)" = feols(CLI_chg ~ op_expo_ew_l1 + op_expo_ew_chg + rg_expo_ew_l1 + rg_expo_ew_chg + ph_expo_ew_l1 + ph_expo_ew_chg + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount_chg ~ op_expo_ew_l1 + op_expo_ew_chg + rg_expo_ew_l1 + rg_expo_ew_chg + ph_expo_ew_l1 + ph_expo_ew_chg + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI_chg ~ op_expo_ew_l1 + op_expo_ew_chg + rg_expo_ew_l1 + rg_expo_ew_chg + ph_expo_ew_l1 + ph_expo_ew_chg + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount_chg ~ op_expo_ew_l1 + op_expo_ew_chg + rg_expo_ew_l1 + rg_expo_ew_chg + ph_expo_ew_l1 + ph_expo_ew_chg + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year` + Firm, df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_ols_qrt_errorcorrect_MODELS_REVISION_NEW.RData")

## OLS Occurrence - Targets --------------------------------------------------------
df$CLI_CONG_quarter <- (df$CLI_HOUS_quarter + df$CLI_SEN_quarter > 0) * 1
models <- list(
  "EPA" = feols(CLI_EPA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOE" = feols(CLI_DOE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "FEMA" = feols(CLI_FEMA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "COEQ" = feols(CLI_COEQ_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOT" = feols(CLI_DOT_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOTY" = feols(CLI_DOTY_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOA" = feols(CLI_DOA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "NOAA" = feols(CLI_NOAA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "HOUS" = feols(CLI_HOUS_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "SEN" = feols(CLI_SEN_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "CONG" = feols(CLI_CONG_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "WTHS" = feols(CLI_WTHS_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_target_MODELS_REVISION_NEW.RData")

## OLS Amount - Targets --------------------------------------------------------
df$CLI_CONG_amount_quarter <- df$CLI_HOUS_amount_quarter + df$CLI_SEN_amount_quarter
models <- list(
  "EPA" = feols(log(CLI_EPA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOE" = feols(log(CLI_DOE_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "FEMA" = feols(log(CLI_FEMA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "COEQ" = feols(log(CLI_COEQ_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOT" = feols(log(CLI_DOT_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOTY" = feols(log(CLI_DOTY_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOA" = feols(log(CLI_DOA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "NOAA" = feols(log(CLI_NOAA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "HOUS" = feols(log(CLI_HOUS_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "SEN" = feols(log(CLI_SEN_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "CONG" = feols(log(CLI_CONG_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "WTHS" = feols(log(CLI_WTHS_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_target_amount_MODELS_REVISION_NEW.RData")

## OLS - Augmented Controls Model --------------------------------------------

df$subs_iso3c <- unlist(lapply(df$subs_iso3c, FUN = function(x) paste(gsub("n.a.", "", unique(unlist(strsplit(x, "\n")))), collapse="|")))
sum(df$subs_iso3c=="n.a.")
sum(df$subs_iso3c=="")

df$subs_iso3c <- gsub("NA", "", df$subs_iso3c)

df$subs_iso3c[which(nchar(df$subs_iso3c)==3)] <- gsub("\\|", "", df$subs_iso3c[which(nchar(df$subs_iso3c)==3)])
df$subs_iso3c[which(df$subs_iso3c=="")] <- NA

df$country_iso_code[which(df$Firm=="NL00150002Q7")] <- "DE"

df$multinational <- 0
df$multinational[which(df$subs_iso3c != df$country_iso_code)] <- 1

## If the subsidiary column isnt EXACTLY equal to the man country column, this
## implies there is a subsidiary in another country.

newdat <- read.csv("data/01_raw/boards_rep_v2/analysis_data_no_proprietary.csv", stringsAsFactors = F)
newdat <- newdat[ , c("gvkey", "year", "cso_exists", "cdp_report")]
df <- merge(df, newdat, by=c("gvkey", "year"), all.x=T)

df$cso_exists[which(is.na(df$cso_exists) & df$year %in% c(2000:2019))] <- 0
df$cdp_report[which(is.na(df$cdp_report) & df$year %in% c(2000:2019))] <- 0

models <- list(
  "(1)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + n_employees + cso_exists + cdp_report | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + n_employees + cso_exists + cdp_report | `Industry x Year`, data=df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_augmented.RData")

## Logit Occurrence Model ------------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm)
)
save(models, file="data/03_final/climate_logit_qrt_bycomponent_MODELS_REVISION_NEW.RData")


## Bills-based measure of climate lobbying -----------------------------------

# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills.rds")
df <- process_df(df)
m1 <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills_support.rds")
df <- process_df(df)
m2 <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills_oppose.rds")
df <- process_df(df)
m3 <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)

models <- list(
  "(1)" = m1,
  "(2)" = m2,
  "(3)" = m3
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_altclimatebills.RData")


## Bills-based measure of climate lobbying -------------------------------------
# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills.rds")
df <- process_df(df)
m1 <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills_support.rds")
df <- process_df(df)
m2 <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altclimatebills_oppose.rds")
df <- process_df(df)
m3 <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)


## Effect of climate exposure on lobbying occurrence -----------------------------------
models <- list(
  "(1)" = m1,
  "(2)" = m2,
  "(3)" = m3
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW_altclimatebills.RData")



## Keywords-based measure of climate lobbying -----------------------------------
# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altkeywords.rds")

names(df)

# Rename fixed effects variables
df <- df |>
  mutate(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

df$CLI_kw <- as.numeric(df$CLI_kw)
df$CLI_mitigation <- as.numeric(df$CLI_mitigation)
df$CLI_adaptation <- as.numeric(df$CLI_adaptation)

df$log_CLI_amount_kw <- log(df$CLI_amount_kw + 1)
df$log_CLI_amount_mitigation <- log(df$CLI_amount_mitigation + 1)
df$log_CLI_amount_adaptation <- log(df$CLI_amount_adaptation + 1)

glimpse(df)

# Occurrence
m1 <- feols(CLI_kw ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)
m2 <- feols(CLI_mitigation ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)
m3 <- feols(CLI_adaptation ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)

# Amount
m4 <- feols(log_CLI_amount_kw ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)
m5 <- feols(log_CLI_amount_mitigation ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)
m6 <- feols(log_CLI_amount_adaptation ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm)


## Effect of climate exposure on lobbying occurrence -----------------------------------
models <- list(
  "(1)" = m1,
  "(2)" = m2,
  "(3)" = m3,
  "(4)" = m4,
  "(5)" = m5,
  "(6)" = m6
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_altkeywords.RData")

## Annual Models ---------------------------------------------------------------------

# load data
#df <- read_rds("data/03_final/lobbying_df_annual_REVISE_normal.rds")
#df <- read_rds("data/03_final/lobbying_df_annual_")

df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

df <- df %>%
  group_by(isin, year, industry, industry_year) %>%
  summarize(CLI_annual = mean(CLI_annual, na.rm=T),
            CLI_amount_annual = mean(CLI_amount_annual, na.rm=T),
            cc_expo_ew = mean(cc_expo_ew, na.rm=T),
            op_expo_ew = mean(op_expo_ew, na.rm=T), 
            rg_expo_ew = mean(rg_expo_ew, na.rm=T), 
            ph_expo_ew = mean(ph_expo_ew, na.rm=T),
            ebit = mean(ebit, na.rm=T),
            ebit_at = mean(ebit_at, na.rm=T),
            us_dummy = mean(us_dummy, na.rm=T),
            total_lobby_annual = mean(total_lobby_annual, na.rm=T))

# Rename fixed effects variables
df <- df |>
  rename(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

df <- df %>% 
  mutate( CLI = as.numeric( CLI_annual ),
          log_CLI_amount = log(CLI_amount_annual + 1))

df <- df %>%
  group_by(Firm) %>%
  mutate(CLI_l1 = lag(CLI, n=1, order_by=Year),
         log_CLI_amount_l1 = lag(log_CLI_amount, n=1, order_by=Year))

## Main Annual Models -----------------------------------------------------

models <- list(
  "(1)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_ols_annual_bycomponent_MODELS_REVISION_NEW.RData")



## Comparing Annual Sautner Model to 10-K based exposure -----------------------

## Compare with 10-Ks -------------------------------------------------------------


tenk_main <- haven::read_dta("data/01_raw/SEC_climate_risk/Excerpt full info.dta")

tenk_aux <- read.csv("data/01_raw/SEC_climate_risk/Ceres_Climate_Risk_Data_BJS - Ceres_Climate_Risk_Data_BJS.csv", 
                     stringsAsFactors = F)

tenk_main <- merge(tenk_main, tenk_aux, by="filename")
tenk_main$year <- tenk_main$filingyear
tenk_main$ticker <- tolower(tenk_main$ticker)

tenk_cy <- tenk_main %>%
  group_by(ticker, year) %>%
  summarize(tenk_exposure = sum(as.numeric(relevance_score.x), na.rm=T))

ticker <- readxl::read_xlsx("data/01_raw/orbis/bbh_orbis_tickersymbols.xlsx", sheet="Results")

df_ticker <- merge(df, ticker, by.x="Firm", by.y="ISIN number") #224,446 of 227,847 obs. have a matching ticker

df_ticker$`Ticker symbol` <- tolower(df_ticker$`Ticker symbol`)
sum(tenk_cy$ticker %in% df_ticker$`Ticker symbol`) #41,727 of 46,190 10-K data obs. have matching 

df_ticker <- merge(df_ticker, tenk_cy, by.x=c("Ticker symbol", "Year"), by.y=c("ticker", "year")) 
#14,852 of 15,396 potential obs. in 10-K data have match in our panel

cortest <- cor(df_ticker[ , c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", "tenk_exposure")],
               use="pairwise.complete.obs")
colnames(cortest) <- c("Overall", "Opportunity", "Regulatory", "Physical", "10-K Exposure")
rownames(cortest) <- c("Overall", "Opportunity", "Regulatory", "Physical", "10-K Exposure")

pdf("results/figures/descriptives/corrplot_expo_tenk.pdf", width=5, height=5)
corrplot::corrplot(cortest)
dev.off()

df_ticker <- df_ticker %>%
  mutate(cc_expo_ew = scale(cc_expo_ew),
         tenk_exposure = scale(tenk_exposure))

df_ticker$CLI[is.na(df_ticker$CLI)] <- 0
df_ticker$log_CLI_amount[is.na(df_ticker$log_CLI_amount)] <- 0
df_ticker$total_lobby_annual[is.na(df_ticker$total_lobby_annual)] <- 0

sautner_mod <- feols(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df_ticker, vcov = ~ Year + Firm)
tenk_mod <- feols(CLI ~ tenk_exposure + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df_ticker, vcov = ~ Year + Firm)

sautner_mod_amt <- feols(log_CLI_amount ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df_ticker, vcov = ~ Year + Firm)
tenk_mod_amt <- feols(log_CLI_amount ~ tenk_exposure + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df_ticker, vcov = ~ Year + Firm)

out = list(sautner_mod, tenk_mod, sautner_mod_amt, tenk_mod_amt)

save(out, file="data/03_final/climate_logit_yr_compare10K_MODELS_REVISION_NEW.RData")

## Annual coalition-based pro/anti-climate lobbying analysis -------------------------------------


df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")
df <- df[!is.na(df$industry) , ]
df <- df %>%
  group_by(gvkey, year, industry, industry_year) %>%
  summarize(CLI_annual = mean(CLI_annual, na.rm=T),
            CLI_amount_annual = mean(CLI_amount_annual, na.rm=T),
            cc_expo_ew = mean(cc_expo_ew, na.rm=T),
            op_expo_ew = mean(op_expo_ew, na.rm=T), 
            rg_expo_ew = mean(rg_expo_ew, na.rm=T), 
            ph_expo_ew = mean(ph_expo_ew, na.rm=T),
            ebit = mean(ebit, na.rm=T),
            ebit_at = mean(ebit_at, na.rm=T),
            us_dummy = mean(us_dummy, na.rm=T),
            total_lobby_annual = mean(total_lobby_annual, na.rm=T))

# Rename fixed effects variables
df <- df |>
  rename(
    #Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

df <- df %>% 
  mutate( CLI = as.numeric( CLI_annual ),
          log_CLI_amount = log(CLI_amount_annual + 1))

df <- df %>%
  group_by(gvkey) %>%
  mutate(CLI_l1 = lag(CLI, n=1, order_by=Year),
         log_CLI_amount_l1 = lag(log_CLI_amount, n=1, order_by=Year))

coal <- read.csv("data/01_raw/coalitions/Lerner and Osgood 2022 replication/analysis_data_no_proprietary.csv", stringsAsFactors = F)


# Edit coalition data ---------------------------------------------------

coal <- coal[ , c("gvkey", "year", "numsupcoal", "numoppcoal")]

coal |> tabyl(numsupcoal)
coal |> tabyl(numoppcoal)
coal |> tabyl(numsupcoal, numoppcoal) # not much overlap

# Create dummy
coal <- coal |>
  mutate(
    sup_climate_action = ifelse(numsupcoal > 0, 1, 0),
    opp_climate_action = ifelse(numoppcoal > 0, 1, 0),
    gvkey = as.character(gvkey)
  )

# How many not part at any point?
# support
coal |>
  group_by(gvkey) %>%
  summarise(support_count = sum(sup_climate_action),
            opposition_count = sum(opp_climate_action)) %>%
  filter(support_count == 0 & opposition_count == 0) |>
  nrow()

coal |> tabyl(sup_climate_action, opp_climate_action) # not much overlap: 274 firm-years / 45 firms

# Merge with df

df$year <- as.numeric(df$Year)
coal$gvkey <- as.integer(coal$gvkey)
df2 <- df |>
  left_join(coal, by = c("gvkey", "year"))

df2 |> tabyl(sup_climate_action)
df2 |> tabyl(opp_climate_action)

# Code directionality -----------------------------------------------------


## Long df --------------------------------------------------------------

df3 <- df2 |>
  #mutate(CLI = ifelse(grepl("ENV|CAW|ENG|FUE", issue_code), 1, 0)) |>
  mutate(pro_CLI = ifelse(CLI == 1 & sup_climate_action == 1 & opp_climate_action == 0, 1, 0), 
         contra_CLI = ifelse(CLI == 1 & opp_climate_action == 1 & sup_climate_action == 0, 1, 0),
         pro_log_CLI_amount = log_CLI_amount * sup_climate_action,
         opp_log_CLI_amount = log_CLI_amount * opp_climate_action) |>
  mutate(
    direction_CLI = case_when(
      pro_CLI == 1 ~ "Pro", 
      contra_CLI == 1 ~ "Contra",
      CLI == 1 & sup_climate_action == 1 & opp_climate_action == 1 ~ "Both", 
      CLI == 1 & sup_climate_action != 1 & opp_climate_action != 1 ~ "None")
  )

insp <- df3[ , c("gvkey", "year", "CLI", "pro_CLI", "direction_CLI", 
                 "sup_climate_action", "contra_CLI", "opp_climate_action")]

df3 |> tabyl(pro_CLI)
df3 |> tabyl(contra_CLI)

desc <- df3 |>
  filter(CLI == 1)
desc <- desc[ , c("gvkey", "year", "direction_CLI")]

##

# Rename fixed effects variables
df <- df3 |>
  rename(Firm = gvkey)

# Change classes for analysis ---------------------------------------------

sum(is.na(df$pro_CLI))
sum(is.na(df$contra_CLI))
sum(is.na(df$sup_climate_action))
sum(is.na(df$opp_climate_action))

## make pro/anti climate lobbying NA if no association membership data for that firm
df$pro_CLI[is.na(df$sup_climate_action)] <- NA
df$contra_CLI[is.na(df$sup_climate_action)] <- NA

df$pro_log_CLI_amount[is.na(df$pro_log_CLI_amount)] <- NA
df$opp_log_CLI_amount[is.na(df$opp_log_CLI_amount)] <- NA

## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "(1)" = feols(pro_CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(2)" = feols(contra_CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(3)" = feols(sup_climate_action ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(4)" = feols(opp_climate_action ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(4)" = feols(pro_log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(4)" = feols(opp_log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | `Industry x Year`, df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_ols_yr_coalition_MODELS_REVISION_NEW.RData")


### END