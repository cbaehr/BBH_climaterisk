### Firms & Lobbying
### Analysis

### Quarterly

rm(list=ls())

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, corrplot)

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
        "log_CLI_amount_l1" = "Lagged DV"
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

## Effect of climate exposure on lobbying occurrence -----------------------------------
models <- list(
  "(1)" = m1,
  "(2)" = m2,
  "(3)" = m3
)
save(models, file="data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_altclimatebills.RData")


## OLS - Amount -----------------------------------------------------

## Bills-based measure of climate lobbying
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

### END