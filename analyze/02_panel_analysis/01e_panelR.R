
### Firms & Lobbying
### Analysis

### Quarterly

rm(list=ls())

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, corrplot, janitor, 
               mice, censReg, arrow, panelr, clubSandwich)
#miceadds

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="cb8007" ) {setwd("/scratch/gpfs/cb8007/BBH/")}
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
  df <- data.frame(data)
  df <- df |>
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
  
  df$ebit <- as.numeric(scale(df$ebit))
  df$ebit_at <- as.numeric(scale(df$ebit_at))
  df$total_lobby_quarter <- as.numeric(scale(df$total_lobby_quarter))
  df$total_lobby_annual <- as.numeric(scale(df$total_lobby_annual))
  df$n_employees <- as.numeric(scale(df$n_employees))
  
  return(df)
}





## Estimate PanelR

#df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")
df <- read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")
df <- process_df(df)

df$`Industry x Quarter` <- paste(df$industry, df$yearqtr)

uniquequarters <- sort(unique(df$yearqtr))

df$yearqtr_fac <- factor(df$yearqtr, levels = uniquequarters, ordered=T)

df_panel <- panel_data(df, id = Firm, wave = yearqtr_fac)

model <- wbm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | us_dummy, 
             data=df_panel, model = "w-b")
model_clust <- summary(model, vcov = "CR2")  # Cluster-robust SEs using CR2

model_clust$n <- model@summ_atts$n
model_clust$within_table
save(model_clust, file="data/03_final/panelR_occurrence_results.RData")


model <- wbm(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | us_dummy, 
             data=df_panel, model = "w-b")
model_clust <- summary(model, vcov = "CR2")  # Cluster-robust SEs using CR2

model_clust$n <- model@summ_atts$n
model_clust$within_table
save(model_clust, file="data/03_final/panelR_amount_results.RData")











