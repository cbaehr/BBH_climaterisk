
rm(list=ls())
options(scipen = 999)

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, arrow)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

###

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

### Define the Prediction Function ---------------------------------------------

predict.values <- function(model, expo, type="occurrence") {
  
  for(i in yearqtrs) {
      if(i==yearqtrs[1]) {out <- c(); out_pct <- c()}
      
      yr <- as.numeric(substr(i, 1, 4))
      iy <- df[which(df$Industry.x.Year == sprintf("Transport Manufacturing %s", yr) & df$yearqtr==i) , ]
      
      if(expo=="op_expo_ew") {
        op <- mean(iy$op_expo_ew, na.rm=T)
        rg <- median(iy$rg_expo_ew, na.rm=T)
        ph <-  median(iy$ph_expo_ew, na.rm=T)
      } else if(expo=="rg_expo_ew") {
        op <- median(iy$op_expo_ew, na.rm=T)
        rg <- mean(iy$rg_expo_ew, na.rm=T)
        ph <-  median(iy$ph_expo_ew, na.rm=T)
      } else {
        op <- median(iy$op_expo_ew, na.rm=T)
        rg <- median(iy$rg_expo_ew, na.rm=T)
        ph <-  mean(iy$ph_expo_ew, na.rm=T)
      }
      
      syn_firm <- data.frame("op_expo_ew" = op,
                             "rg_expo_ew" = rg,
                             "ph_expo_ew" = ph,
                             "ebit" = median(iy$ebit, na.rm=T),
                             "ebit_at" = median(iy$ebit_at, na.rm=T),
                             "us_dummy" = 1,
                             "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                             "Industry x Year" = sprintf("Transport Manufacturing %s", yr))
      names(syn_firm)[8] <- "Industry x Year"
      
      test1 <- predict(object = model, newdata = syn_firm, type = "response")
      syn_firm[ , expo] <- syn_firm[ , expo] + 2*sd(iy[ , expo], na.rm=T)
      test2 <- predict(object = model, newdata = syn_firm, type = "response")
      
      amount1 <- test1
      amount2 <- test2
      if(type=="amount") {
        amount1 <- exp(amount1) - 1
        amount2 <- exp(amount2) - 1
      }
      out <- c(out, amount2 - amount1)
      out_pct <- c(out_pct, ((amount2 - amount1) / amount1))
  }
  return(list(out=out, out_pct=out_pct))
}



## OLS Occurrence prediction ---------------------------------------------------

# load data
#df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")
df <- read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")
df <- process_df(df)
df <- data.frame(df)

yearqtrs <- unique(df$yearqtr)

#load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION.RData")
load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]

## Opportunity - predicted change in lobbying occurrence -----------------------

out <- predict.values(mod5, expo="op_expo_ew")

P_inc <- paste0(round(mean(out$out, na.rm=T), 3)*100, " pp")
sprintf("Probability of lobbying on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out$out_pct, na.rm=T), 3)*100)
sprintf("Probability of lobbying on climate issues increases by %s percent", Pct_inc)

## 

out <- predict.values(mod5, expo="rg_expo_ew")

P_inc <- paste0(round(mean(out$out, na.rm=T), 3)*100, " pp")
sprintf("Probability of lobbying on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out$out_pct, na.rm=T), 3)*100)
sprintf("Probability of lobbying on climate issues increases by %s percent", Pct_inc)

##

out <- predict.values(mod5, expo="ph_expo_ew")

P_inc <- paste0(round(mean(out$out, na.rm=T), 3)*100, " pp")
sprintf("Probability of lobbying on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out$out_pct, na.rm=T), 3)*100)
sprintf("Probability of lobbying on climate issues increases by %s percent", Pct_inc)


## Opportunity - predicted change in lobbying expenditure ----------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]

out <- predict.values(mod5, expo="op_expo_ew", type="amount")

P_inc <- paste0("$", round(mean(out$out, na.rm=T), 3)*100)
sprintf("Average lobbying expenditure on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out$out_pct, na.rm=T), 3)*100)
sprintf("Average lobbying expenditure on climate issues increases by %s percent", Pct_inc)

## 

out <- predict.values(mod5, expo="rg_expo_ew", type="amount")

P_inc <- paste0("$", round(mean(out$out, na.rm=T), 3)*100)
sprintf("Average lobbying expenditure on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out$out_pct, na.rm=T), 3)*100)
sprintf("Average lobbying expenditure on climate issues increases by %s percent", Pct_inc)

##

out <- predict.values(mod5, expo="ph_expo_ew", type="amount")

P_inc <- paste0("$", round(mean(out$out, na.rm=T), 3)*100)
sprintf("Average lobbying expenditure on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out$out_pct, na.rm=T), 3)*100)
sprintf("Average lobbying expenditure on climate issues increases by %s percent", Pct_inc)


## Toyota and Ford Example -----------------------------------------------------

toyo <- df[which(df$conm=="TOYOTA MOTOR CORPORATION" & df$yearqtr=="2018_1") , ]
ford <- df[which(df$conm=="FORD MOTOR COMPANY" & df$yearqtr=="2018_1") , ]

toyo_modeldata <- toyo[ , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at",
                  "us_dummy", "total_lobby_quarter", "Industry.x.Year")]
names(toyo_modeldata)[8] <- "Industry x Year"

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]

toyo_pred <- predict(object = mod5, newdata = toyo_modeldata, type = "response")













load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]















