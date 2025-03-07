
rm(list=ls())
options(scipen = 999)

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

###

# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

# Rename fixed effects variables
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

### Define the Function --------------------------------------------------------

predict.values <- function(model, expo, type="occurrence") {
  
  for(i in yearqtrs) {
    for(j in industries) {
      yr <- as.numeric(substr(i, 1, 4))
      if(i==yearqtrs[1] & j==industries[1]) {out <- c(); out_pct <- c()}
      iy <- df[which(df$`Industry x Year` == sprintf("%s %s", j, yr) & df$yearqtr==i) , ]
      
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
                             "Industry x Year" = sprintf("%s %s", j, yr))
      names(syn_firm)[8] <- "Industry x Year"
      
      test1 <- predict(object = model, newdata = syn_firm, type = "response")
      syn_firm[ , expo] <- syn_firm[ , expo] + 2*sd(iy[ , expo], na.rm=T)
      #syn_firm$op_expo_ew <- syn_firm$op_expo_ew + 2*sd(iy$op_expo_ew, na.rm=T)
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
  }
  return(list(out=out, out_pct=out_pct))
}



## OLS Occurrence prediction ---------------------------------------------------

#load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION.RData")
load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]

yearqtrs <- unique(df$yearqtr)

#industries <- unique(df$industry)
industries <- "Transport Manufacturing"


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

##

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]

out <- predict.values(mod5, expo="op_expo_ew", type="amount")

mean(out$out, na.rm=T)

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

##



for(i in yearqtrs) {
  yr <- as.numeric(substr(i, 1, 4))
  
  if(i==yearqtrs[1]) {out <- c(); out_pct <- c()}
  #iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", yr) & df$yearqtr==i) , ]
  
  syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = mean(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit_at, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         #"Industry x Year" = sprintf("Transport Manufacturing %s", i))
                         "Industry x Year" = sprintf("Transport Manufacturing %s", yr))
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$rg_expo_ew <- syn_firm$rg_expo_ew + 2*sd(iy$rg_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
  #sprintf("Probability of lobbying moves from %s to %s", test1, test2)
  amount1 <- test1
  amount2 <- test2
  out <- c(out, amount2 - amount1)
  out_pct <- c(out_pct, (amount2 - amount1) / amount1)
}
P_inc <- paste0(round(mean(out, na.rm=T), 3)*100, " pp")
sprintf("Probability of lobbying on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out_pct, na.rm=T), 3)*100)
sprintf("Probability of lobbying on climate issues increases by %s percent", Pct_inc)

#for(i in 2001:2023) {
for(i in yearqtrs) {
  yr <- as.numeric(substr(i, 1, 4))
  
  #if(i==2001) {out <- c(); out_pct <- c()}
  if(i==yearqtrs[1]) {out <- c(); out_pct <- c()}
  #iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", yr) & df$yearqtr==i) , ]
  
  syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = mean(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit_at, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = sprintf("Transport Manufacturing %s", yr))
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$ph_expo_ew <- syn_firm$ph_expo_ew + 2*sd(iy$ph_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
  amount1 <- test1
  amount2 <- test2
  out <- c(out, amount2 - amount1)
  out_pct <- c(out_pct, (amount2 - amount1) / amount1)
}
P_inc <- paste0(round(mean(out, na.rm=T), 6)*100, " pp")
sprintf("Probability of lobbying on climate issues increases by %s", P_inc)
Pct_inc <- paste0(round(mean(out_pct, na.rm=T), 3)*100)
sprintf("Probability of lobbying on climate issues increases by %s percent", Pct_inc)

## OLS Amount prediction ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")
mod5 <- models[[5]]

unique(grep("Transport Manufacturing", df$`Industry x Year`, value=T))

#for(i in 2001:2023) {
for(i in yearqtrs) {
  yr <- as.numeric(substr(i, 1, 4))
  
  #if(i==2001) {out <- c(); out_pct <- c()}
  if(i==yearqtrs[1]) {out <- c(); out_pct <- c()}
  #iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", yr) & df$yearqtr==i) , ]
  
  syn_firm <- data.frame("op_expo_ew" = mean(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit_at, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = sprintf("Transport Manufacturing %s", yr))
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$op_expo_ew <- syn_firm$op_expo_ew + 2*sd(iy$op_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
  amount1 <- exp(test1) - 1
  amount2 <- exp(test2) - 1
  out <- c(out, amount2 - amount1)
  out_pct <- c(out_pct, (amount2 - amount1) / amount1)
}
P_inc <- paste0(round(mean(out, na.rm=T), 3))
sprintf("Lobbying expenditure on climate issues increases by $%s", P_inc)
Pct_inc <- paste0(round(mean(out_pct, na.rm=T), 3)*100)
sprintf("Lobbying expenditure on climate issues increases by %s percent", Pct_inc)

#for(i in 2001:2023) {
for(i in yearqtrs) {
  yr <- as.numeric(substr(i, 1, 4))
  
  #if(i==2001) {out <- c(); out_pct <- c()}
  if(i==yearqtrs[1]) {out <- c(); out_pct <- c()}
  #iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", yr) & df$yearqtr==i) , ]
  
  syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = mean(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit_at, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = sprintf("Transport Manufacturing %s", yr))
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$rg_expo_ew <- syn_firm$rg_expo_ew + 2*sd(iy$rg_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")

  amount1 <- exp(test1) - 1
  amount2 <- exp(test2) - 1
  out <- c(out, amount2 - amount1)
  out_pct <- c(out_pct, (amount2 - amount1) / amount1)
}
P_inc <- paste0(round(mean(out, na.rm=T), 3))
sprintf("Lobbying expenditure on climate issues increases by $%s", P_inc)
Pct_inc <- paste0(round(mean(out_pct, na.rm=T), 3)*100)
sprintf("Lobbying expenditure on climate issues increases by %s percent", Pct_inc)

#for(i in 2001:2023) {
for(i in yearqtrs) {
  yr <- as.numeric(substr(i, 1, 4))
  
  #if(i==2001) {out <- c(); out_pct <- c()}
  if(i==yearqtrs[1]) {out <- c(); out_pct <- c()}
  #iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", yr) & df$yearqtr==i) , ]
  
  syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = mean(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit_at, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = sprintf("Transport Manufacturing %s", yr))
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$ph_expo_ew <- syn_firm$ph_expo_ew + 2*sd(iy$ph_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")

  amount1 <- exp(test1) - 1
  amount2 <- exp(test2) - 1
  
  out <- c(out, amount2 - amount1)
  out_pct <- c(out_pct, (amount2 - amount1) / amount1)
}
P_inc <- paste0(round(mean(out, na.rm=T), 3))
sprintf("Lobbying expenditure on climate issues increases by $%s", P_inc)
Pct_inc <- paste0(round(mean(out_pct, na.rm=T), 3)*100)
sprintf("Lobbying expenditure on climate issues increases by %s percent", Pct_inc)




