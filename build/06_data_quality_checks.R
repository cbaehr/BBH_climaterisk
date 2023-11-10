
rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, modelsummary)
options(scipen=999)

if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
df <- read.csv("data/03_final/lobbying_df_annual_REVISE_normal.csv", stringsAsFactors = F)

df$ph_expo_ew <- as.numeric(df$ph_expo_ew)
df$op_expo_ew <- as.numeric(df$op_expo_ew)
df$total_assets_usd <- as.numeric(df$total_assets_usd)
df$ebit <- as.numeric(df$ebit)

hist(df$total_assets_usd)
unique(df$conm[which(df$total_assets_usd>3000000000)])
## these seem like reasonable companies
hist(df$total_assets_usd[which(df$total_assets_usd<300000000)])
## see a bit more expected gradation when omitting the major firms

hist(df$ebit)
unique(df$conm[which(df$ebit>50000000)])
## again, seem like reasonable companies

df$ebit_at <- df$ebit / df$at
hist(df$ebit_at)
unique(df$conm[which(df$ebit_at>40000)])
## no strong expectations here

## generating COMPANY level z-scores for total assets - values further away from
## zero suggest increasingly UNLIKELY values to be realized for a company
test <- tapply(df$total_assets_usd, INDEX = list(df$conm), FUN = function(x) scale(x))
test <- unlist(test)
hist(test)
summary(test)
## a few well out in the tails 

which.min(test)
which.max(test)

df$at[which(df$conm=="D'IETEREN GROUP SA/NV")] 
## this spike is legit actually -- confirmed with FT

df$at[which(df$conm=="HUNTSMAN CORPORATION")] 
## this spike after 2002 also looks legit 
## https://companiesmarketcap.com/huntsman/total-assets/

## as a robustness check, we may want to define a threshold of these "pseudo z scores"
## over which we omit the row from the data or censor the value at 2-standard deviations.
## similar to Winsorizing


