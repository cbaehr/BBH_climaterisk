
rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, modelsummary)
options(scipen=999)

if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

df <- read.csv("data/03_final/lobbying_df_annual_REVISE_normal.csv", stringsAsFactors = F)

df$ph_expo_ew <- as.numeric(df$ph_expo_ew)
df$op_expo_ew <- as.numeric(df$op_expo_ew)

hist(df$at)
unique(df$conm[which(df$at>3000000000)])
## these seem like reasonable companies
hist(df$at[which(df$at<300000000)])
## see a bit more expected gradation when omitting the major firms

hist(df$ebit)
unique(df$conm[which(df$ebit>50000000)])
## again, seem like reasonable companies

df$ebit_at
hist(df$ebit_at)
unique(df$conm[which(df$ebit_at>40000)])
## no strong expectations here

## generating COMPANY level z-scores for total assets - values further away from
## zero suggest increasingly UNLIKELY values to be realized for a company
test <- tapply(df$at, INDEX = list(df$conm), FUN = function(x) scale(x))
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

# @Christian: I agree, that sounds reasonable



# Diagnose with dlookr ----------------------------------------------------

pacman::p_load(dlookr)

# simple diagnose
diag_df <- diagnose(df)

# check missing: plot top 15 variables with most missing values
missingness <- df %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>% 
  filter(missing_count > 0) %>% 
  arrange(desc(missing_count))
# 90% missingness lobbying
# 60% missingness exposure
# 40% missingness orbis

# diagnose numeric
numerics <- df %>%
  select_if(is.numeric) %>%
  diagnose_numeric()

# diagnose outliers
outliers <- df %>%
  select_if(is.numeric) %>%
  diagnose_outlier()

# get variables with most outliers
outliers %>%
  arrange(desc(outliers_cnt)) %>%
  head(30)
# ebit, ebit_at, and at have the most outliers
# also total_lobby_annual
hist(df$total_lobby_annual)
summary(df$total_lobby_annual)

# plot outliers for total_lobby_annual
df %>%
  plot_outlier(total_lobby_annual)

# get highest 50 outliers
df %>%
  arrange(desc(total_lobby_annual)) %>%
  head(50) %>%
  select(conm, total_lobby_annual, year)
# Seems reasonable

## Look at CLI_amount_annual
df %>%
  plot_outlier(CLI_amount_annual)

# get highest 50 outliers
df %>%
  arrange(desc(CLI_amount_annual)) %>%
  head(50) %>%
  select(conm, CLI_amount_annual, year)
# Seems reasonable

# Look at ebit/at
summary(df$ebit_at)
# Infinity

# Get observations where ebit_at == Inf
df %>%
  filter(ebit_at == Inf) %>%
  select(conm, ebit, at, ebit_at, year)
# We should code at == 1 in these cases

### END