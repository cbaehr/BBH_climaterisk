### Firms & Lobbying
### Data normalization

rm(list=ls())

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("C:/Users/fiona/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

# load packages
pacman::p_load(data.table, tidyverse)

#Load data
df <- fread("data/03_final/lobbying_df_quarterly_REVISE.csv")
df <- data.frame(df)

#Normalize variables for interpretation 

## continuous variables in regression models
# df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew",
#                        "ebit_at", "total_lobby_quarterly")
df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")
## pull from main data
df_wide_cont <- df[, df_wide_cont_vars]
## rescale to standard normal
df_wide_cont <- scale(df_wide_cont)
## slot back into main df_wide
df[, df_wide_cont_vars] <- df_wide_cont


# write csv
fwrite(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.csv")

### End


#Load data
df <- fread("data/03_final/lobbying_df_REVISE.csv")
df <- data.frame(df)
#Normalize variables for interpretation 

## continuous variables in regression models
df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")
# df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew",
#                        "ebit_at", "total_lobby_annual")
## pull from main data
df_wide_cont <- df[, df_wide_cont_vars]
df_wide_cont <- data.frame(apply(df_wide_cont, 2, as.numeric))

## rescale to standard normal
df_wide_cont <- scale(df_wide_cont)
## slot back into main df_wide
df[, df_wide_cont_vars] <- df_wide_cont


# write csv
fwrite(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.csv")

### End
