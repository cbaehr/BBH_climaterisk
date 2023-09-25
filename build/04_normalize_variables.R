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
df <- fread("data/03_final/lobbying_df_wide_reduced.csv") |>
  data.frame()


#Normalize variables for interpretation 

## continuous variables in regression models
df_wide_cont_vars <- c("cc_expo_ew_y", "cc_expo_ew_q", "op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y",
                       "ebit", "at", "total_lobby")
## pull from main data
df_wide_cont <- df[, df_wide_cont_vars]
## rescale to standard normal
df_wide_cont <- scale(df_wide_cont)
## slot back into main df_wide
df[, df_wide_cont_vars] <- df_wide_cont


# write csv
fwrite(df, file="data/03_final/lobbying_df_wide_reduced_normal.csv")

### End