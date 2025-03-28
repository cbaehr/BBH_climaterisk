# Script to retrieve firms that were exposed to climate and lobbied on specific bills

# rm(list=ls())

# load packages
pacman::p_load(tidyverse)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- arrow::read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")
df_kw <- arrow::read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW_altkeywords.parquet")

# Filter: Firms that were exposed to climate
firms_exposed <- df %>%
  filter(op_expo_ew > 0 | rg_expo_ew > 0 | ph_expo_ew > 0) %>%
  distinct(isin)

# Filter: Firms that lobbied on climate
firms_lobby_CLI <- df %>%
  filter(CLI_quarter > 0) %>%
  distinct(isin)

firms_lobby_CLI_kw <- df_kw %>%
  filter(CLI_kw > 0) %>%
  distinct(isin)


# Firms that were exposed to climate and lobbied on climate
firms_exposed_lobby_CLI <- firms_exposed %>%
  filter(isin %in% firms_lobby_CLI$isin | isin %in% firms_lobby_CLI_kw$isin) %>%
  distinct(isin)
nrow(firms_exposed_lobby_CLI)

# Save
saveRDS(firms_exposed_lobby_CLI, "data/xx_other/firms_exposed_lobby_CLI_new.rds")
fwrite(firms_exposed_lobby_CLI, "data/xx_other/firms_exposed_lobby_CLI_new.csv")

glimpse(firms_exposed_lobby_CLI)

### END