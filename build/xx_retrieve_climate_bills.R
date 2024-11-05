# Script to retrieve climate bills

# Input: .csv file downloaded from Congress.gov search.

# Query input:
# "climate change" OR "global warming" OR "greenhouse gas" OR "carbon emission" OR "cap and trade" OR "low carbon" OR "carbon pricing" OR "carbon capture" OR "carbon tax" OR "methane emission" OR "renewable energy" OR "clean energy" OR "renewable electricity" OR "climate mitigation" OR "climate adaptation"

rm(list=ls())

# load packages
pacman::p_load(tidyverse, data.table)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
bills <- fread("data/01_raw/bills/search_results_2024-11-05_0336pm.csv")
lobbying <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")
# load lobbying data
lobby_client <- fread("data/01_raw/lobbyview/dataset___client_level.csv")
lobby_text <- fread("data/01_raw/lobbyview/dataset___issue_text.csv")
lobby_issue <- fread("data/01_raw/lobbyview/dataset___issue_level.csv")
lobby_report <- fread("data/01_raw/lobbyview/dataset___report_level.csv")
