
rm(list=ls())

# install_github("insongkim/concordance", dependencies=TRUE)
# Load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary, readxl)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

## Obs 1 ---------------------------------------------------------------------







