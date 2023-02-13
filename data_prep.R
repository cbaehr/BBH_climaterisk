### Data preparation ###

# Merge Climate Change Exposure data with controls and political behavior data

rm(list=ls())

# load packages
library(data.table)
library(tidyverse)
library(countrycode)
library(readxl)

# set working directory
setwd("C:/Users/fiona/Dropbox (Princeton)/BBH/BBH1/data")
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/data/")}

# Import datasets ---------------------------------------------------------

### Climate change exposure
#import quarterly data 
ccexposure_q <- fread("CC Exposure/cc_firmquarter_2021Q4_03082021_OSF (1).csv")
#import yearly data 
ccexposure_y <- fread("CC Exposure/cc_firmyear_2021Q4_03082021_OSF (1).csv")

### WSJ news data 
wsj <- read_excel("Misc/WSJ/EGLKS_dataupdated.xlsx")

###import compustat financial data 
compustat <- fread("Misc/compustat_102422.csv")
compustatna <- fread("Misc/compustat_northamerica.csv")
vars <- names(compustat)[names(compustat) %in% names(compustatna)]
compustat <- compustat[, ..vars]
compustatna <- compustatna[, ..vars]
compustat <- rbind(compustat, compustatna)

compustat <- compustat[order(compustat[, c("gvkey", "fyear")]), ] # ensure if we drop duplicates, we drop the LATER observations from a given year
compustat <- compustat[!duplicated(compustat[, c("gvkey", "fyear")])] # drop duplicate year-firm observations

### Refinitive esg data 
esg <- fread("Misc/refinitiveesg.csv")

### ESG variables
company_en <- fread("Misc/company_en.csv")

### Sic codes
sic <- fread("Misc/sic_2_digit_codes.csv") |>
  mutate(sic = as.integer(sic)) |>
  filter(!is.na(sic))

sic <- sic[!duplicated(sic), ]

# Merge -------------------------------------------------------------------

### Prepare datasets for merging
# Merge compustat with sic codes
compustat$sic <- strtrim(compustat$sic, 2)

compustat <- compustat |>
  mutate(sic = as.integer(sic))

compustat <- compustat |> 
  left_join(sic, by = "sic")

### Merge ESG dataset with company data
esg <- esg |>
  # make sure no duplicates
  select(-c(tick, Cusip,Sedol,OAId)) |>
  filter(Isin != "") |>
  distinct() |>
  # merge
  left_join(company_en, by = c("OrgID", "FisYear"))


### Merge
# quarterly
ccexposure_qfull <- ccexposure_q |>
  mutate(country_name = countrycode(hqcountrycode, origin = 'iso2c', destination = 'country.name')) |>
  left_join(compustat, by = c("year" = "fyear", "gvkey")) |>
  left_join(esg, by = c("isin" = "Isin", "year" = "FisYear"))

# annual
ccexposure_afull <- ccexposure_y |>
  mutate(country_name = countrycode(hqcountrycode, origin = 'iso2c', destination = 'country.name')) |>
  left_join(compustat, by = c("year" = "fyear", "gvkey")) |>
  left_join(esg, by = c("isin" = "Isin", "year" = "FisYear"))


# Transform variables -----------------------------------------------------

#transform climate change exposure variables x1000
ccexposure_qfull <- ccexposure_qfull |>
  mutate(across(c(cc_expo_ew, cc_risk_ew, cc_pos_ew, cc_neg_ew, cc_sent_ew, op_expo_ew, op_risk_ew, op_pos_ew, op_neg_ew, op_sent_ew, rg_expo_ew, rg_risk_ew, rg_pos_ew, rg_neg_ew, rg_sent_ew, ph_expo_ew, ph_risk_ew, ph_pos_ew, ph_neg_ew, ph_sent_ew), list(~.*1000))) |>
  rename(ccexp = cc_expo_ew_1,
         ccrisk = cc_risk_ew_1,
         ccpos = cc_pos_ew_1,
         ccneg = cc_neg_ew_1,
         ccsent = cc_sent_ew_1,
         opexpo = op_expo_ew_1,
         oprisk = op_risk_ew_1,
         oppos = op_pos_ew_1,
         opneg = op_neg_ew_1,
         opsent = op_sent_ew_1,
         rgexpo = rg_expo_ew_1, 
         rgrisk = rg_risk_ew_1,
         rgpos = rg_pos_ew_1,
         rgneg = rg_neg_ew_1,
         rgsent = rg_sent_ew_1,
         phexpo = ph_expo_ew_1,
         phrisk = ph_risk_ew_1,
         phpos = ph_pos_ew_1,
         phneg = ph_neg_ew_1,
         phsent = ph_sent_ew_1)

ccexposure_afull <- ccexposure_afull |>
  mutate(across(c(cc_expo_ew, cc_risk_ew, cc_pos_ew, cc_neg_ew, cc_sent_ew, op_expo_ew, op_risk_ew, op_pos_ew, op_neg_ew, op_sent_ew, rg_expo_ew, rg_risk_ew, rg_pos_ew, rg_neg_ew, rg_sent_ew, ph_expo_ew, ph_risk_ew, ph_pos_ew, ph_neg_ew, ph_sent_ew), list(~.*1000))) |>
  rename(ccexp = cc_expo_ew_1,
         ccrisk = cc_risk_ew_1,
         ccpos = cc_pos_ew_1,
         ccneg = cc_neg_ew_1,
         ccsent = cc_sent_ew_1,
         opexpo = op_expo_ew_1,
         oprisk = op_risk_ew_1,
         oppos = op_pos_ew_1,
         opneg = op_neg_ew_1,
         opsent = op_sent_ew_1,
         rgexpo = rg_expo_ew_1, 
         rgrisk = rg_risk_ew_1,
         rgpos = rg_pos_ew_1,
         rgneg = rg_neg_ew_1,
         rgsent = rg_sent_ew_1,
         phexpo = ph_expo_ew_1,
         phrisk = ph_risk_ew_1,
         phpos = ph_pos_ew_1,
         phneg = ph_neg_ew_1,
         phsent = ph_sent_ew_1)



# Write .csv dfs ----------------------------------------------------------
fwrite(x = ccexposure_afull, file = "df_year_fb.csv")
fwrite(x = ccexposure_qfull, file = "df_quarterly_fb.csv")

# End