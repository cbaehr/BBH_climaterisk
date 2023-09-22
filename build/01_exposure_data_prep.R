### Climate Lobbying

### Data preparation 
# Step 1: Merge Climate Change Exposure data with controls and political behavior data

rm(list=ls())

# install pacman if not done
# install.packages("pacman")


# load packages
pacman::p_load(data.table, tidyverse, countrycode, readxl)

# set working directory
if(Sys.info()["user"]=="fiona"){setwd("C:/Users/fiona/Dropbox (Princeton)/BBH/BBH1/data")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/data/")}

# Import datasets ---------------------------------------------------------

### Climate change exposure
# import quarterly data 
ccexposure_q <- fread("01_raw/exposure/cc_firmquarter_2021Q4_03082021_OSF.csv")
# import yearly data 
ccexposure_y <- fread("01_raw/exposure/cc_firmyear_2021Q4_03082021_OSF.csv")

# #sum(ccexposure_q$cc_expo_ew[which(ccexposure_q$isin=="AEA002001013" & ccexposure_q$year==2018)])/4
# #ccexposure_y$cc_expo_ew[which(ccexposure_y$isin=="AEA002001013" & ccexposure_y$year==2018)]
# 
# ### WSJ news data 
# wsj <- read_excel("Misc/WSJ/EGLKS_dataupdated.xlsx")
# 
# ###import compustat financial data 
# #compustat <- fread("Misc/compustat_102422.csv")
# #compustatna <- fread("Misc/compustat_northamerica.csv")
# 
# compustatglobal <- fread("/Users/christianbaehr/Downloads/compustat_Global_annual_6-6-2022.csv")
# compustatna <- fread("/Users/christianbaehr/Downloads/compustat_NA_annual_6-6-2022.csv")
# 
# compustatglobal$uniqueid <- paste(compustatglobal$gvkey, compustatglobal$fyear)
# compustatna$uniqueid <- paste(compustatna$gvkey, compustatna$fyear)
# 
# compustatglobal$dup <- duplicated(compustatglobal$uniqueid) | duplicated(compustatglobal$uniqueid, fromLast = T)
# compustatna$dup <- duplicated(compustatna$uniqueid) | duplicated(compustatna$uniqueid, fromLast = T)
# 
# compustatglobal$maxdup <- NA
# compustatna$maxdup <- NA
# 
# for(i in 1:nrow(compustatglobal)) {
#   if(compustatglobal$dup[i]) {
#     dates <- compustatglobal$datadate[which(compustatglobal$gvkey==compustatglobal$gvkey[i] & compustatglobal$fyear==compustatglobal$fyear[i])]
#     if (compustatglobal$datadate[i] == max(dates)) {
#       compustatglobal$maxdup[i] <- T
#     } else {
#       compustatglobal$maxdup[i] <- F
#     }
#   } else {
#     compustatglobal$maxdup[i] <- T
#   }
# }
# 
# for(i in 1:nrow(compustatna)) {
#   if(compustatna$dup[i]) {
#     dates <- compustatna$datadate[which(compustatna$gvkey==compustatna$gvkey[i] & compustatna$fyear==compustatna$fyear[i])]
#     if (compustatna$datadate[i] == max(dates)) {
#       compustatna$maxdup[i] <- T
#     } else {
#       compustatna$maxdup[i] <- F
#     }
#   } else {
#     compustatna$maxdup[i] <- T
#   }
# }
# 
# compustatglobal <- compustatglobal[which(compustatglobal$maxdup), ]
# compustatna <- compustatna[which(compustatna$maxdup), ]
# 
# ## drop company-year observations from GLOBAL that are also present in North America
# compustatglobal <- compustatglobal[which(!compustatglobal$uniqueid %in% compustatna$uniqueid ), ]
# 
# ## bind dataframes 
# # get variables that are in both dataframes
# vars <- names(compustatglobal)[names(compustatglobal) %in% names(compustatna)]
# compustatglobal <- compustatglobal[, ..vars]
# compustatna <- compustatna[, ..vars]
# compustat <- rbind(compustatglobal, compustatna)
# compustat$conversion <- NA
# 
# er <- fread("/Users/christianbaehr/Downloads/WS_XRU_D_csv_row.csv")
# names(er) <- sapply(er[2,], function(x) strsplit(as.character(x), ":")[[1]][1])
# er <- er[-(1:8), ]
# er[,1] <- IDateTime(er$Currency)[,1]
# er <- er[which(year(er$Currency) > 1999), ]
# 
# unique(compustat$curcd)
# nomatch <- unique(compustat$curcd[!compustat$curcd %in% colnames(er)])
# sum(compustat$curcd %in% nomatch) # only 480 compustat cases have no exchange rate info
# compustat <- compustat[which(!compustat$curcd %in% nomatch), ]
# 
# for(i in 1:nrow(compustat)) {
#   nm <- compustat$curcd[i]
#   if (nm=="USA") {
#     compustat$conversion[i] <- 1
#   } else {
#     ind <- which.min(abs(compustat$datadate[i] - er$Currency))
#     compustat$conversion[i] <- er[ind, ..nm]
#   }
# }
# 
# compustat$conversion <- as.numeric(unlist(compustat$conversion))
# 
# for(col in c("at", "capx", "che", "dltt", "ebit", "ppent", "xrd")) {
#   
#   compustat[, col] <- compustat[, col, with=F] / compustat$conversion
#   
# }
# 
# sum(duplicated(compustat[, c("gvkey", "fyear")])) #no duplicates!
# 
# # produce dataframe with gvkey-years that are in the dataset multiple times
# # dupl <- compustat_comb |>
# #   group_by(gvkey,fyear) |>
# #   filter(n()>1) |>
# #   arrange(gvkey,fyear)
# # fwrite(dupl, "Misc/compustat_duplicates.csv")
# 
# ### Refinitive esg data 
# esg <- fread("Misc/refinitiveesg.csv")
# 
# ### ESG variables
# company_en <- fread("Misc/company_en.csv")
# 
# ### Sic codes
# sic <- fread("Misc/sic_2_digit_codes.csv") |>
#   mutate(sic = as.integer(sic)) |>
#   filter(!is.na(sic))
# 
# sic <- sic[!duplicated(sic), ]


# Orbis data --------------------------------------------------------------


# Read Orbis data
orbis <- read_xlsx("01_raw/orbis/BBH_orbis_extract.xlsx", sheet = 2, na = "n.a.")

# Transform
# Delete the first row
orbis <- data.frame(orbis[, -1])
# Select variables and rename them
names(orbis) <- c("conm", "isin", "isocode", "bvd_sector", "sic", "sic_primary", "sic_secondary",
          paste0("at_", 2021:2001), paste0("ebit_", 2021:2001))
# Remove rows with missing ISINs
orbis <- orbis[!is.na(orbis$isin), ]

## Remove a random company with duplicate rows
drop <- orbis$isin=="US45321L1008" & orbis$bvd_sector=="Banking, Insurance & Financial Services"
orbis <- orbis[!drop, ]

# Reshape the data from wide to long format
orbis_long <- reshape(orbis, direction = "long",
            varying = c(paste0("at_", 2021:2001), paste0("ebit_", 2021:2001)),
            timevar = "year", times = as.character(2001:2021), v.names = c("assets", "ebit"), idvar = "isin")
orbis_long$year <- as.numeric(orbis_long$year)


# Merge -------------------------------------------------------------------

### Prepare datasets for merging
# Merge compustat with sic codes
# compustat_drop <- compustat |>
#   mutate(sic = as.integer(strtrim(sic, 2))) |> 
#   left_join(sic, by = "sic")

### Merge ESG dataset with company data
# esg <- esg |>
#   # make sure no duplicates
#   select(-c(tick, Cusip,Sedol,OAId)) |>
#   filter(Isin != "") |>
#   distinct() |>
#   # merge
#   left_join(company_en, by = c("OrgID", "FisYear"))


### Merge
# quarterly
ccexposure_qfull <- ccexposure_q |>
  mutate(country_name = countrycode(hqcountrycode, origin = 'iso2c', destination = 'country.name')) |>
  left_join(orbis_long, by = c("year", "isin"))
#left_join(compustat_drop, by = c("year" = "fyear", "gvkey")) |>
#left_join(esg, by = c("isin" = "Isin", "year" = "FisYear"))

# annual
ccexposure_afull <- ccexposure_y |>
  mutate(country_name = countrycode(hqcountrycode, origin = 'iso2c', destination = 'country.name')) |>
  left_join(orbis_long, by = c("year", "isin"))
#left_join(compustat_drop, by = c("year" = "fyear", "gvkey")) |>
#left_join(esg, by = c("isin" = "Isin", "year" = "FisYear"))

### Note that merging the exposure produces duplicates. Will deal with this later. (V, Feb 13, 23)


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
fwrite(x = ccexposure_afull, file = "02_processed/exposure_year.csv")
fwrite(x = ccexposure_qfull, file = "02_processed/exposure_quarterly.csv")

# End