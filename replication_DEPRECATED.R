#set working directory
setwd("~/PU Fall 2022/POL573/project")
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/CC Exposure/")
#packages
library(tidyr)
library(data.table)
library(tidyverse)
library(dplyr)

#######import and merge data

#import quarterly data 
ccexposure_q <- fread("cc_firmquarter_2021Q4_03082021_OSF (1).csv")

#import yearly data 
ccexposure_y <- fread("cc_firmyear_2021Q4_03082021_OSF (1).csv")


#import WSJ news data 
library(readxl)
#wsj <- read_excel("EGLKS_dataupdated.xlsx")
wsj <- read_excel("../Misc/WSJ/EGLKS_dataupdated.xlsx")

#import compustat financial data 
compustat <- fread("../Misc/compustat.csv")

#import refinitive esg data 
esg <- fread("../Misc/refinitiveesg.csv")

#add country codes
#install.packages("countrycode")
library(countrycode)

ccexposure_q <- ccexposure_q %>%
  mutate(country_name = countrycode(ccexposure_q$hqcountrycode, origin = 'iso2c', destination = 'country.name'))

ccexposure_y <- ccexposure_y %>%
  mutate(country_name = countrycode(ccexposure_q$hqcountrycode, origin = 'iso2c', destination = 'country.name'))

#add industry description to SIC codes
sic <- fread("../Misc/sic_codes.csv")

compustat <- compustat |> 
  left_join(sic, by = "sic")

#merge compustat with exposure dataset
ccexposure_qfull <- left_join(ccexposure_q, compustat, by = c("year" = "fyear", "isin"))

###

ccexposure_y <- read.csv("cc_firmyear_2021Q4_03082021_OSF (1).csv", stringsAsFactors = F)
#ccexposure_q <- read.csv("cc_firmquarter_2021Q4_03082021_OSF (1).csv", stringsAsFactors = F)

compustat1 <- read.csv("/Users/christianbaehr/Downloads/ty6epfvcbujj2u5q.csv", stringsAsFactors = F) # better when using the north america specific data
compustat2 <- read.csv("../Misc/compustat_102422.csv", stringsAsFactors = F)
#compustat$sic <- substr(compustat$sic, 1, 2)

compustat1 <- compustat1[, c("gvkey", "fyear")]
compustat2 <- compustat2[, c("gvkey", "fyear")]

compustat <- rbind(compustat1, compustat2)

#sum(duplicated(compustat[, c("fyear", "isin")]))
#compustat <- compustat[!duplicated(compustat[, c("fyear", "isin")]), ]
compustat <- compustat[!duplicated(compustat[, c("fyear", "gvkey")]), ]

length(unique(ccexposure_y$isin))
sum(unique(ccexposure_y$gvkey) %in% compustat$gvkey) # dont have complete compustat data
sum(unique(ccexposure_y$gvkey) %in% compustat$gvkey) /length(unique(ccexposure_y$gvkey)) # this is where we lose 70% of the data

ccexposure_yfull <- merge(ccexposure_y, compustat, by.x = c("year", "isin"), by.y = c("fyear", "isin")) #losing a ton of data

ccexposure_yfull_top10opp <- ccexposure_yfull[(ccexposure_yfull$sic %in% as.character(c(49, 16, 17, 37, 36, 12, 35, 29, 55, 75))), ]
ccexposure_yfull_top10reg <- ccexposure_yfull[(ccexposure_yfull$sic %in% as.character(c(49, 12, 29, 32, 10, 37, 33, 35, 41, 24))), ]
ccexposure_yfull_top10phy <- ccexposure_yfull[(ccexposure_yfull$sic %in% as.character(c(26, 24, 14, 49, 12, 64, 15, 10, 22, 35))), ]

annual_mean_opp <- aggregate(ccexposure_yfull_top10opp[, c("op_expo_ew")],by=list(ccexposure_yfull_top10opp$year),FUN=function(x) mean(x)*1000)
annual_mean_opp <- annual_mean_opp[annual_mean_opp$Group.1<2021, ]

annual_mean_reg <- aggregate(ccexposure_yfull_top10reg[, c("rg_expo_ew")],by=list(ccexposure_yfull_top10reg$year),FUN=function(x) mean(x)*1000)
annual_mean_reg <- annual_mean_reg[annual_mean_reg$Group.1<2021, ]

annual_mean_phy <- aggregate(ccexposure_yfull_top10phy[, c("ph_expo_ew")],by=list(ccexposure_yfull_top10phy$year),FUN=function(x) mean(x)*1000)
annual_mean_phy <- annual_mean_phy[annual_mean_phy$Group.1<2021, ]

plot(annual_mean_opp$Group.1, annual_mean_opp$x, type = "l", ylab=parse(text="CCexposure^Opp")) # looks pretty similar to Fig 3a
plot(annual_mean_reg$Group.1, annual_mean_reg$x, type = "l", ylab=parse(text="CCexposure^Reg")) # looks pretty similar to Fig 3a
plot(annual_mean_phy$Group.1, annual_mean_phy$x, type = "l", ylab=parse(text="CCexposure^Phy"), ylim=c(0.01, 0.05)) # looks pretty similar to Fig 3a




###

#include additional environmental ESG variables
company_en <- fread("../Misc/company_en.csv")

esg <- left_join(esg, company_en, by = c("OrgID", "FisYear"))

#merge esg data 
ccexposure_qfull <- left_join(ccexposure_qfull, esg, by = c("isin" = "Isin", "year" = "FisYear"))

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


########summary statistics tables 
#install.packages("modelsummary")
library(modelsummary)

#summary statistics for climate change exposure measures
datasummary((`CC Exposure` = ccexp) + (`CC Opportunity` = opexpo) + (`CC Regulatory` = rgexpo) + (`CC Physical` = phexpo) + (`CC Positive Sentiment` = ccpos) + (`CC Negative Sentiment` = ccneg) + (`CC Risk` = ccrisk) ~ Mean + SD + P25 + Median + P75 + N, 
            data = ccexposure_qfull, 
            title = 'Climate Change Exposure Variables - Summary Statistics',
            note = 'This table reports summary statistics of different measures of climate change exposure, using quarterly earnings call data. The sample includes 10,673 unique firms from 34 countries over the period 2002 to 2020.',
            align = 'lcccccc',
            output = 'latex')

#summary statistics for carbon emissions 
datasummary((`Total CO2 Emissions` = En_En_ER_DP023) ~ Mean + SD + P25 + Median + P75 + N,
            data = ccexposure_qfull,
            title = 'Total CO2 and CO2 Equivalents Emissions - Summary Statistics',
            note = 'This table reports summary statistics for annual CO2 emissions in tons using RefinitivESG data.',
            align = 'lcccccc',
            fmt = 0,
            output = 'latex')

#summary statistics for climate change attention 
datasummary((`WSJ CC News Index` = wsj) ~ Mean + SD + P25 + Median + P75 + N,
            data = wsj,
            title = 'Climate Change News Attention - Summary Statistics',
            note = 'This table reports summary statistics for monthly attention on climate change issues in the Wall Stree Journal from 1984 - 2017.',
            align = 'lcccccc',
            output = 'latex')

##########analysis by industry 
exp_ind <- ccexposure_qfull |>
  select(year, quarter, sic, industry, ccexp, opexpo, rgexpo, phexpo)

#top overall exposure 
exp_ind1 <- exp_ind[exp_ind$sic %in% c(49, 16, 17, 37, 36, 12, 29, 41, 55, 33),]

datasummary(industry ~
            (`CC Exposure` = ccexp) * (Mean + SD + Median + N), 
            data = exp_ind1,
            title = 'Climate Change Overall Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = 'latex')

#top opportunity 
exp_ind2 <- exp_ind[exp_ind$sic %in% c(49, 16, 17, 37, 36, 12, 35, 29, 55, 75),]

datasummary(industry ~
              (`CC Opportunity` = opexpo) * (Mean + SD + Median + N), 
            data = exp_ind2,
            title = 'Climate Change Opportunity Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = 'latex')

#top regulatory 
exp_ind3 <- exp_ind[exp_ind$sic %in% c(49, 12, 29, 32, 10, 37, 33, 35, 41, 24),]

datasummary(industry ~
              (`CC Regulatory` = rgexpo) * (Mean + SD + Median + N), 
            data = exp_ind3,
            title = 'Climate Change Regulatory Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = 'latex')

#top physical
exp_ind4 <- exp_ind[exp_ind$sic %in% c(26, 24, 14, 49, 12, 64, 15, 10, 22, 35),]

datasummary(industry ~
              (`CC Physical` = phexpo) * (Mean + SD + Median + N), 
            data = exp_ind4,
            title = 'Climate Change Physical Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = 'latex')

######graphs over time

#overall 
exp_ind1_avg <- exp_ind1[ ,list(mean=mean(ccexp)), by=year]

ggplot(data=exp_ind1_avg, aes(x=year, y=mean)) +
  geom_line()

#opportunity 
exp_ind2_avg <- exp_ind2[ ,list(mean=mean(opexpo)), by=year]

ggplot(data=exp_ind2_avg, aes(x=year, y=mean)) +
  geom_line()

#regulatory
exp_ind3_avg <- exp_ind3[ ,list(mean=mean(rgexpo)), by=year]

ggplot(data=exp_ind3_avg, aes(x=year, y=mean)) +
  geom_line()

#physical 
exp_ind4_avg <- exp_ind4[ ,list(mean=mean(phexpo)), by=year]

ggplot(data=exp_ind4_avg, aes(x=year, y=mean)) +
  geom_line()
