#set working directory
setwd("~/PU Fall 2022/POL573/project")
#setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")

#packages
library(tidyr)
library(data.table)
library(tidyverse)
library(dplyr)


# Import and Merge Data ---------------------------------------------------


#import quarterly data 
ccexposure_q <- fread("cc_firmquarter_2021Q4_03082021_OSF (1).csv")

#import yearly data 
ccexposure_y <- fread("cc_firmyear_2021Q4_03082021_OSF (1).csv")

#import WSJ news data 
library(readxl)
wsj <- read_excel("EGLKS_dataupdated.xlsx")

#import compustat financial data 
compustat <- fread("compustat_102422.csv")

#import refinitive esg data 
esg <- fread("refinitiveesg.csv")

#add country codes
install.packages("countrycode")
library(countrycode)

ccexposure_q <- ccexposure_q %>%
  mutate(country_name = countrycode(ccexposure_q$hqcountrycode, origin = 'iso2c', destination = 'country.name'))

ccexposure_y <- ccexposure_y %>%
  mutate(country_name = countrycode(ccexposure_q$hqcountrycode, origin = 'iso2c', destination = 'country.name'))

#add industry description to SIC codes
sic <- fread("sic_codes.csv")

compustat <- compustat |> 
  left_join(sic, by = "sic")

#merge compustat with exposure dataset
#quarterly
ccexposure_qfull <- left_join(ccexposure_q, compustat, by = c("year" = "fyear", "isin"))
#annual
ccexposure_afull <- left_join(ccexposure_y, compustat, by = c("year" = "fyear", "isin"))

#include additional environmental ESG variables
company_en <- fread("company_en.csv")

esg <- left_join(esg, company_en, by = c("OrgID", "FisYear"))

#merge esg data 
#quarterly
ccexposure_qfull <- left_join(ccexposure_qfull, esg, by = c("isin" = "Isin", "year" = "FisYear"))
#annual
ccexposure_afull <- left_join(ccexposure_afull, esg, by = c("isin" = "Isin", "year" = "FisYear"))

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

# Summary Statistics Tables -----------------------------------------------

#packages
install.packages("modelsummary")
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


# Time Series Graphs ------------------------------------------------------

#overall 
exp_ind1_avg <- exp_ind1[ ,list(mean=mean(ccexp)), by=year]

ggplot(data=exp_ind1_avg, aes(x=year, y=mean)) +
  geom_line(size = 1, color = "#00AFBB") + 
  geom_vline(xintercept = c(2005, 2009, 2012, 2015, 2017), color = "#FC4E07") + 
  annotate(geom = "text",
           label = c("EU Emissions Trading", "Copenhagen Summit", "Doha Summit", "Paris Agreement", "Trump Paris Withdrawal"),
           x = c(2005, 2009, 2012, 2015, 2017),
           y = c(5, 5, 5, 5, 5),
           angle = 90,
           vjust = -1.5,
           size = 3) +
  labs(title = "CC Exposure Top 10 Industries",
       x = "Year", y = "CCExposure") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) 
  

#opportunity 
exp_ind2_avg <- exp_ind2[ ,list(mean=mean(opexpo)), by=year]

ggplot(data=exp_ind2_avg, aes(x=year, y=mean)) +
  geom_line(size = 1, color = "#00AFBB") + 
  geom_vline(xintercept = c(2005, 2009, 2012, 2015, 2017), color = "#FC4E07") + 
  annotate(geom = "text",
           label = c("EU Emissions Trading", "Copenhagen Summit", "Doha Summit", "Paris Agreement", "Trump Paris Withdrawal"),
           x = c(2005, 2009, 2012, 2015, 2017),
           y = c(1.5, 1.75, 1.85, 1.9, 1.25),
           angle = 90,
           vjust = -1.5,
           size = 3) +
  labs(title = "CC Opportunity Exposure Top 10 Industries",
       x = "Year", y = "CCExposure - Opportunity") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) 

#regulatory
exp_ind3_avg <- exp_ind3[ ,list(mean=mean(rgexpo)), by=year]

ggplot(data=exp_ind3_avg, aes(x=year, y=mean)) +
  geom_line(size = 1, color = "#00AFBB") + 
  geom_vline(xintercept = c(2005, 2009, 2012, 2015, 2017), color = "#FC4E07") + 
  annotate(geom = "text",
           label = c("EU Emissions Trading", "Copenhagen Summit", "Doha Summit", "Paris Agreement", "Trump Paris Withdrawal"),
           x = c(2005, 2009, 2012, 2015, 2017),
           y = c(.25, .3, .25, .25, .3),
           angle = 90,
           vjust = -1.5,
           size = 3) +
  labs(title = "CC Regulatory Exposure Top 10 Industries",
       x = "Year", y = "CCExposure - Regulatory") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) 

#physical 
exp_ind4_avg <- exp_ind4[ ,list(mean=mean(phexpo)), by=year]

ggplot(data=exp_ind4_avg, aes(x=year, y=mean)) +
  geom_line(size = 1, color = "#00AFBB") + 
  geom_vline(xintercept = c(2005, 2012, 2017), color = "#FC4E07") + 
  annotate(geom = "text",
           label = c("Hurricane Katrina", "Hurricane Sandy", "Hurricane Harvey"),
           x = c(2005, 2012, 2017),
           y = c(.1, .1, .1),
           angle = 90,
           vjust = -1.5,
           size = 3) +
  labs(title = "CC Physical Exposure Top 10 Industries",
       x = "Year", y = "CCExposure - Physical") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5)) 
