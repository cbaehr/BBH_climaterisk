#set working directory
setwd("~/PU Fall 2022/POL573/project")
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")
#packages
library(tidyr)
library(data.table)
library(tidyverse)
library(dplyr)

#######import and merge data

#import quarterly data 
ccexposure_q <- fread("CC Exposure/cc_firmquarter_2021Q4_03082021_OSF (1).csv")

#import yearly data 
ccexposure_y <- fread("CC Exposure/cc_firmyear_2021Q4_03082021_OSF (1).csv")
#test <- paste(sort(unique(ccexposure_q$gvkey)), collapse = " \n")
#write_lines(test, file = "/Users/christianbaehr/Desktop/test.txt")

#import WSJ news data 
library(readxl)

#import compustat financial data 
compustat <- fread("Misc/compustat_102422.csv")
compustatna <- fread("Misc/compustat_northamerica.csv")
vars <- names(compustat)[names(compustat) %in% names(compustatna)]
compustat <- compustat[, ..vars]
compustatna <- compustatna[, ..vars]
compustat <- rbind(compustat, compustatna)

compustat <- compustat[order(compustat[, c("gvkey", "fyear")]), ] # ensure if we drop duplicates, we drop the LATER observations from a given year
compustat <- compustat[!duplicated(compustat[, c("gvkey", "fyear")])] # drop duplicate year-firm observations

#import refinitive esg data 
esg <- fread("Misc/refinitiveesg.csv")

#add country codes
#install.packages("countrycode")
library(countrycode)

ccexposure_q <- ccexposure_q %>%
  mutate(country_name = countrycode(ccexposure_q$hqcountrycode, origin = 'iso2c', destination = 'country.name'))

ccexposure_y <- ccexposure_y %>%
  mutate(country_name = countrycode(ccexposure_y$hqcountrycode, origin = 'iso2c', destination = 'country.name'))

#add industry description to SIC codes
sic <- fread("Misc/sic_codes.csv")

names(sic)[names(sic)=="SIC Code"] <- "sic"
compustat <- compustat |> 
  left_join(sic, by = "sic")

#merge compustat with exposure dataset
#ccexposure_qfull <- left_join(ccexposure_q, compustat, by = c("year" = "fyear", "isin"))
ccexposure_qfull <- left_join(ccexposure_q, compustat, by = c("year" = "fyear", "gvkey"))
ccexposure_yfull <- left_join(ccexposure_y, compustat, by = c("year" = "fyear", "gvkey"))

#include additional environmental ESG variables
company_en <- fread("Misc/company_en.csv")

esg <- left_join(esg, company_en, by = c("OrgID", "FisYear"))

#merge esg data 
ccexposure_qfull <- left_join(ccexposure_qfull, esg, by = c("isin" = "Isin", "year" = "FisYear"))

ccexposure_yfull <- left_join(ccexposure_yfull, esg, by = c("isin" = "Isin", "year" = "FisYear"))


wsj <- read_excel("Misc/WSJ/EGLKS_dataupdated.xlsx")
wsj$quarter <- ifelse(wsj$month %in% seq(1,3), 1, 
                      ifelse(wsj$month %in% seq(4,6), 2,
                             ifelse(wsj$month %in% seq(7,9), 3, 4)))
wsj <- aggregate(wsj[, c("wsj")], by=list(wsj$year, wsj$month), FUN=function(x) mean(x, na.rm=T))
wsj$Group.1 <- as.numeric(wsj$Group.1)
wsj$Group.2 <- as.numeric(wsj$Group.2)
ccexposure_qfull <- left_join(ccexposure_qfull, wsj, by=c("year"="Group.1", "quarter"="Group.2"))

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

ccexposure_yfull <- ccexposure_yfull |>
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
            data = ccexposure_qfull, # replication is tighter if we omit 2021 
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
names(ccexposure_qfull)[names(ccexposure_qfull)=="Industry Title"] <- "industry"
exp_ind <- ccexposure_yfull |>
  select(year, sic, ccexp, opexpo, rgexpo, phexpo)
exp_ind$sic <- substr(exp_ind$sic, 1, 2)

exp_ind$Industry <- NA
exp_ind$Industry[exp_ind$sic==49] <- "Electric, Gas, and Sanitary Services"
exp_ind$Industry[exp_ind$sic==16] <- "Heavy Construction, Except Building"
exp_ind$Industry[exp_ind$sic==17] <- "Construction"
exp_ind$Industry[exp_ind$sic==37] <- "Transportation Equipment"
exp_ind$Industry[exp_ind$sic==36] <- "Electronic & Other Electric Equipment"
exp_ind$Industry[exp_ind$sic==12] <- "Coal Mining"
exp_ind$Industry[exp_ind$sic==29] <- "Petroleum Refining"
exp_ind$Industry[exp_ind$sic==41] <- "Local & Suburban Transit"
exp_ind$Industry[exp_ind$sic==55] <- "Automative Dealers & Service Stations"
exp_ind$Industry[exp_ind$sic==33] <- "Primary Metal"
exp_ind$Industry[exp_ind$sic==35] <- "Industrial Machinery & Equipment"
exp_ind$Industry[exp_ind$sic==24] <- "Lumber & Wood"
exp_ind$Industry[exp_ind$sic==75] <- "Auto Repair, Services, & Parking"
exp_ind$Industry[exp_ind$sic==32] <- "Stone, Clay, & Glass Products"
exp_ind$Industry[exp_ind$sic==10] <- "Metal Mining"
exp_ind$Industry[exp_ind$sic==26] <- "Paper & Allied Products"
exp_ind$Industry[exp_ind$sic==14] <- "Nonmetallic Minerals, Except Fuels"
exp_ind$Industry[exp_ind$sic==64] <- "Insurance Agents, Brokers, & Service"
exp_ind$Industry[exp_ind$sic==15] <- "General Building Contractors"
exp_ind$Industry[exp_ind$sic==22] <- "Textile Mill Products"

#top overall exposure 
exp_ind1 <- exp_ind[exp_ind$sic %in% c(49, 16, 17, 37, 36, 12, 29, 41, 55, 33),]

datasummary(Industry ~
            (`CC Exposure` = ccexp) * (Mean + SD + Median + N), 
            data = exp_ind1,
            title = 'Climate Change Overall Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output='../results/Tables/top10stats_overall.tex')

#top opportunity 
exp_ind2 <- exp_ind[exp_ind$sic %in% c(49, 16, 17, 37, 36, 12, 35, 29, 55, 75),]

datasummary(Industry ~
              (`CC Opportunity` = opexpo) * (Mean + SD + Median + N), 
            data = exp_ind2,
            title = 'Climate Change Opportunity Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = '../results/Tables/top10stats_opp.tex')

#top regulatory 
exp_ind3 <- exp_ind[exp_ind$sic %in% c(49, 12, 29, 32, 10, 37, 33, 35, 41, 24),]

datasummary(Industry ~
              (`CC Regulatory` = rgexpo) * (Mean + SD + Median + N), 
            data = exp_ind3,
            title = 'Climate Change Regulatory Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = '../results/Tables/top10stats_reg.tex')

#top physical
exp_ind4 <- exp_ind[exp_ind$sic %in% c(26, 24, 14, 49, 12, 64, 15, 10, 22, 35),]

datasummary(Industry ~
              (`CC Physical` = phexpo) * (Mean + SD + Median + N), 
            data = exp_ind4,
            title = 'Climate Change Physical Exposure for Top 10 SIC Industries',
            align = 'lcccc',
            output = '../results/Tables/top10stats_phy.tex')

######graphs over time

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
  labs(title = " ",
       x = "Year", y = "CCExposure") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14)) 

ggsave("../results/Figures/exposure_timeseries_overall.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

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
  theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14)) 

ggsave("../results/Figures/exposure_timeseries_opp.pdf", width=unit(8, units="in"), height=unit(6, units="in"))


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
  theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14)) 

ggsave("../results/Figures/exposure_timeseries_reg.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

#physical 
exp_ind4_avg <- exp_ind4[ ,list(mean=mean(phexpo)), by=year]

ggplot(data=exp_ind4_avg, aes(x=year, y=mean)) +
  geom_line(size = 1, color = "#00AFBB") + 
  geom_vline(xintercept = c(2005, 2012, 2017), color = "#FC4E07") + 
  annotate(geom = "text",
           label = c("Hurricane Katrina", "Hurricane Sandy", "Hurricane Harvey"),
           x = c(2005, 2012, 2017),
           y = c(.018, .018, .018),
           angle = 90,
           vjust = -1.5,
           size = 3) +
  labs(title = "CC Physical Exposure Top 10 Industries",
       x = "Year", y = "CCExposure - Physical") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14)) 

ggsave("../results/Figures/exposure_timeseries_phy.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

###

ccexposure_qfull <- data.frame(ccexposure_qfull)

ccexposure_qfull$debt_asset <- ccexposure_qfull$dltt/ccexposure_qfull$at
ccexposure_qfull$cash_asset <- ccexposure_qfull$che/ccexposure_qfull$at
ccexposure_qfull$ppe_asset <- ccexposure_qfull$ppent/ccexposure_qfull$at
ccexposure_qfull$ebit_asset <- ccexposure_qfull$ebit/ccexposure_qfull$at
ccexposure_qfull$capex_asset <- ccexposure_qfull$capx/ccexposure_qfull$at
ccexposure_qfull$rnd_asset <- ccexposure_qfull$xrd/ccexposure_qfull$at

winsors <- c("at", "debt_asset", "cash_asset", "ppe_asset", "ebit_asset", "capex_asset", "rnd_asset")

library(DescTools)
ccexposure_qfull[, winsors] <- apply(ccexposure_qfull[, winsors], 2, FUN=function(x) Winsorize(x, minval=quantile(x, 0.01, na.rm=T), maxval=quantile(x, 0.99, na.rm=T)))

ccexposure_qfull$logasset <- log(ccexposure_qfull$at)

meanvars <- c("ccexp", "opexpo", "rgexpo", "phexpo", "En_En_ER_DP023", "logasset",
              "debt_asset", "cash_asset", "ppe_asset", "ebit_asset", "capex_asset",
              "rnd_asset", "wsj")

ccexposure_yfull <- aggregate(ccexposure_qfull[, meanvars],
                              by=list(ccexposure_qfull$year, ccexposure_qfull$isin, ccexposure_qfull$sic, ccexposure_qfull$hqcountrycode),
                              FUN = function(x) mean(x, na.rm=T)) # IGNORING MISSING DATA

ccexposure_yfull <- ccexposure_yfull[order(ccexposure_yfull$Group.2, ccexposure_yfull$Group.1), ]

for(i in meanvars) {
  test <- tapply(ccexposure_yfull[,i], INDEX=list(ccexposure_yfull$Group.2), FUN=function(x) lag(x))
  test <- list(test)
  ccexposure_yfull[, paste0(i, "_l1")] <- unlist(test[[1]])
}

###

ccexposure_yfull$logemissions <- log(ccexposure_yfull$En_En_ER_DP023 + 1)

test <- tapply(ccexposure_yfull$logemissions, INDEX=list(ccexposure_yfull$Group.2), FUN=function(x) lag(x))
ccexposure_yfull$logemissions_l1 <- unlist(test)

ccexposure_yfull$industry_year_fe <- paste(ccexposure_yfull$Group.1, ccexposure_yfull$Group.3)

#controls <- paste0(c("logasset", "debt_asset", "cash_asset", "ppe_asset", "ebit_asset", "capex_asset", "rnd_asset"), "_l1")
controls <- paste0(c("logasset", "debt_asset", "cash_asset", "ppe_asset", "ebit_asset"), "_l1")

form1 <- paste0("ccexp ~ ",
                paste(c("logemissions_l1", controls, "factor(industry_year_fe)", "factor(Group.4)"), collapse=" + "))
form2 <- paste0("opexpo ~ ",
                paste(c("logemissions_l1", controls, "factor(industry_year_fe)", "factor(Group.4)"), collapse=" + "))
form3 <- paste0("rgexpo ~ ",
                paste(c("logemissions_l1", controls, "factor(industry_year_fe)", "factor(Group.4)"), collapse=" + "))
form4 <- paste0("phexpo ~ ",
                paste(c("logemissions_l1", controls, "factor(industry_year_fe)", "factor(Group.4)"), collapse=" + "))

table1 <- list("Main" = lm(form1, data=ccexposure_yfull),
               "Opp" = lm(form2, data=ccexposure_yfull),
               "Reg" = lm(form3, data=ccexposure_yfull),
               "Phy" = lm(form4, data=ccexposure_yfull)
)

modelsummary(table1,
             #output="../results/Tables/table4a_replication.tex", 
             stars=T,
             coef_omit= "Intercept|factor",
             coef_rename = c("logemissions_l1"="ln(1 + Total Emissions)","logasset_l1"= "ln(Total Assets)","debt_asset_l1"= "Debt/Assets",
                             "cash_asset_l1"="Cash/Assets","ppe_asset_l1"= "PP&E/Assets","ebit_asset_l1"= "EBIT/Assets","capex_asset_l1"= "Cap. Expend./Assets",
                             "rnd_asset_l1"="R&D/Assets"),
             gof_omit="Std",
             add_rows=data.frame(matrix(c(
               "Industry x Year FE", "X", "X", "X", "X",
               "Country FE", "X", "X", "X", "X"), ncol=5, byrow=T)),
             vcov= ~ Group.1 + Group.3,
             notes = "All independent variables lagged by one period and (except ln(Total Emissions)) are winsorized at the one percent level. PP&E is Property, plant, and equipment. EBIT is Earnings before interest and taxes.")

form5 <- paste0("ccexp ~ ",
                paste(c("wsj", controls, "factor(Group.3)", "factor(Group.4)"), collapse=" + "))
form6 <- paste0("opexpo ~ ",
                paste(c("wsj", controls, "factor(Group.3)", "factor(Group.4)"), collapse=" + "))
form7 <- paste0("rgexpo ~ ",
                paste(c("wsj", controls, "factor(Group.3)", "factor(Group.4)"), collapse=" + "))
form8 <- paste0("phexpo ~ ",
                paste(c("wsj", controls, "factor(Group.3)", "factor(Group.4)"), collapse=" + "))

table2 <- list("Main" = lm(form5, data=ccexposure_yfull),
               "Opp" = lm(form6, data=ccexposure_yfull),
               "Reg" = lm(form7, data=ccexposure_yfull),
               "Phy" = lm(form8, data=ccexposure_yfull)
)


modelsummary(table2,
             output="../results/table_4b.tex", 
             stars=T,
             coef_omit= "Intercept|factor",
             gof_omit="Std",
             add_rows=data.frame(matrix(c(
               "Industry FE", "X", "X", "X", "X",
               "Country FE", "X", "X", "X", "X"), ncol=5, byrow=T)),
             vcov= ~ Group.1 + Group.3)



