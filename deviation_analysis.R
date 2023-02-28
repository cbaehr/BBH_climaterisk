
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")}

options(stringsAsFactors = F)
library(fixest)
library(tidyverse)
library(modelsummary)

dat <- read.csv("lobbying_df_wide.csv")
if(sum(duplicated(dat[, c("gvkey", "year", "report_quarter_code")]))) {warning("DUPLICATE ROWS IN MAIN DATASET")}

### Create environmental lobbying dummy

dat$CLI <- (dat$ENV==1 | dat$CAW==1 | dat$ENG==1 | dat$FUE==1) * 1

###

### Create relative advantage score

industryscore <- aggregate(dat[c("ccexp", "op_expo_ew_q", "rg_expo_ew_q", "ph_expo_ew_q")], by = list(dat$industry, dat$year, dat$report_quarter_code), FUN = function(x) mean(x, na.rm=T))
industryscore <- industryscore[which(industryscore$Group.1!=""), ]
names(industryscore) <- paste0(names(industryscore), "INDUSTRY")

dat <- merge(dat, industryscore, by.x = c("industry", "year", "report_quarter_code"), by.y = c("Group.1INDUSTRY", "Group.2INDUSTRY", "Group.3INDUSTRY"), all.x=T)

dat$RELADV <- dat$ccexp - dat$ccexpINDUSTRY # relative advantage to industry
dat$RELADV <- (dat$RELADV - mean(dat$RELADV, na.rm=T)) / sd(dat$RELADV, na.rm=T)
dat$RELADV_SQ <- dat$RELADV ^ 2

dat$RELADV_op <- dat$op_expo_ew_q - dat$op_expo_ew_qINDUSTRY # relative advantage to industry
dat$RELADV_op <- (dat$RELADV_op - mean(dat$RELADV_op, na.rm=T)) / sd(dat$RELADV_op, na.rm=T)
dat$RELADV_op_SQ <- dat$RELADV_op ^ 2

dat$RELADV_rg <- dat$rg_expo_ew_q - dat$rg_expo_ew_qINDUSTRY # relative advantage to industry
dat$RELADV_rg <- (dat$RELADV_rg - mean(dat$RELADV_rg, na.rm=T)) / sd(dat$RELADV_rg, na.rm=T)
dat$RELADV_rg_SQ <- dat$RELADV_rg ^ 2

dat$RELADV_ph <- dat$ph_expo_ew_q - dat$ph_expo_ew_qINDUSTRY # relative advantage to industry
dat$RELADV_ph <- (dat$RELADV_ph - mean(dat$RELADV_ph, na.rm=T)) / sd(dat$RELADV_ph, na.rm=T)
dat$RELADV_ph_SQ <- dat$RELADV_ph ^ 2

### generate covariates

#US dummy variable 
dat <- dat |>
  mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

#Average lobbying
dat <- dat |>    
  group_by(gvkey, year) |>
  mutate(total_lobby = n_distinct(report_uuid))

#Rename CO2 emissions variable 
dat <- dat |>
  rename(co2_emissions = En_En_ER_DP023)

#Lag emissions variable 
dat <- dat |>
  mutate(log_co2 = log(co2_emissions + 1))

dat <- dat |>
  # group by unit (in our case: firm)
  group_by(gvkey) |>
  # arrange by year
  arrange(year) |>
  # for one year
  mutate(log_co2_l1 = lag(log_co2, 1)) |>
  #ungroup
  ungroup()

dat$ebit <- dat$ebit /100000

View(dat[which(dat$ebit>10000), ])
dat$client_name[which(dat$ebit>10000)]



###

View(dat[, c("ccexp", "ccexpINDUSTRY", "RELADV", "CLI")])
nonmissing <- complete.cases(dat[, c("CLI", "RELADV")])
finite <- apply(dat[, c("CLI", "RELADV")], 1, FUN = function(x) {!any(is.infinite(x))})
dat_nomi <- dat[(nonmissing & finite), ]

#sum(complete.cases(dat_nomi[,c("CLI", "ebit", "at", "log_co2_l1", "us_dummy", "total_lobby", "year", "industry", "RELADV")]))

# models <- list(
#   "Model 1" = feglm(CLI ~ RELADV, family = "binomial", data=dat_nomi),
#   "Model 2" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby, family = "binomial", data=dat_nomi),
#   "Model 3" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nomi),
#   "Model 4" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nomi),
#   "Model 5" = feglm(CLI ~ RELADV + RELADV_SQ, family = "binomial", data=dat_nomi),
#   "Model 6" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby, family = "binomial", data=dat_nomi),
#   "Model 7" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nomi),
#   "Model 8" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nomi))

models <- list(
  "Combined" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nomi),
  "Opportunity" = feglm(CLI ~ RELADV_op + RELADV_op_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nomi),
  "Physical" = feglm(CLI ~ RELADV_ph + RELADV_ph_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nomi),
  "Regulatory" = feglm(CLI ~ RELADV_rg + RELADV_rg_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nomi))

# names
cm <- c("RELADV" = "Within-Industry Attn. Diff.",
        "RELADV_op" = "Within-Industry Attn. Diff.",
        "RELADV_ph" = "Within-Industry Attn. Diff.",
        "RELADV_rg" = "Within-Industry Attn. Diff.",
        "RELADV_SQ" = "WIAD Squared",
        "RELADV_op_SQ" = "WIAD Squared",
        "RELADV_ph_SQ" = "WIAD Squared",
        "RELADV_rg_SQ" = "WIAD Squared",
        "cc_expo_ew_y" = "Overall Attention", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying",
        "cc_expo_ew_q" = "Overall Attention")

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Within-Industry Differences in Climate Attention on Climate Lobbying',
  coef_map = cm,
  gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  vcov= ~ gvkey,
  output = "../results/withinIND_logit.tex")









###

dat_nosam <- dat_nomi[which(dat_nomi$client_name!="Samsung Electronics America, Inc."), ]

models <- list(
  "Model 1" = feglm(CLI ~ RELADV, family = "binomial", data=dat_nosam),
  "Model 2" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby, family = "binomial", data=dat_nosam),
  "Model 3" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nosam),
  "Model 4" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nosam),
  "Model 5" = feglm(CLI ~ RELADV + RELADV_SQ, family = "binomial", data=dat_nosam),
  "Model 6" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby, family = "binomial", data=dat_nosam),
  "Model 7" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nosam),
  "Model 8" = feglm(CLI ~ RELADV + RELADV_SQ + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year + industry, family = "binomial", data=dat_nosam))



# names
cm <- c("RELADV" = "Within-Industry Advantage",
        "RELADV_SQ" = "WIA Squared",
        "cc_expo_ew_y" = "Overall Attention", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying",
        "cc_expo_ew_q" = "Overall Attention")

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Within-Industry Advantage on Climate Lobbying',
  coef_map = cm,
  # ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  vcov= ~ year + industry,
  output = "../results/withinIND_logit_nosam.tex")





