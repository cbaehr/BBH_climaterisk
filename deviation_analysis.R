
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")}

options(stringsAsFactors = F)
library(fixest)
library(tidyverse)
library(modelsummary)

dat <- read.csv("lobbying_df_wide.csv")

#View(dat[1:1000, ])

dat <- dat[which(dat$report_quarter_code %in% seq(1,4)), ]

sum(duplicated(dat[, c("gvkey", "year", "report_quarter_code")]))

### Create environmental lobbying dummy

dat$CLI <- (dat$ENV==1 | dat$CAW==1 | dat$ENG==1 | dat$FUE==1) * 1

###

### Create relative advantage score

industryscore <- aggregate(dat["ccexp"], by = list(dat$industry, dat$year, dat$report_quarter_code), FUN = function(x) mean(x, na.rm=T))
names(industryscore)[names(industryscore)=="ccexp"] <- "ccexpINDUSTRY"

dat <- merge(dat, industryscore, by.x = c("industry", "year", "report_quarter_code"), by.y = c("Group.1", "Group.2", "Group.3"), all.x=T)

dat$RELADV <- dat$ccexp - dat$ccexpINDUSTRY # relative advantage to industry

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





###

View(dat[, c("ccexp", "ccexpINDUSTRY", "RELADV", "CLI")])
nonmissing <- complete.cases(dat[, c("CLI", "RELADV")])
finite <- apply(dat[, c("CLI", "RELADV")], 1, FUN = function(x) {!any(is.infinite(x))})
dat_nomi <- dat[(nonmissing & finite), ]

mod1 <- feglm(CLI ~ RELADV, family = "binomial", data=dat_nomi)
mod1 <- feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby, family = "binomial", data=dat_nomi)
mod1 <- feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nomi)

models <- list(
  "Model 1" = feglm(CLI ~ RELADV, family = "binomial", data=dat_nomi),
  "Model 2" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby, family = "binomial", data=dat_nomi),
  "Model 3" = feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nomi))

# names
cm <- c("cc_expo_ew_y" = "Overall Attention", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying",
        "cc_expo_ew_q" = "Overall Attention",
        "RELADV" = "Within-Industry Advantage")

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = 'Effect of Within-Industry Comparative Advantage on Climate Lobbying',
  coef_map = cm
  # ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE',
  , output = "../results/withinIND_logit.tex")






