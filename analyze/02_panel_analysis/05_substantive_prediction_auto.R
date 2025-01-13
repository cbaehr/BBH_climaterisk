
rm(list=ls())
options(scipen = 999)

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

###

# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

# Rename fixed effects variables
df <- df |>
  mutate(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

df$CLI <- as.numeric(df$CLI_quarter)

###

load("data/03_final/climate_logit_qrt_bycomponent_MODELS.RData")

## select the model from column 4 (year and industry FE separate)
#mod <- models[[4]]
mod5 <- models[[5]]

## grab all relevant covariates for Ford 2019 q4
# inp <- df[which(df$conm == "FORD MOTOR CO" & df$year == 2019 & df$qtr == 4) ,
#           c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
#                "total_lobby_quarter", "Year", "Industry")]
gm <- which(df$conm == "GENERAL MOTORS COMPANY" & df$year == 2019 & df$qtr == 4)
inp <- df[gm , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", 
                 "total_lobby_quarter", "Industry x Year")]

#temp <- df[ , c("conm", "year", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter", "Industry x Year")]

#temp <- temp[which(temp$conm == "GENERAL MOTORS COMPANY") , ]

## predict for Ford 2019 q4
test1 <- predict(object = mod5, newdata = inp, type = "response")

## find index of toyota 2019 q4
replace <- which(df$conm == "TOYOTA MOTOR CORPORATION" & df$year == 2019 & df$qtr == 4)
## replace Ford opportunity exposure with Toyota in 2019 q4
inp$op_expo_ew <- df$op_expo_ew[replace]

## predicted probability of exposure for Ford with Toyota's opportunity 
## score (all in 2019q4)
test2 <- predict(object = mod5, newdata = inp, type = "response")


test1
test2

df$op_expo_ew[gm]
df$op_expo_ew[replace]
cdf <- ecdf(df$op_expo_ew[which(df$`Industry x Year` =="Transport Manufacturing 2019")])
hist(df$op_expo_ew[which(df$`Industry x Year` =="Transport Manufacturing 2019")])

sum(df$op_expo_ew[which(df$`Industry x Year` =="Transport Manufacturing 2019")] > df$op_expo_ew[gm], na.rm=T) / length(df$op_expo_ew[which(df$`Industry x Year` =="Transport Manufacturing 2019" & !is.na(df$op_expo_ew))])
cdf(df$op_expo_ew[gm]) # GM 86th percentile of opportunity exposure in Auto in 2019
cdf(df$op_expo_ew[replace]) # Toyota 38th percentile of opportunity exposure in Auto in 2019

## Set covars to median I-Y, 20th vs. 80th percentile expo. ----------------------------------------------------

unique(grep("Transport Manufacturing", df$`Industry x Year`, value=T))

for(i in 2001:2020) {
  if(i==2001) {out <- c(); out_pct <- c()}
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  
  syn_firm <- data.frame("op_expo_ew" = mean(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = "Transport Manufacturing 2019")
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$op_expo_ew <- syn_firm$op_expo_ew + 2*sd(iy$op_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
  #sprintf("Probability of lobbying moves from %s to %s", test1, test2)
  out <- c(out, test2 - test1)
  out_pct <- c(out_pct, (test2 - test1) / test1)
}
summary(out)
summary(out_pct)


for(i in 2001:2020) {
  if(i==2001) {out <- c(); out_pct <- c()}
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  
  syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = mean(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = "Transport Manufacturing 2019")
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$rg_expo_ew <- syn_firm$rg_expo_ew + 2*sd(iy$rg_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
  #sprintf("Probability of lobbying moves from %s to %s", test1, test2)
  out_pct <- c(out_pct, (test2 - test1) / test1)
}
summary(out)
summary(out_pct)

for(i in 2001:2020) {
  if(i==2001) {out <- c(); out_pct <- c()}
  iy <- df[which(df$`Industry x Year` == sprintf("Transport Manufacturing %s", i)) , ]
  
  syn_firm <- data.frame("op_expo_ew" = mean(iy$op_expo_ew, na.rm=T),
                         "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                         "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                         "ebit" = median(iy$ebit, na.rm=T),
                         "ebit_at" = median(iy$ebit, na.rm=T),
                         "us_dummy" = 1,
                         "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                         "Industry x Year" = "Transport Manufacturing 2019")
  names(syn_firm)[8] <- "Industry x Year"
  
  test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
  syn_firm$ph_expo_ew <- syn_firm$ph_expo_ew + 2*sd(iy$ph_expo_ew, na.rm=T)
  test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
  #sprintf("Probability of lobbying moves from %s to %s", test1, test2)
  out_pct <- c(out_pct, (test2 - test1) / test1)
}
summary(out)
summary(out_pct)

summary(iy$op_expo_ew)
summary(iy$rg_expo_ew)
summary(iy$ph_expo_ew)

syn_firm <- data.frame("op_expo_ew" = mean(iy$op_expo_ew, na.rm=T),
                       "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                       "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                       "ebit" = median(iy$ebit, na.rm=T),
                       "ebit_at" = median(iy$ebit, na.rm=T),
                       "us_dummy" = 1,
                       "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                       "Industry x Year" = "Transport Manufacturing 2019")
names(syn_firm)[8] <- "Industry x Year"

test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
syn_firm$op_expo_ew <- syn_firm$op_expo_ew + 2*sd(iy$op_expo_ew, na.rm=T)
test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
sprintf("Probability of lobbying moves from %s to %s", test1, test2)

syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                       "rg_expo_ew" = mean(iy$rg_expo_ew, na.rm=T),
                       "ph_expo_ew" = median(iy$ph_expo_ew, na.rm=T),
                       "ebit" = median(iy$ebit, na.rm=T),
                       "ebit_at" = median(iy$ebit, na.rm=T),
                       "us_dummy" = 1,
                       "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                       "Industry x Year" = "Transport Manufacturing 2019")
names(syn_firm)[8] <- "Industry x Year"
test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
syn_firm$rg_expo_ew <- syn_firm$rg_expo_ew + 2*sd(iy$rg_expo_ew, na.rm=T)
test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
sprintf("Probability of lobbying moves from %s to %s", test1, test2)


syn_firm <- data.frame("op_expo_ew" = median(iy$op_expo_ew, na.rm=T),
                       "rg_expo_ew" = median(iy$rg_expo_ew, na.rm=T),
                       "ph_expo_ew" = mean(iy$ph_expo_ew, na.rm=T),
                       "ebit" = median(iy$ebit, na.rm=T),
                       "ebit_at" = median(iy$ebit, na.rm=T),
                       "us_dummy" = 1,
                       "total_lobby_quarter" = median(iy$total_lobby_quarter, na.rm=T),
                       "Industry x Year" = "Transport Manufacturing 2019")
names(syn_firm)[8] <- "Industry x Year"
test1 <- predict(object = mod5, newdata = syn_firm, type = "response")
syn_firm$ph_expo_ew <- syn_firm$ph_expo_ew + 2*sd(iy$ph_expo_ew, na.rm=T)
test2 <- predict(object = mod5, newdata = syn_firm, type = "response")
sprintf("Probability of lobbying moves from %s to %s", test1, test2)



###

tobs <- read.csv("data/03_final/tobit_model_coefs.csv", stringsAsFactors = F)
tobs <- tobs[-c(1:2) , ]
sigmas <- tobs$X..5.[grep("=sig", tobs$X.)]
tobs <- tobs[which(tobs$X. != "=") , ]
tobs <- tobs[which(!tobs$X. %in% c("=Observations", "=t statistics in parentheses", "=* p<0.05, ** p<0.01, *** p<0.001")) , ]
tobs <- tobs[!grepl("=sig", tobs$X.) , ]
## coefs come from model 4
coefs4 <- as.numeric(gsub("=|\\*", "", tobs$X..4.))
coefs5 <- as.numeric(gsub("=|\\*", "", tobs$X..5.))
names(coefs4) <- names(coefs5) <- tobs$X.

sigmas <- as.numeric(gsub("=", "", sigmas))

###

# Function to predict outcome of Tobit model
predict_tobit <- function(x_values, tobit_betas, sigma) {
  XB <- x_values %*% tobit_betas
  # Compute the cumulative distribution function and the probability density function
  Phi <- pnorm(XB / sigma)
  phi <- dnorm(XB / sigma)
  # Compute the inverse Mills ratio
  lambda <- phi / Phi
  # Compute the expected value of Y
  expected_Y <- Phi * (XB + sigma * lambda)
  return(expected_Y)
}


###

## model 4 prediction
coef_temp <- coefs4[1:55]
temp <- df[which(df$conm=="GENERAL MOTORS COMPANY") , ]
inddums <- ifelse(1:28 == 22, 1, 0)
for(i in 1:nrow(temp)) {
  if(i==1) {y <- c(); y_alt <- c()}
  #yrdums <- ifelse(2001:2020 == 2019, 1, 0)
  yrdums <- ifelse(2001:2020 == temp$year[i], 1, 0)
  xvals <- c(as.numeric(temp[i , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter")]), yrdums, inddums)
  names(xvals) <- names(coef_temp)
  
  if(!any(is.na(xvals))) {
    y[i] <- predict_tobit(xvals, coef_temp, sigmas[4])
    op <- df$op_expo_ew[which(df$conm=="TOYOTA MOTOR CORPORATION" & df$yearqtr==temp$yearqtr[i])]
    xvals["=Opportunity Exposure"] <- op
    y_alt[i] <- predict_tobit(xvals, coef_temp, sigmas[4])
  } else {
    y[i] <- y_alt[i] <- NA
  }
}

temp$m4_y <- exp(y)
temp$m4_yalt <- exp(y_alt)

## model 4 prediction
coef_temp <- coefs4[1:55]
temp2 <- df[which(df$conm=="FORD MOTOR CO") , ]
inddums <- ifelse(1:28 == 22, 1, 0)
for(i in 1:nrow(temp)) {
  if(i==1) {yf <- c(); yf_alt <- c()}
  #yrdums <- ifelse(2001:2020 == 2019, 1, 0)
  yrdums <- ifelse(2001:2020 == temp$year[i], 1, 0)
  xvals <- c(as.numeric(temp2[i , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter")]), yrdums, inddums)
  names(xvals) <- names(coef_temp)
  
  if(!any(is.na(xvals))) {
    yf[i] <- predict_tobit(xvals, coef_temp, sigmas[4])
    op <- df$op_expo_ew[which(df$conm=="TOYOTA MOTOR CORPORATION" & df$yearqtr==temp2$yearqtr[i])]
    xvals["=Opportunity Exposure"] <- op
    yf_alt[i] <- predict_tobit(xvals, coef_temp, sigmas[4])
  } else {
    yf[i] <- yf_alt[i] <- NA
  }
}

temp$m4_yf <- exp(yf)
temp$m4_yfalt <- exp(yf_alt)




###

tobs <- read.csv("/Users/christianbaehr/Downloads/tobit_prediction_data.csv", stringsAsFactors = F)

## model 5 prediction
coef_temp <- coefs5
temp2 <- tobs[which(tobs$conm=="GENERAL MOTORS COMPANY") , ]
inddums <- ifelse(1:28 == 22, 1, 0)
ind_yr <- grep("industry_year", names(coef_temp), value=T)
ind_yr <- as.numeric(gsub("=group\\(industry_year\\)=", "", ind_yr))
for(i in 1:nrow(temp2)) {
  if(i==1) {y <- c(); y_alt <- c()}
  #yrdums <- ifelse(2001:2020 == 2019, 1, 0)
  yrdums <- ifelse(2001:2020 == temp2$year[i], 1, 0)
  
  indyr_dums <- ifelse(ind_yr == temp2$industry_year_n[i], 1, 0)
  
  xvals <- c(as.numeric(temp2[i , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter")]), yrdums, inddums, indyr_dums)
  names(xvals) <- names(coef_temp)
  
  if(!any(is.na(xvals))) {
    y[i] <- predict_tobit(xvals, coef_temp, sigmas[5])
    op <- df$op_expo_ew[which(df$conm=="TOYOTA MOTOR CORPORATION" & df$yearqtr==temp$yearqtr[i])]
    xvals["=Opportunity Exposure"] <- op
    y_alt[i] <- predict_tobit(xvals, coef_temp, sigmas[5])
  } else {
    y[i] <- y_alt[i] <- NA
  }
}

temp$m5_y <- exp(y)
temp$m5_yalt <- exp(y_alt)




View(temp[ , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter",
               "conm", "yearqtr", "m4_y", "m4_yalt", "m4_yf", "m4_yfalt", "m5_y", "m5_yalt")])




###

## this is already subsetted to q4 2019 obs for Transport Manufacturing firms
#pred <- read.csv("data/03_final/tobit_model_prediction_data.csv", stringsAsFactors = F)


#which(pred$conm == "FORD MOTOR CO")
#which(pred$conm == "TOYOTA MOTOR CORPORATION")

## industry_year_n == 459

## grab all relevant covariates for Ford 2019 q4
# inp <- pred[which(pred$conm == "FORD MOTOR CO") ,
#           c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
#             "total_lobby_quarter")]
# inp <- pred[which(pred$conm == "GENERAL MOTORS COMPANY") ,
#             c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
#               "total_lobby_quarter")]
#pred$industry_n[which(pred$conm=="FORD MOTOR CO")]

yrdums <- ifelse(2001:2020 == 2019, 1, 0)
inddums <- ifelse(1:28 == 22, 1, 0)

ind_yr <- grep("industry_year", names(coefs), value=T)
ind_yr <- as.numeric(gsub("=group\\(industry_year\\)=", "", ind_yr))
ind_yr_dums <- ifelse(ind_yr == 459, 1, 0)

#xvals <- c(as.numeric(inp), yrdums, inddums)
xvals <- c(as.numeric(temp), yrdums, inddums, ind_yr_dums)
names(xvals) <- names(coefs)
#c(names(coefs), paste0("=year=", 2001:2020), paste0("=group(industry)=", 1:28))

#xvals <- xvals[!is.na(xvals)]
xvals <- xvals[!is.na(coefs)]
coefs <- coefs[!is.na(coefs)]

###



coef_temp <- coefs[1:55]
for(i in 1:nrow(temp)) {
  if(i==1) {expected_y <- c()}
  #yrdums <- ifelse(2001:2020 == 2019, 1, 0)
  yrdums <- ifelse(2001:2020 == temp$year[i], 1, 0)
  xvals <- c(as.numeric(temp[i , c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter")]), yrdums, inddums)
  names(xvals) <- names(coef_temp)
  
  if(!any(is.na(xvals))) {
    expected_y[i] <- predict_tobit(xvals, coef_temp, sigmas[4])
  } else {
    expected_y[i] <- NA
  }
}

temp$log_y_pred <- expected_y
temp$y_pred <- exp(temp$log_y_pred)





expected_Y <- predict_tobit(xvals, coefs, sigmas[4])
#expected_Y <- predict_tobit(xvals, coefs, sigmas[5])
cat("GM PREDICTED SPEND 2019Q4:", exp(as.numeric(expected_Y)))

xvals["=Opportunity Exposure"] <- pred$op_expo_ew[which(pred$conm == "TOYOTA MOTOR CORPORATION")]
expected_Y <- predict_tobit(xvals, coefs, sigmas[4])
#expected_Y <- predict_tobit(xvals, coefs, sigmas[5])
cat("GM PREDICTED SPEND 2019Q4 (TOYOTA OPPO):", exp(as.numeric(expected_Y)))















##################

tobs <- read.csv("data/03_final/tobit_model_coefs.csv", stringsAsFactors = F)
tobs <- tobs[-c(1:2) , ]
sigmas <- tobs$X..5.[grep("=sig", tobs$X.)]
tobs <- tobs[which(tobs$X. != "=") , ]
tobs <- tobs[which(!tobs$X. %in% c("=Observations", "=t statistics in parentheses", "=* p<0.05, ** p<0.01, *** p<0.001")) , ]
tobs <- tobs[!grepl("=sig", tobs$X.) , ]
## coefs come from model 4
coefs <- as.numeric(gsub("=|\\*", "", tobs$X..4.))
#coefs <- as.numeric(gsub("=|\\*", "", tobs$X..5.))
names(coefs) <- tobs$X.

sigmas <- as.numeric(gsub("=", "", sigmas))

###

## this is already subsetted to q4 2019 obs for Transport Manufacturing firms
pred <- read.csv("data/03_final/tobit_model_prediction_data.csv", stringsAsFactors = F)


which(pred$conm == "FORD MOTOR CO")
which(pred$conm == "TOYOTA MOTOR CORPORATION")

## industry_year_n == 459

## grab all relevant covariates for Ford 2019 q4
# inp <- pred[which(pred$conm == "FORD MOTOR CO") ,
#           c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
#             "total_lobby_quarter")]
inp <- pred[which(pred$conm == "GENERAL MOTORS COMPANY") ,
          c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
            "total_lobby_quarter")]
#pred$industry_n[which(pred$conm=="FORD MOTOR CO")]

yrdums <- ifelse(2001:2020 == 2019, 1, 0)
inddums <- ifelse(1:28 == 22, 1, 0)

ind_yr <- grep("industry_year", names(coefs), value=T)
ind_yr <- as.numeric(gsub("=group\\(industry_year\\)=", "", ind_yr))
ind_yr_dums <- ifelse(ind_yr == 459, 1, 0)

#xvals <- c(as.numeric(inp), yrdums, inddums)
xvals <- c(as.numeric(inp), yrdums, inddums, ind_yr_dums)
names(xvals) <- names(coefs)
#c(names(coefs), paste0("=year=", 2001:2020), paste0("=group(industry)=", 1:28))

#xvals <- xvals[!is.na(xvals)]
xvals <- xvals[!is.na(coefs)]
coefs <- coefs[!is.na(coefs)]

###

# Function to predict outcome of Tobit model
predict_tobit <- function(x_values, tobit_betas, sigma) {
  XB <- x_values %*% tobit_betas
  # Compute the cumulative distribution function and the probability density function
  Phi <- pnorm(XB / sigma)
  phi <- dnorm(XB / sigma)
  # Compute the inverse Mills ratio
  lambda <- phi / Phi
  # Compute the expected value of Y
  expected_Y <- Phi * (XB + sigma * lambda)
  return(expected_Y)
}

expected_Y <- predict_tobit(xvals, coefs, sigmas[4])
#expected_Y <- predict_tobit(xvals, coefs, sigmas[5])
cat("GM PREDICTED SPEND 2019Q4:", exp(as.numeric(expected_Y)))

xvals["=Opportunity Exposure"] <- pred$op_expo_ew[which(pred$conm == "TOYOTA MOTOR CORPORATION")]
expected_Y <- predict_tobit(xvals, coefs, sigmas[4])
#expected_Y <- predict_tobit(xvals, coefs, sigmas[5])
cat("GM PREDICTED SPEND 2019Q4 (TOYOTA OPPO):", exp(as.numeric(expected_Y)))








