
rm(list=ls())

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

# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure", 
        "cc_expo_ew" = "Overall Exposure",
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_quarter" = "Total Lobbying (\\$)"
)

df$CLI <- as.numeric(df$CLI_quarter)

glimpse(df)

###

load("data/03_final/climate_logit_qrt_bycomponent_MODELS.RData")

## select the model from column 4 (year and industry FE separate)
#mod <- models[[4]]
mod <- models[[5]]

#unique(grep("TOYOTA", df$conm, value=T)) # find the name corresponding to firm i
## find the index corresponding to firm-year-qtr i
which(df$conm == "FORD MOTOR CO" & df$year == 2019 & df$qtr == 4)
which(df$conm == "TOYOTA MOTOR CORPORATION" & df$year == 2019 & df$qtr == 4)

## grab all relevant covariates for Ford 2019 q4
# inp <- df[which(df$conm == "GENERAL MOTORS COMPANY" & df$year == 2019 & df$qtr == 4) ,
#           c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
#                "total_lobby_quarter", "Year", "Industry")]
inp <- df[which(df$conm == "GENERAL MOTORS COMPANY" & df$year == 2019 & df$qtr == 4) ,
          c("op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy",
               "total_lobby_quarter", "Industry x Year")]

temp <- df[ , c("conm", "year", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", "ebit", "ebit_at", "us_dummy", "total_lobby_quarter", "Industry x Year")]

temp <- temp[which(temp$conm == "GENERAL MOTORS COMPANY") , ]

###




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














###

## predict for Ford 2019 q4
test1 <- predict(object = mod, newdata = inp, type = "response")

## find index of toyota 2019 q4
replace <- which(df$conm == "TOYOTA MOTOR CORPORATION" & df$year == 2019 & df$qtr == 4)
## replace Ford opportunity exposure with Toyota in 2019 q4
inp$op_expo_ew <- df$op_expo_ew[replace]

## predicted probability of exposure for Ford with Toyota's opportunity 
## score (all in 2019q4)
test2 <- predict(object = mod, newdata = inp, type = "response")



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








