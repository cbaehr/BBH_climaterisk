
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")}

options(stringsAsFactors = F)
library(fixest)

dat <- read.csv("lobbying_df_wide.csv")

#View(dat[1:1000, ])

dat <- dat[which(dat$report_quarter_code %in% seq(1,4)), ]

sum(duplicated(dat[, c("gvkey", "year", "report_quarter_code")]))

### Create environmental lobbying dummy

dat$CLI <- dat$ENV==1 | dat$CAW==1 | dat$ENG==1 | dat$FUE==1

###

### Create relative advantage score

industryscore <- aggregate(dat["ccexp"], by = list(dat$industry, dat$year, dat$report_quarter_code), FUN = function(x) mean(x, na.rm=T))
names(industryscore)[names(industryscore)=="ccexp"] <- "ccexpINDUSTRY"

dat <- merge(dat, industryscore, by.x = c("industry", "year", "report_quarter_code"), by.y = c("Group.1", "Group.2", "Group.3"), all.x=T)

dat$RELADV <- dat$ccexp - dat$ccexpINDUSTRY # relative advantage to industry

###

View(dat[, c("ccexp", "ccexpINDUSTRY", "RELADV", "CLI")])
nonmissing <- complete.cases(dat[, c("CLI", "RELADV")])
finite <- apply(dat[, c("CLI", "RELADV")], 1, FUN = function(x) {!any(is.infinite(x))})
dat_nomi <- dat[(nonmissing & finite), ]

mod1 <- feglm(CLI ~ RELADV + ebit + I(ebit/at) + log_co2_l1 + us_dummy + total_lobby | year, family = "binomial", data=dat_nomi)



