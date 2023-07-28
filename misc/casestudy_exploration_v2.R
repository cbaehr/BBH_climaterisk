

setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")

data <- read.csv("lobbying_df_wide__red_w_directionality.csv")

data <- data[order(data$gvkey, data$year, data$report_quarter_code), ]
data$yearqtr <- as.numeric(paste0(data$year, data$report_quarter_code))

data$climate_lob_dollars <- rowSums(data[, c("amount_num_ENV", "amount_num_ENG",
                                             "amount_num_FUE", "amount_num_CAW",
                                             "amount_num_NAT")], na.rm = T)

data <- data[, c("conm", "gvkey", "year", "report_quarter_code", "yearqtr", "phexpo_q", "climate_lob_dollars")]

data_split <- split(data, data$gvkey)

lagg_back <- function(x, lag) {
  len <- length(x)
  if(len>lag) {
    out <- c(x[(lag+1):len], rep(NA, lag))
  } else {
    out <- rep(NA, len)
  }
  return(out)
}

test <- lapply(data_split, function(x) lagg_back(x[,"yearqtr"], lag=6))
data$yearqtr_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"year"], lag=6))
data$year_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"report_quarter_code"], lag=6))
data$report_quarter_code_lplus6 <- unlist(test)

data$yeardiff <- data$year_lplus6 - data$year
data$qtrdiff <- data$report_quarter_code_lplus6 - data$report_quarter_code

data$diff <- data$yeardiff*4 + data$qtrdiff

#View(data[, c("gvkey", "yearqtr", "yearqtr_lplus3", "diff")])

test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=6))
data$climate_lob_dollars_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=5))
data$climate_lob_dollars_lplus5 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=4))
data$climate_lob_dollars_lplus4 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=3))
data$climate_lob_dollars_lplus3 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=2))
data$climate_lob_dollars_lplus2 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=1))
data$climate_lob_dollars_lplus1 <- unlist(test)

data$lobbied_tplus1_3 <- rowSums(data[, c("climate_lob_dollars_lplus3", "climate_lob_dollars_lplus2", "climate_lob_dollars_lplus1",
                                          "climate_lob_dollars_lplus4", "climate_lob_dollars_lplus5", "climate_lob_dollars_lplus6")],
                                 na.rm = T)

#View(data[which(data$diff>20), c("gvkey", "yearqtr", "yearqtr_lplus3", "diff")])

out <- data[which(data$lobbied_tplus1_3>0 & data$phexpo_q>0),
            c("conm", "gvkey", "yearqtr", "yearqtr_lplus6", "diff", "phexpo_q", "climate_lob_dollars_lplus1", "climate_lob_dollars_lplus2", "climate_lob_dollars_lplus3",
              "climate_lob_dollars_lplus4", "climate_lob_dollars_lplus5", "climate_lob_dollars_lplus6")]

write.csv(data.frame(out), "/Users/christianbaehr/Desktop/physical_risk_candidates.csv", row.names = F)

# lagg <- function(x, time, lag) {
#   len <- length(x)
#   if(len>lag) {
#     y <- c(rep(NA, lag), x[1:(len-lag)])
#   } else{
#     y <- rep(NA, len)
#   }
#   # 2020 value for previous country carries over into 1970 for current country.
#   # replace with NA
#   #y[which(time==min(time))] <- NA # dont need - if data is ordered
#   return(y)
# }

################################################################################

data <- read.csv("lobbying_df_wide__red_w_directionality.csv")

data <- data[order(data$gvkey, data$year, data$report_quarter_code), ]
data$yearqtr <- as.numeric(paste0(data$year, data$report_quarter_code))

data$climate_lob_dollars <- rowSums(data[, c("amount_num_ENV", "amount_num_ENG",
                                             "amount_num_FUE", "amount_num_CAW",
                                             "amount_num_NAT")], na.rm = T)

data <- data[, c("conm", "gvkey", "year", "report_quarter_code", "yearqtr", "opexpo_q", "climate_lob_dollars")]

data_split <- split(data, data$gvkey)

lagg_back <- function(x, lag) {
  len <- length(x)
  if(len>lag) {
    out <- c(x[(lag+1):len], rep(NA, lag))
  } else {
    out <- rep(NA, len)
  }
  return(out)
}

test <- lapply(data_split, function(x) lagg_back(x[,"yearqtr"], lag=6))
data$yearqtr_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"year"], lag=6))
data$year_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"report_quarter_code"], lag=6))
data$report_quarter_code_lplus6 <- unlist(test)

data$yeardiff <- data$year_lplus6 - data$year
data$qtrdiff <- data$report_quarter_code_lplus6 - data$report_quarter_code

data$diff <- data$yeardiff*4 + data$qtrdiff

#View(data[, c("gvkey", "yearqtr", "yearqtr_lplus3", "diff")])

test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=6))
data$climate_lob_dollars_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=5))
data$climate_lob_dollars_lplus5 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=4))
data$climate_lob_dollars_lplus4 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=3))
data$climate_lob_dollars_lplus3 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=2))
data$climate_lob_dollars_lplus2 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=1))
data$climate_lob_dollars_lplus1 <- unlist(test)

data$lobbied_tplus1_3 <- rowSums(data[, c("climate_lob_dollars_lplus3", "climate_lob_dollars_lplus2", "climate_lob_dollars_lplus1",
                                          "climate_lob_dollars_lplus4", "climate_lob_dollars_lplus5", "climate_lob_dollars_lplus6")],
                                 na.rm = T)

#View(data[which(data$diff>20), c("gvkey", "yearqtr", "yearqtr_lplus3", "diff")])

out <- data[which(data$lobbied_tplus1_3>0 & data$opexpo_q>0),
            c("conm", "gvkey", "yearqtr", "yearqtr_lplus6", "diff", "opexpo_q", "climate_lob_dollars_lplus1", "climate_lob_dollars_lplus2", "climate_lob_dollars_lplus3",
              "climate_lob_dollars_lplus4", "climate_lob_dollars_lplus5", "climate_lob_dollars_lplus6")]

write.csv(data.frame(out), "/Users/christianbaehr/Desktop/opportunity_risk_candidates.csv", row.names = F)



################################################################################

data <- read.csv("lobbying_df_wide__red_w_directionality.csv")

data <- data[order(data$gvkey, data$year, data$report_quarter_code), ]
data$yearqtr <- as.numeric(paste0(data$year, data$report_quarter_code))

data$climate_lob_dollars <- rowSums(data[, c("amount_num_ENV", "amount_num_ENG",
                                             "amount_num_FUE", "amount_num_CAW",
                                             "amount_num_NAT")], na.rm = T)

data <- data[, c("conm", "gvkey", "year", "report_quarter_code", "yearqtr", "rgexpo_q", "climate_lob_dollars")]

data_split <- split(data, data$gvkey)

lagg_back <- function(x, lag) {
  len <- length(x)
  if(len>lag) {
    out <- c(x[(lag+1):len], rep(NA, lag))
  } else {
    out <- rep(NA, len)
  }
  return(out)
}

test <- lapply(data_split, function(x) lagg_back(x[,"yearqtr"], lag=6))
data$yearqtr_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"year"], lag=6))
data$year_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"report_quarter_code"], lag=6))
data$report_quarter_code_lplus6 <- unlist(test)

data$yeardiff <- data$year_lplus6 - data$year
data$qtrdiff <- data$report_quarter_code_lplus6 - data$report_quarter_code

data$diff <- data$yeardiff*4 + data$qtrdiff

test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=6))
data$climate_lob_dollars_lplus6 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=5))
data$climate_lob_dollars_lplus5 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=4))
data$climate_lob_dollars_lplus4 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=3))
data$climate_lob_dollars_lplus3 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=2))
data$climate_lob_dollars_lplus2 <- unlist(test)
test <- lapply(data_split, function(x) lagg_back(x[,"climate_lob_dollars"], lag=1))
data$climate_lob_dollars_lplus1 <- unlist(test)

data$lobbied_tplus1_3 <- rowSums(data[, c("climate_lob_dollars_lplus3", "climate_lob_dollars_lplus2", "climate_lob_dollars_lplus1",
                                          "climate_lob_dollars_lplus4", "climate_lob_dollars_lplus5", "climate_lob_dollars_lplus6")],
                                 na.rm = T)

#View(data[which(data$diff>20), c("gvkey", "yearqtr", "yearqtr_lplus3", "diff")])

out <- data[which(data$lobbied_tplus1_3>0 & data$rgexpo_q>0),
            c("conm", "gvkey", "yearqtr", "yearqtr_lplus6", "diff", "rgexpo_q", "climate_lob_dollars_lplus1", "climate_lob_dollars_lplus2", "climate_lob_dollars_lplus3",
              "climate_lob_dollars_lplus4", "climate_lob_dollars_lplus5", "climate_lob_dollars_lplus6")]

write.csv(data.frame(out), "/Users/christianbaehr/Desktop/regulatory_risk_candidates.csv", row.names = F)








