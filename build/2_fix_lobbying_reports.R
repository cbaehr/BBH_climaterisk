
print("Hello!")
setwd("/scratch/network/cb8007/BBH/BBH1/data")

lobby_report <- read.csv("dataset___report_level.csv", stringsAsFactors=F)

### Fix multiple-quarter report codes 

# shift amount value to numeric
lobby_report$amount_num <- gsub("\\$|\\.|,", "", lobby_report$amount)
lobby_report$amount_num <- as.numeric(lobby_report$amount_num)

# convert quarter codes to list object at once for faster processing
lobby_report$report_quarter_code_list <- sapply(as.character(lobby_report$report_quarter_code), FUN=function(x) strsplit(x, split=""))

for(i in 1:nrow(lobby_report)) {
  if(i==1) {store <- list()}
  
  # pull list of quarters corresponding to firm i year j
  qt <- lobby_report$report_quarter_code_list[i][[1]]
  
  if(length(qt) > 1) { # if more than one quarter for this obs...
    data <- rep(lobby_report[i,], times=length(qt))
    mat <- matrix(data, nrow=length(qt), byrow=T)
    mat <- data.frame(mat)
    names(mat) <- names(lobby_report)
    mat$amount_num <- unlist(mat$amount_num) / length(qt) # break up total value into average per period
    
  } else { # if only one quarter for this obs, do nothing...
    mat <- lobby_report[i,]
    
  }
  mat$report_quarter_code_new <- qt
  store[[i]] <- mat
}

# shift individual firm-year-quarter dataframes back into a single df
lobby_report <- do.call(rbind, store)

# add variable to identify cases that caused the problem
lobby_report$report_type <- ifelse(lobby_report$report_quarter_code=="12", "first half",
                                   ifelse(lobby_report$report_quarter_code=="34", "second half",
                                          ifelse(lobby_report$report_quarter_code=="1234", "full year", "quarter")))

# drop utility variables and rename others
lobby_report <- lobby_report[, !names(lobby_report) %in% c("report_quarter_code_list", "report_quarter_code")]
names(lobby_report)[names(lobby_report)=="report_quarter_code_new"] <- "report_quarter_code"

lobby_report <- apply(lobby_report, 2, as.character)

write.csv(lobby_report, "dataset___report_level_FIXED.csv", row.names = F)
print("Done!")
