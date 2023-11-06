### Firms & Lobbying
### Data transformation

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haven)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

# load lobbying data
lobby_client <- fread("data/01_raw/lobbyview/dataset___client_level.csv")
lobby_text <- fread("data/01_raw/lobbyview/dataset___issue_text.csv")
lobby_issue <- fread("data/01_raw/lobbyview/dataset___issue_level.csv")
lobby_report <- fread("data/01_raw/lobbyview/dataset___report_level.csv")

lobby_report$n_quarters <- str_count(as.character(lobby_report$report_quarter_code), "")

#####


## remove nuisance characters
lobby_issue$gov_entity <- gsub('"|\\{|\\}|([\\])|-', ' ', lobby_issue$gov_entity)
lobby_issue$gov_entity <- gsub(',', ';', lobby_issue$gov_entity)
lobby_issue$gov_entity <- gsub("\\s+", " ", lobby_issue$gov_entity) # remove redundant spaces
lobby_issue$gov_entity <- trimws(lobby_issue$gov_entity)

## remove special escape characters
lobby_text$issue_text <- gsub("[^A-z0-9. ]", " ", lobby_text$issue_text)
lobby_text$issue_text <- gsub("`|\\^|\\[|\\]|\\\\|_", " ", lobby_text$issue_text)
lobby_text$issue_text <- gsub("\\s+", " ", lobby_text$issue_text) # remove redundant spaces

lobby_issuetext <- merge(lobby_issue, lobby_text)

## sometimes multiple issue codes or government entities per lobby report
#View(lobby_issue[duplicated(lobby_issue$report_uuid) | duplicated(lobby_issue$report_uuid, fromLast = T),])

lobby_issuetext_nodup <- aggregate(lobby_issuetext[, c("issue_code", "gov_entity", "issue_text")], 
                               by=list(lobby_issuetext$report_uuid), 
                               FUN=function(x) paste(x, collapse = ";")) |>
  setNames(c("report_uuid", "issue_code", "gov_entity", "issue_text"))

#####

lobbying <- merge(lobby_report, lobby_issuetext_nodup, all.x = T)
lobbying <- merge(lobbying, lobby_client, by = "client_uuid", all.x = T)

lobbying$report_quarter_code <- as.character(lobbying$report_quarter_code)

lobbying$amount <- gsub("\\$|,", "", lobbying$amount)

## we match with firm data based on BvDID, so all clients under the same bvdid are assigned a consistent name

## here I want to collapse the lobbying data to the FIRM-year level - which would mean no duplication of client_uuid-year-bvdid (remove client_uuid)

## just treat lobbying amount as zero if missing -> wont affect the amount calculations, because
## missing would just be dropped. But makes the mapply easier
lobbying$amount[which(lobbying$amount=="")] <- 0

# collapse.char <- aggregate(lobbying[, c("client_uuid", "client_name", "report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
#                   by=list(lobbying$report_year, lobbying$bvdid),
#                   FUN = function(x) paste(x, collapse = "|"))

collapse.char <- aggregate(lobbying[, c("report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
                           by=list(lobbying$report_year, lobbying$client_uuid),
                           FUN = function(x) paste(x, collapse = "|"))
lobbying_firmyear <- collapse.char
# names(lobbying_firmyear) <- c("report_year", "bvdid", "client_uuid", "client_name", 
#                               "report_uuid", "issue_code", "gov_entity", "issue_text", 
#                               "registrant_uuid", "registrant_name", "report_quarter_code", "amount_num")
names(lobbying_firmyear) <- c("report_year", "client_uuid",
                              "report_uuid", "issue_code", "gov_entity", "issue_text",
                              "registrant_uuid", "registrant_name", "report_quarter_code", "amount_num")
# collapse.num <- aggregate(lobbying[, c("amount_num")],
#                    by=list(lobbying$report_year, lobbying$bvdid),
#                    FUN = function(x) sum(x, na.rm = T))

#lobbying_firmyear <- merge(collapse.char, collapse.num)


lobbying_firmyear$n_issue_codes <- str_count(lobbying_firmyear$issue_code, "\\|") + 1

###

rm(list = setdiff(ls(), "lobbying_firmyear"))

#####

## any appearance of climate issues in a year
lobbying_firmyear$CLI_annual <- grepl("ENV|CAW|ENG|FUE", lobbying_firmyear$issue_code)

## now look by quarter -> first step is to break up the issue code and report_quarter_code
## for each lobbying report. Second step is to look for coincidences of quarter i and 
## climate-related issue codes in that quarter
issue_code_split <- lapply(lobbying_firmyear$issue_code, FUN = function(x) strsplit(x, "\\|")[[1]])
quarters <- lapply(lobbying_firmyear$report_quarter_code, FUN = function(x) strsplit(x, "\\|")[[1]])

## determine whether climate mentioned in a given report, by firm-year. This produces
## a vector of T/F values for EACH firm-year, one for each lobbying report in that firm-year
climate_issue <- lapply(issue_code_split, FUN = function(x) grepl("ENV|CAW|ENG|FUE", x))
## now produce a vector of T/F for each firm-year indicating if quarter i is mentioned in
## a given report
q1 <- lapply(quarters, FUN = function(x) grepl("1", x))
q2 <- lapply(quarters, FUN = function(x) grepl("2", x))
q3 <- lapply(quarters, FUN = function(x) grepl("3", x))
q4 <- lapply(quarters, FUN = function(x) grepl("4", x))

## now iterate through both the climate dummy list and quarter dummy list. Co-occurences
## of climate TRUE and quarter TRUE implies firm-year lobbied on climate in quarter i
lobbying_firmyear$CLI_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_issue, q1)
lobbying_firmyear$CLI_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_issue, q2)
lobbying_firmyear$CLI_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_issue, q3)
lobbying_firmyear$CLI_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_issue, q4)
## for each row, we compute whether for any lobbying reports A) the report is about a climate issue
## AND B) the report is for qX . If any reports for that firm-year meet this criteria, they get a TRUE; else FALSE.


#summary(lobbying_firmyear$CLI_annual)
#summary(lobbying_firmyear$CLI_q1 | lobbying_firmyear$CLI_q2 | lobbying_firmyear$CLI_q3 | lobbying_firmyear$CLI_q4)
## same distro of T/F values as should be

## now move on to AMOUNT

## for each report in a firm-year, we compute the proportion of issues in that REPORT
## that are climate-related.
climate_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENV|CAW|ENG|FUE", y))}[[1]])})

amount_split <- lapply(lobbying_firmyear$amount, FUN = function(x) strsplit(x, "\\|")[[1]])

climate_amount <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, climate_issue_proportion)
## we divide the total sum by the NUMBER OF ISSUES IN THE REPORT. I thus assume
## lobbying dollars are divided evenly among activities in a report

lobbying_firmyear$CLI_amount_annual <- climate_amount

## we divide the total sum by the NUMBER OF ISSUES IN THE REPORT. I thus assume
## lobbying dollars are divided evenly among activities in a report

## to do for annual was easy - just needed to compute the proportion of issues that
## were climate for each report, then scale it by the dollar amount.

## it is trickier to do for quarterly, because now we need to break down by report
## and determine if each report is in the quarter of interest

## for each REPORT, we compute the proportion that are environmental. Then, 
## if the report is for a single quarter, we only scale the amount for that report
## by the environmental issue proportion. If there are TWO quarters in the report, then 
## we scale the amount by 1/2 times the environmental issue proportion, and 1/4 for FOUR quarters.

## proportion of each report that is q1
q1_proportion <- lapply(quarters, FUN = function(x) {grepl("1", x) * (1/(str_count(x, "|")-1))})
q2_proportion <- lapply(quarters, FUN = function(x) {grepl("2", x) * (1/(str_count(x, "|")-1))})
q3_proportion <- lapply(quarters, FUN = function(x) {grepl("3", x) * (1/(str_count(x, "|")-1))})
q4_proportion <- lapply(quarters, FUN = function(x) {grepl("4", x) * (1/(str_count(x, "|")-1))})

## product of q1 * "climate issue" proportion
total_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, climate_issue_proportion)
total_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, climate_issue_proportion)
total_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, climate_issue_proportion)
total_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, climate_issue_proportion)

## product of report dollar amount and the compound proportion scalar
climate_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, total_proportion_q1)
climate_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, total_proportion_q2)
climate_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, total_proportion_q3)
climate_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, total_proportion_q4)
## we scale the total sum by the NUMBER OF ISSUES IN THE REPORT. I thus assume
## lobbying dollars are divided evenly among activities in a report, and that
## if lobbying occurred across multiple quarters it was uniform across those quarters

lobbying_firmyear$CLI_amount_q1 <- climate_amount_q1
lobbying_firmyear$CLI_amount_q2 <- climate_amount_q2
lobbying_firmyear$CLI_amount_q3 <- climate_amount_q3
lobbying_firmyear$CLI_amount_q4 <- climate_amount_q4

rm(list = setdiff(ls(), "lobbying_firmyear"))

#####

#exposure_orbis_wide <- read.csv("data/02_processed/exposure_orbis_client_wide_REVISE.csv", stringsAsFactors = F)
exposure_orbis_long <- read.csv("data/02_processed/exposure_orbis_client_long_REVISE.csv", stringsAsFactors = F)

#out <- merge(exposure_orbis, lobbying_firmyear, by.x = c("bvdid", "year"), by.y = c("bvdid", "report_year"))
exposure_orbis_lobbyview_long <- merge(exposure_orbis_long, lobbying_firmyear, by.x = c("client_uuid", "year"), by.y = c("client_uuid", "report_year"), all.x=T)
#out3 <- merge(exposure_orbis_long, lobbying_firmyear, by.x = c("client_uuid", "year"), by.y = c("client_uuid", "report_year"))

sum(duplicated(exposure_orbis_lobbyview_long[, c("isin", "year")]))
sum(duplicated(exposure_orbis_lobbyview_long[, c("gvkey", "year")]))
sum(duplicated(exposure_orbis_lobbyview_long[, c("bvdid", "year")]))
sum(duplicated(exposure_orbis_lobbyview_long[, c("client_uuid", "year")]))

## need to double check about the duplication situation with "client_uuid". Assume
## this means a single company, but maybe not? What exactly is client_uuid versus
## isin, versus gvkey and bvdid?

## if we merge without all.x=T, we only get 18784 matches. This means that we have 
## about 1/3 of our firm-years from exposure_orbis actually getting a companion in LobbyView

exposure_orbis_lobbyview_long$us_dummy <- ifelse(exposure_orbis_lobbyview_long$hqcountrycode=="US", 1, 0)
exposure_orbis_lobbyview_long$industry <- exposure_orbis_lobbyview_long$sic_core_3digit
exposure_orbis_lobbyview_long$industry_year <- paste(exposure_orbis_lobbyview_long$industry, exposure_orbis_lobbyview_long$year)

exposure_orbis_lobbyview_long$ebit <- as.numeric(exposure_orbis_lobbyview_long$P_L_b4tax_usd) # some "n.a."s drop out
exposure_orbis_lobbyview_long$at <- as.numeric(exposure_orbis_lobbyview_long$total_assets_usd) # some "n.a."s drop out
exposure_orbis_lobbyview_long$ebit_at <- exposure_orbis_lobbyview_long$ebit / exposure_orbis_lobbyview_long$at

## few cases with zero denominator
exposure_orbis_lobbyview_long$ebit_at[exposure_orbis_lobbyview_long$ebit_at == (-Inf)] <- NA

#View(exposure_orbis_lobbyview_long[which(exposure_orbis_lobbyview_long$ebit_at<(-1000)), ])
drop_clipper2011 <- exposure_orbis_lobbyview_long$conm=="CLIPPER WINDPOWER HOLDINGS LTD" & exposure_orbis_lobbyview_long$year==2011
exposure_orbis_lobbyview_long <- exposure_orbis_lobbyview_long[which(!drop_clipper2011), ]

#hist(exposure_orbis_lobbyview_long$ebit)
#hist(exposure_orbis_lobbyview_long$at)
#hist(exposure_orbis_lobbyview_long$ebit_at)


## write csv
write.csv(exposure_orbis_lobbyview_long, "data/03_final/lobbying_df_REVISE.csv", row.names=F)








