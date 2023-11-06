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
#lobby_bills <- fread("data/01_raw/lobbyview/dataset___bills.csv")
#lobby_report <- fread("data/01_raw/lobbyview/dataset___report_level_FIXED.csv")
lobby_report <- fread("data/01_raw/lobbyview/dataset___report_level.csv")

# load firm climate risk data
# create one data-frame with yearly and quarterly exposure data + yearly control variables
# firm_data <- fread("data/df_quarterly_fb.csv") |>
#   mutate(gvkey = as.character(gvkey)) |>
#   filter(!is.na(gvkey)) |>
#   # add _q as identifier for quarterly data
#   rename_at(vars(c(cc_expo_ew:ph_sent_ew,ccpos:phsent)), ~ paste0(., "_q")) |>
#   # merge with yearly firm data
#   left_join(fread("data/df_year_fb.csv") |>
#               mutate(gvkey = as.character(gvkey)) |>
#               filter(!is.na(gvkey)) |>
#               # select only exposure data: control variables come from the quarterly dataframe
#               select(isin:ph_sent_ew,ccpos:phsent) |>
#               # add _y as identifier for yearly data
#               rename_at(vars(-c(isin,year)), ~ paste0(., "_y")),
#             by = c("isin","year"))

## this firm has different client IDs, but the exact same bvdid and isin. So combine into one
# exactscience <- lobby_client[grepl("EXACT SCIENCE", toupper(lobby_client$client_name)), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% exactscience$client_uuid)] <- exactscience$client_uuid[1]
# 
# cvs <- lobby_client[which(lobby_client$client_name %in% c("CVS/Caremark", "CVS Health (and subsidiaries)")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% cvs$client_uuid)] <- cvs$client_uuid[1]
# 
# kaman <- lobby_client[which(lobby_client$client_name %in% c("KAMAN CORPORATION AND SUBSIDIARIES", "Kaman Corporation")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% kaman$client_uuid)] <- kaman$client_uuid[1]
# 
# harman <- lobby_client[which(lobby_client$client_name %in% c("Harman International Industries Incorporated", "Harman International Industries Inc.")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% harman$client_uuid)] <- harman$client_uuid[1]
# 
# curtiss <- lobby_client[which(lobby_client$client_name %in% c("CURTISS WRIGHT", "Curtiss-Wright Corporation")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% curtiss$client_uuid)] <- curtiss$client_uuid[1]
# 
# moodys <- lobby_client[which(lobby_client$client_name %in% c("Moody's Corporation", "Dun and Bradstreet")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% moodys$client_uuid)] <- moodys$client_uuid[1]
# 
# amerada <- lobby_client[which(lobby_client$client_name %in% c("AMERADA HESS CORP", "HESS CORPORATION")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% amerada$client_uuid)] <- amerada$client_uuid[1]
# 
# workday <- lobby_client[which(lobby_client$client_name %in% c("Workday, Ltd.", "Workday, Inc.")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% workday$client_uuid)] <- workday$client_uuid[1]
# 
# 
# aramark <- lobby_client[which(lobby_client$client_name %in% c("Aramark Corporation (through FTI Government Affairs)", "Aramark")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% aramark$client_uuid)] <- aramark$client_uuid[2]
# 
# 
# cr_bard <- lobby_client[which(lobby_client$client_name %in% c("CR BARD INC", "C.R. Bard Inc.")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% cr_bard$client_uuid)] <- cr_bard$client_uuid[1]
# 
# amerisource <- lobby_client[which(lobby_client$client_name %in% c("AmerisourceBergen Corporation", "AMERISOURCEBERGEN")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% amerisource$client_uuid)] <- amerisource$client_uuid[1]
# 
# nexstar <- lobby_client[which(lobby_client$client_name %in% c("Nexstar Media Group, Inc.", "Nexstar Broadcasting Group, Inc.")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% nexstar$client_uuid)] <- nexstar$client_uuid[1]
# 
# arconic <- lobby_client[which(lobby_client$client_name %in% c("Arconic", "ALCOA INC")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% arconic$client_uuid)] <- arconic$client_uuid[1]
# 
# arconic <- lobby_client[which(lobby_client$client_name %in% c("Arconic", "ALCOA INC")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% arconic$client_uuid)] <- arconic$client_uuid[1]
# 
# allegheny <- lobby_client[which(lobby_client$client_name %in% c("Allegheny Technologies Inc.", "Allegheny Technologies Incorporated")), ]
# lobby_report$client_uuid[which(lobby_report$client_uuid %in% allegheny$client_uuid)] <- allegheny$client_uuid[1]






# Load firm level data ----------------------------------------------------

# load firm climate risk data
# create one data-frame with yearly and quarterly exposure data + yearly control variables
# firm_data <- fread("data/02_processed/exposure_quarterly.csv") |>
#   #mutate(gvkey = as.character(gvkey)) |>
#   mutate(isin = as.character(isin), year=as.numeric(year)) |>
#   #filter(!is.na(gvkey)) |>
#   filter(!is.na(isin)) |>
#   # add _q as identifier for quarterly data
#   rename_at(vars(c(cc_expo_ew:ph_sent_ew,ccexp:phsent)), ~ paste0(., "_q"))


#isin.codes <- unique(firm_data$isin)
#write.table(isin.codes, file = "/Users/christianbaehr/Desktop/isin_codes.txt", row.names = F)

# firm_data_year <- fread("data/02_processed/exposure_year.csv") |>
#   #mutate(gvkey = as.character(gvkey)) |>
#   mutate(isin = as.character(isin)) |>
#   #filter(!is.na(gvkey)) |>
#   filter(!is.na(isin)) |>
#   # select only exposure data: control variables come from the quarterly dataframe
#   select(isin:ph_sent_ew,ccexp:phsent) |>
#   rename_at(vars(-c(isin,year)), ~ paste0(., "_y"))



# Check for duplicates
#firm_data <- firm_data[!duplicated(firm_data), ] # none
#firm_data_year <- firm_data_year[!duplicated(firm_data_year), ]

#firm_data <- left_join(firm_data, firm_data_year, by=c("isin","year"))

#View(lobby_report[duplicated(lobby_report$report_uuid) | duplicated(lobby_report$report_uuid, fromLast = T), ])

## once get rid of reports with multiple quarters, no duplicated quarter codes
# lobby_report <- data.frame(lobby_report)
# temp <- lobby_report[!duplicated(lobby_report$report_uuid), names(lobby_report)[names(lobby_report)!="report_quarter_code"]]
# sum(duplicated(temp))

## aggregate to the report_uuid level. Some reports have multiple rows, indicating multiple quarters lobbied in.
## I smush into a single row and separate the quarters by the vertical bar

# collapse.cols <- aggregate(lobby_report$report_quarter_code, by=list(lobby_report$report_uuid), FUN=function(x) paste(x, collapse = "|")) |>
#   setNames(c("report_uuid", "report_quarter_code"))

## remove rows with duplicate report IDs
# lobby_report_nodup <- lobby_report[!duplicated(lobby_report$report_uuid), ]
# lobby_report_nodup <- data.frame(lobby_report_nodup)
# 
# ## merge unique report-level data with the smushed quarter codes
# drop.quarter <- names(lobby_report_nodup)[names(lobby_report_nodup)!="report_quarter_code"]
# lobby_report_nodup <- merge(lobby_report_nodup[,drop.quarter], collapse.cols)

## number of quarters in the report
#lobby_report_nodup$n_quarters <- str_count(lobby_report_nodup$report_quarter_code, "\\|") + 1

## compute the number of quarters captured in a given report (12 = 2 quarters, 1234 = 4 quarters)
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

#lobbying <- merge(lobby_report_nodup, lobby_issuetext_nodup, all.x = T)
lobbying <- merge(lobby_report, lobby_issuetext_nodup, all.x = T)
lobbying <- merge(lobbying, lobby_client, by = "client_uuid", all.x = T)

## a few missing values get converted to NA but otherwise ok
#lobbying$amount_num <- gsub(",|\\$", "", lobbying$amount)
#lobbying$amount_num <- as.numeric(lobbying$amount_num)

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

exposure_orbis_wide <- read.csv("data/02_processed/exposure_orbis_lobbyclient_wide_REVISE.csv", stringsAsFactors = F)
exposure_orbis_long <- read.csv("data/02_processed/exposure_orbis_lobbyclient_long_REVISE.csv", stringsAsFactors = F)

out <- merge(exposure_orbis, lobbying_firmyear, by.x = c("bvdid", "year"), by.y = c("bvdid", "report_year"))
out2 <- merge(exposure_orbis, lobbying_firmyear, by.x = c("client_uuid", "year"), by.y = c("client_uuid", "report_year"), all.x=T)
out3 <- merge(exposure_orbis, lobbying_firmyear, by.x = c("client_uuid", "year"), by.y = c("client_uuid", "report_year"))

## need to double check about the duplication situation with "client_uuid". Assume
## this means a single company, but maybe not? What exactly is client_uuid versus
## isin, versus gvkey and bvdid?

## if we merge without all.x=T, we only get 18784 matches. This means that we have 
## about 1/3 of our firm-years from exposure_orbis actually getting a companion in LobbyView

cc_wide <- out2[!is.na(out2$cc_expo_ew), ]

# write csv
fwrite(df_wide, file="data/03_final/lobbying_df_wide_REVISE.csv")
fwrite(cc_wide, file="data/03_final/lobbying_df_wide_reduced_REVISE.csv")








#####

## compustat doesnt have isin codes - there is NO WAY to merge w Sautner covars with compustat

#sum(!is.na(lobbying$gvkey)) / nrow(lobbying) # only 23% of lobbying obs have real GVKEYS
#sum(!is.na(lobbying$bvdid)) / nrow(lobbying) # no NA bvdids
#sum(lobbying$bvdid!="") / nrow(lobbying) # ~84% of lobbying obs have real bvdids

firm_data <- read_dta("data/01_raw/exposure/SvLVZ_pseudo.dta") # sautner firm data annual only
## NO DUPLICATES IN SAUTNER FIRM DATA
sum(duplicated(firm_data))

#write.table(unique(lobbying$bvdid), "/Users/christianbaehr/Desktop/lobbyview_bvdid.txt", row.names = F)
#write.table(unique(firm_data$isin), "/Users/christianbaehr/Desktop/sautner_isin.txt", row.names = F)

bvdid_map <- readxl::read_xlsx("data/01_raw/firm_identifier_mapping/ORBIS_bvdid_search.xlsx", sheet = 2)
bvdid_map <- bvdid_map[, names(bvdid_map) %in% c("BvD ID number", "ISIN number (All)")]
#length(unique(lobbying$bvdid))
#sum(unique(lobbying$bvdid) %in% isin_map$`BvD ID number`) ## only 1770 of 35202 bvdids in lobbying

isin_map <- readxl::read_xlsx("data/01_raw/firm_identifier_mapping/ORBIS_isin_search.xlsx", sheet = 2)
isin_map <- isin_map[, names(isin_map) %in% c("BvD ID number", "ISIN number (All)")]

mapping <- rbind(bvdid_map, isin_map) |>
  setNames(c("bvdid", "isin"))
mapping <- mapping[!duplicated(mapping), ]

df_wide <- merge(firm_data, mapping, all.x = T) # keeping ALL Sautner data - even if no isin that matches the bridge!

## keepping all Sautner covar observations - even if they dont appear in LobbyView
## we will assume that they didnt lobby AT ALL
## consider using the "us" variable to drop non-American companies from the analysis
df_wide <- merge(df_wide, lobbying_firmyear, by.x=c("bvdid", "year"), by.y = c("bvdid", "report_year"), all.x = T)

## drop cases of actual report uuid duplicates. This is when we we able to match bvdid and isin, and a single 
## isin corresponded to multiple bvdid (or vice versa). Either way, include these would result in some sort of
## double counting. In some cases it is US company and their foreign subsidiary. ONLY ~36 cases
duplicate_reportuuid <- (duplicated(df_wide$report_uuid) | duplicated(df_wide$report_uuid, fromLast=T)) & !is.na(df_wide$report_uuid)
df_wide <- df_wide[!duplicate_reportuuid, ]

sum(duplicated(df_wide$report_uuid) & !is.na(df_wide$report_uuid)) # none!

View(df_wide[duplicated(df_wide[, c("isin", "year")])|duplicated(df_wide[, c("isin", "year")], fromLast = T),])
## a few cases (~50) where multiple isin codes to a single bvdid, but these seem ok to leave in because none of these
## cases actually involve any lobbying. So were not really doing any double counting for these.



View(df_wide[( (duplicated(df_wide[, c("bvdid", "year")]) | duplicated(df_wide[, c("bvdid", "year")], fromLast=T) ) & !is.na(df_wide$bvdid) ), ])
## only 52 cases left where bvdid duplicated and there is ACTUALLY a bvdid associated. Drop these, because they mean firm data is
## being double counted
duplicate_bvdid <- (duplicated(df_wide[, c("bvdid", "year")]) | duplicated(df_wide[, c("bvdid", "year")], fromLast=T) ) & !is.na(df_wide$bvdid)
df_wide <- df_wide[!duplicate_bvdid, ]


## dont worry about the below code -- this is for when we were still allowing client_uuid to vary for the same bvdid. Not doing anymore
# n_firms_in_report <- aggregate(df_wide$isin, by=list(df_wide$bvdid, df_wide$year),
#                                FUN = function(x) length(unique(x)))
# names(n_firms_in_report) <- c("bvdid", "year", "n_firms_in_report")
# 
# df_wide <- merge(df_wide, n_firms_in_report)
# 
# ## for cases where same lobbying report is attributed to two firms in our data,
# ## assume they each contributed an equal amount and divide the amount of lobbying
# ## dollars into equal shares
# df_wide$amount_num <- df_wide$amount_num / df_wide$n_firms_in_report

## doesnt look like there are actually any missing cc_expo_ew
cc <- df_wide |>
  filter(!is.na(cc_expo_ew))

# write csv
fwrite(df_wide, file="data/03_final/lobbying_df_REVISE.csv")
fwrite(cc, file="data/03_final/lobbying_df_reduced_REVISE.csv")

## lobbying information will be double-counted for any firms that have a single bvdid
## but multiple isin codes corresponding to that single bvdid. My solution is just to 
## distribute the lobbying amounts evenly between the two components of the overall firm

df_wide$CLI <- grepl("ENV|CAW|ENG|FUE", df_wide$issue_code) * 1
df_wide$CLI[is.na(df_wide$issue_code)] <- NA
unique(df_wide$issue_code[which(df_wide$CLI==1)])
sum(is.na(df_wide$CLI))

## number of environmental issues
df_wide$n_envir_issues <- sapply(strsplit(df_wide$issue_code, "\\|"), FUN = function(x) sum( str_count("ENV|CAW|ENG|FUE", x) ))

## total amount divided by proportion of environmental issues in report
df_wide$CLI_dollars <- df_wide$amount_num * ( df_wide$n_envir_issues / df_wide$n_issue_codes )

df_wide$us_dummy <- ifelse(df_wide$hqcountrycode=="US", 1, 0)

total_lobby <- aggregate(df_wide$amount_num, 
                         by=list(df_wide$year, df_wide$bvdid), 
                         FUN=function(x ) sum(x, na.rm = T))
names(total_lobby) <- c("year", "bvdid", "total_lobby")

df_wide <- merge(df_wide, total_lobby)


df_wide$industry <- df_wide$sic2
#df_wide <- df_wide[which(df_wide$industry!=""), ]
df_wide$industry_year <- paste(df_wide$industry, df_wide$year)

df_wide$ebit_at <- df_wide$W_ebit_assets

cc_wide <- df_wide[!is.na(df_wide$cc_expo_ew), ]

# write csv
fwrite(df_wide, file="data/03_final/lobbying_df_wide_REVISE.csv")
fwrite(cc_wide, file="data/03_final/lobbying_df_wide_reduced_REVISE.csv")

#cc_wide 225025 by 422
#df_wide 1464710 by 422


### End

################################################################################

## deprecated code I tried using to increaes bvdid-isin matches. unsuccessfully

# sum(lobbying$bvdid %in% bvdid_map$`BvD ID number`) # 1,000,133 bvdids match out of 1,267,469 total in lobbyview
# 
# sum(firm_data$isin %in% bvdid_map$`ISIN number (All)`)
# firm_data$isin_match <- firm_data$isin %in% bvdid_map$`ISIN number (All)`
# firm_data_match <- firm_data[which(firm_data$isin_match), ]
# firm_data_nomatch <- firm_data[which(!firm_data$isin_match), ]
# 
# nonmatch_firmdata <- firm_data_nomatch$isin[which(firm_data_nomatch$hqcountrycode=="US")]
# nonmatch_orbis <- bvdid_map$`ISIN number (All)`[which(!bvdid_map$`ISIN number (All)` %in% firm_data$isin)]
# 
# nonmatch_firmdata <- sort(unique(nonmatch_firmdata))
# nonmatch_orbis <- unique(nonmatch_orbis)
# View(data.frame(nonmatch_firmdata))
# View(data.frame(unique(nonmatch_orbis)))
# 
# nonmatch_firmdata[180]
# temp <- afind(nonmatch_orbis, nonmatch_firmdata[180])
# 
# close.match <- function(x) {
#   y <- afind(nonmatch_orbis, x)
#   return(sum(as.numeric(y$distance)<3, na.rm=T))
# }
# 
# temp <- sapply(nonmatch_firmdata[101:200], close.match)
# 
# which(temp==1)
# 
# test <- afind(nonmatch_orbis, "US00404A1097")
# test$match[test$distance<3]
# 
# test <- afind(bvdid_map$`Company name Latin alphabet`, "ACADIA HEALTHCARE")
# hist(test$distance)
# 
# z <- scale(test$distance)
# test$match[z<(-8)]
# 
# 
# hist(temp$distance)
# temp$match[temp$distance<=4]
# 
# 
# isin_map <- data.frame(isin_map)
# bvdid_map <- data.frame(bvdid_map)
# joint_map <- rbind(isin_map[, c("BvD.ID.number", "ISIN.number")], newmap[, c("BvD.ID.number", "ISIN.number")])
# 
# sum(duplicated(joint_map))
# joint_map <- joint_map[!duplicated(joint_map), ]
# #joint_map <- joint_map[!is.na(joint_map$ISIN.number), ]
# names(joint_map) <- c("bvdid", "isin")
# 
# length(unique(firm_data$isin)) # 10000 unique firms in firm data by isin
# length(unique(lobbying$bvdid)) # 35000 unique firms in lobbying data by bvdid
# 
# lobbying.temp <- merge(lobbying, joint_map)
# lobbying.temp.2 <- merge(lobbying.temp, firm_data, by.x = c("isin", "report_year"), by.y=c("isin", "year"))
# 
# sum(lobbying$bvdid %in% bvdid_map$BvD.ID.number)
# sum(lobbying$bvdid %in% joint_map$bvdid)
# 
# length(unique(joint_map$bvdid))
# length(unique(joint_map$isin))
# 
# length(unique(lobbying.temp.2$isin))
# length(unique(lobbying.temp.2$bvdid))
# ## problem is that we just have tons of missingness for isin codes which have bvdids 
# 
# sum(firm_data$isin %in% joint_map$isin)
# 
# 
# View(joint_map[duplicated(joint_map$ISIN.number) | duplicated(joint_map$ISIN.number, fromLast = T), ])
# 
# ## lot of duplication in isin number
# ## basically each isin is associated with MANY bvdid numbers
# 
# newmap <- readxl::read_xlsx("/Users/christianbaehr/Downloads/missing_bvdids_orbis.xlsx",
#                             sheet = 2)
# sum(unique(lobbying$bvdid) %in% map$BvD.ID.number) ## only 1770 of 35202 bvdids in lobbying
# 
# map <- rbind(isin_map[, c("BvD.ID.number", "ISIN.number")], newmap[, c("BvD.ID.number", "ISIN.number")])
# 
# length(unique(map$BvD.ID.number))
# 
# sum(firm_data$isin %in% isin_map$ISIN.number)
# sum(firm_data$isin %in% map$ISIN.number)
# 
# lob.nomatch <- lobbying$bvdid[!lobbying$bvdid %in% isin_map$`BvD ID number`]
# lob.nomatch <- unique(lob.nomatch)
# map.nomatch <- isin_map$`BvD ID number`[!isin_map$`BvD ID number` %in% lobbying$bvdid]
# 
# View(data.frame(sort(lob.nomatch)))
# View(data.frame(sort(map.nomatch)))
# 
# testa <- strsplit(lob.nomatch, "|")
# testa <- unique(unlist(testa))
# 
# testb <- strsplit(map.nomatch, "|")
# testb <- unique(unlist(testb))
# 
# temp <- afind(lob.nomatch, "US020405716")
# hist(temp$distance)
# 
# as.character(temp$match)[as.numeric(temp$distance)<=4]


