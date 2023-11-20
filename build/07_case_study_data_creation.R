### Firms & Lobbying
### Data transformation: With issue texts only for reports that include CLIMATE issues

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

## there are some cases in the exposure_orbis data with multiple client_uuids for 
## a single isin code. These are ok to include -- we already made sure we arent duplicating
## data. However, to properly merge the lobbyview data with the exposure_orbis data,
## we need to treat the various members of the same isin with the same client_uuid. 
## this will ensure they all get fed into the same aggregation process. I reassign
## new client_uuids that are unique at the isin level -- they are just
## "client_uuid1|client_uuid2|client_uuid3|..."

exposure_orbis_wide <- read.csv("data/02_processed/exposure_orbis_client_quarter_wide_REVISE.csv", stringsAsFactors = F)
relevant <- exposure_orbis_wide[grepl("\\|", exposure_orbis_wide$client_uuid), ] # these are the only client_uuids we need to worry about

getid <- function(x) {
  id <- grep(x, relevant$client_uuid)
  if(length(id)>0) {
    return(id)
  } else {
    return(NA)
  }
}

## identify which members in lobby_client have a client_uuid that falls into one
## of the "new" client_uuids I define. I also identify the index of the corresponding
## "new" client_uuid in exposure_orbis.
alt_ids <- sapply(lobby_client$client_uuid, FUN=function(x) getid(x))
table(length(alt_ids)) # they all match with at most one client_uuid in exposure_orbis

## now create a new variable in lobby_client that is an "alternative" client_uuid
## it is either the "new" client_uuid from exposure_orbis, or if there is no need for 
## a "new" id, it is just the original (same) client_uuid as before
client_uuid_alt <- relevant$client_uuid[alt_ids]
lobby_client$client_uuid_alt <- ifelse(is.na(client_uuid_alt), lobby_client$client_uuid, client_uuid_alt)

#View(lobby_client[1:1000, ])

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
lobbying$gov_entity[lobbying$gov_entity==""] <- NA


## Now filter out non-climate reports
lobbying2 <- lobbying |> 
  filter(grepl("ENV|CAW|ENG|FUE", issue_code))


# collapse.char <- aggregate(lobbying[, c("client_uuid", "client_name", "report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
#                   by=list(lobbying$report_year, lobbying$bvdid),
#                   FUN = function(x) paste(x, collapse = "|"))
# collapse.char <- aggregate(lobbying[, c("report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
#                            by=list(lobbying$report_year, lobbying$client_uuid),
#                            FUN = function(x) paste(x, collapse = "|"))
collapse.char <- aggregate(lobbying[, c("report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
                           by=list(lobbying$report_year, lobbying$client_uuid_alt),
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

# env_issue <- lapply(issue_code_split, FUN = function(x) grepl("ENV", x))
# caw_issue <- lapply(issue_code_split, FUN = function(x) grepl("CAW", x))
# eng_issue <- lapply(issue_code_split, FUN = function(x) grepl("ENG", x))
# fue_issue <- lapply(issue_code_split, FUN = function(x) grepl("FUE", x))
# lobbying_firmyear$CLI_ENV_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, env_issue, q1)
# lobbying_firmyear$CLI_ENV_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, env_issue, q2)
# lobbying_firmyear$CLI_ENV_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, env_issue, q3)
# lobbying_firmyear$CLI_ENV_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, env_issue, q4)
# 
# lobbying_firmyear$CLI_CAW_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, caw_issue, q1)
# lobbying_firmyear$CLI_CAW_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, caw_issue, q2)
# lobbying_firmyear$CLI_CAW_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, caw_issue, q3)
# lobbying_firmyear$CLI_CAW_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, caw_issue, q4)
# 
# lobbying_firmyear$CLI_ENG_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, eng_issue, q1)
# lobbying_firmyear$CLI_ENG_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, eng_issue, q2)
# lobbying_firmyear$CLI_ENG_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, eng_issue, q3)
# lobbying_firmyear$CLI_ENG_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, eng_issue, q4)
# 
# lobbying_firmyear$CLI_FUE_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q1)
# lobbying_firmyear$CLI_FUE_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q2)
# lobbying_firmyear$CLI_FUE_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q3)
# lobbying_firmyear$CLI_FUE_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q4)



gov_entity_split <- lapply(lobbying_firmyear$gov_entity, FUN = function(x) strsplit(x, "\\|")[[1]])

doe <- lapply(gov_entity_split, FUN = function(x) grepl("DEPARTMENT OF ENERGY", x))
epa <- lapply(gov_entity_split, FUN = function(x) grepl("ENVIRONMENTAL PROTECTION AGENCY", x))

lobbying_firmyear$CLI_DOE_annual <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_issue, doe)
lobbying_firmyear$CLI_EPA_annual <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_issue, epa)

lobbying_firmyear$CLI_DOE_q1 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, doe, q1)
lobbying_firmyear$CLI_EPA_q1 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, epa, q1)

lobbying_firmyear$CLI_DOE_q2 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, doe, q2)
lobbying_firmyear$CLI_EPA_q2 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, epa, q2)

lobbying_firmyear$CLI_DOE_q3 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, doe, q3)
lobbying_firmyear$CLI_EPA_q3 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, epa, q3)

lobbying_firmyear$CLI_DOE_q4 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, doe, q4)
lobbying_firmyear$CLI_EPA_q4 <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_issue, epa, q4)

#summary(lobbying_firmyear$CLI_annual)
#summary(lobbying_firmyear$CLI_q1 | lobbying_firmyear$CLI_q2 | lobbying_firmyear$CLI_q3 | lobbying_firmyear$CLI_q4)
## same distro of T/F values as should be

#summary(lobbying_firmyear$CLI_DOE_annual)
#summary(lobbying_firmyear$CLI_DOE_q1 | lobbying_firmyear$CLI_DOE_q2 | lobbying_firmyear$CLI_DOE_q3 | lobbying_firmyear$CLI_DOE_q4)

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

# env_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENV", y))}[[1]])})
# caw_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("CAW", y))}[[1]])})
# eng_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENG", y))}[[1]])})
# fue_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("FUE", y))}[[1]])})
# 
# env_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, env_issue_proportion)
# env_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, env_issue_proportion)
# env_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, env_issue_proportion)
# env_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, env_issue_proportion)
# env_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, env_proportion_q1)
# env_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, env_proportion_q2)
# env_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, env_proportion_q3)
# env_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, env_proportion_q4)
# lobbying_firmyear$CLI_ENV_amount_q1 <- env_amount_q1
# lobbying_firmyear$CLI_ENV_amount_q2 <- env_amount_q2
# lobbying_firmyear$CLI_ENV_amount_q3 <- env_amount_q3
# lobbying_firmyear$CLI_ENV_amount_q4 <- env_amount_q4
# 
# caw_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, caw_issue_proportion)
# caw_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, caw_issue_proportion)
# caw_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, caw_issue_proportion)
# caw_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, caw_issue_proportion)
# caw_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, caw_proportion_q1)
# caw_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, caw_proportion_q2)
# caw_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, caw_proportion_q3)
# caw_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, caw_proportion_q4)
# lobbying_firmyear$CLI_CAW_amount_q1 <- caw_amount_q1
# lobbying_firmyear$CLI_CAW_amount_q2 <- caw_amount_q2
# lobbying_firmyear$CLI_CAW_amount_q3 <- caw_amount_q3
# lobbying_firmyear$CLI_CAW_amount_q4 <- caw_amount_q4
# 
# eng_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, eng_issue_proportion)
# eng_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, eng_issue_proportion)
# eng_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, eng_issue_proportion)
# eng_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, eng_issue_proportion)
# eng_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, eng_proportion_q1)
# eng_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, eng_proportion_q2)
# eng_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, eng_proportion_q3)
# eng_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, eng_proportion_q4)
# lobbying_firmyear$CLI_ENG_amount_q1 <- eng_amount_q1
# lobbying_firmyear$CLI_ENG_amount_q2 <- eng_amount_q2
# lobbying_firmyear$CLI_ENG_amount_q3 <- eng_amount_q3
# lobbying_firmyear$CLI_ENG_amount_q4 <- eng_amount_q4
# 
# fue_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, fue_issue_proportion)
# fue_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, fue_issue_proportion)
# fue_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, fue_issue_proportion)
# fue_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, fue_issue_proportion)
# fue_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q1)
# fue_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q2)
# fue_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q3)
# fue_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q4)
# lobbying_firmyear$CLI_FUE_amount_q1 <- fue_amount_q1
# lobbying_firmyear$CLI_FUE_amount_q2 <- fue_amount_q2
# lobbying_firmyear$CLI_FUE_amount_q3 <- fue_amount_q3
# lobbying_firmyear$CLI_FUE_amount_q4 <- fue_amount_q4


#####

issues <- lapply(lobbying_firmyear$issue_code, FUN = function(x) unique(strsplit(x, ";|\\|")[[1]]))
issues <- unique(unlist(issues))
issues

for(i in issues) {
  
  #fue_issue <- lapply(issue_code_split, FUN = function(x) grepl("FUE", x))
  X_issue <- lapply(issue_code_split, FUN = function(x) grepl(i, x))
  
  nm1 <- sprintf("CLI_%s_", i)
  nm2 <- sprintf("CLI_%s_amount_", i)
  lobbying_firmyear[ , paste0(nm1, "q1")] <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, X_issue, q1)
  lobbying_firmyear[ , paste0(nm1, "q2")] <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, X_issue, q2)
  lobbying_firmyear[ , paste0(nm1, "q3")] <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, X_issue, q3)
  lobbying_firmyear[ , paste0(nm1, "q4")] <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, X_issue, q4)
  # lobbying_firmyear$CLI_FUE_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q1)
  # lobbying_firmyear$CLI_FUE_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q2)
  # lobbying_firmyear$CLI_FUE_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q3)
  # lobbying_firmyear$CLI_FUE_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, fue_issue, q4)
  
  X_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl(i, y))}[[1]])})
  
  X_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, X_issue_proportion)
  X_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, X_issue_proportion)
  X_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, X_issue_proportion)
  X_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, X_issue_proportion)
  X_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, X_proportion_q1)
  X_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, X_proportion_q2)
  X_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, X_proportion_q3)
  X_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, X_proportion_q4)
  lobbying_firmyear[ , paste0(nm2, "q1")] <- X_amount_q1
  lobbying_firmyear[ , paste0(nm2, "q2")] <- X_amount_q2
  lobbying_firmyear[ , paste0(nm2, "q3")] <- X_amount_q3
  lobbying_firmyear[ , paste0(nm2, "q4")] <- X_amount_q4
  # fue_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, fue_issue_proportion)
  # fue_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, fue_issue_proportion)
  # fue_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, fue_issue_proportion)
  # fue_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, fue_issue_proportion)
  # fue_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q1)
  # fue_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q2)
  # fue_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q3)
  # fue_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1)*x2, na.rm=T)}, amount_split, fue_proportion_q4)
  # lobbying_firmyear$CLI_FUE_amount_q1 <- fue_amount_q1
  # lobbying_firmyear$CLI_FUE_amount_q2 <- fue_amount_q2
  # lobbying_firmyear$CLI_FUE_amount_q3 <- fue_amount_q3
  # lobbying_firmyear$CLI_FUE_amount_q4 <- fue_amount_q4
}

#lobbying_firmyear$CLI_ENV_annual <- grepl("ENV", lobbying_firmyear$issue_code)
lobbying_firmyear[ , sprintf("CLI_%s_annual", i)] <- grepl(i, lobbying_firmyear$issue_code)

#env_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENV", y))}[[1]])})
issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl(i, y))}[[1]])})
#env_amount <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, env_issue_proportion)
issue_amount <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, issue_proportion)
#lobbying_firmyear$CLI_ENV_amount_annual <- env_amount
lobbying_firmyear[ , sprintf("CLI_%s_amount_annual", i)] <- issue_amount
print(i)

#####



doe_proportion <- lapply(gov_entity_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("DEPARTMENT OF ENERGY", y))}[[1]])})
epa_proportion <- lapply(gov_entity_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENVIRONMENTAL PROTECTION AGENCY", y))}[[1]])})

## product of q1 * "climate issue" proportion
total_proportion_doe_q1 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q1_proportion, climate_issue_proportion, doe_proportion)
total_proportion_doe_q2 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q2_proportion, climate_issue_proportion, doe_proportion)
total_proportion_doe_q3 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q3_proportion, climate_issue_proportion, doe_proportion)
total_proportion_doe_q4 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q4_proportion, climate_issue_proportion, doe_proportion)

total_proportion_epa_q1 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q1_proportion, climate_issue_proportion, epa_proportion)
total_proportion_epa_q2 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q2_proportion, climate_issue_proportion, epa_proportion)
total_proportion_epa_q3 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q3_proportion, climate_issue_proportion, epa_proportion)
total_proportion_epa_q4 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q4_proportion, climate_issue_proportion, epa_proportion)

## scale report amount by the product of a) proportion of issues in the report that are climate
## and b) proportion of gov entities in the report that are DOE (EPA)
doe_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_doe_q1)
doe_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_doe_q2)
doe_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_doe_q3)
doe_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_doe_q4)

epa_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_epa_q1)
epa_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_epa_q2)
epa_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_epa_q3)
epa_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_epa_q4)

lobbying_firmyear$CLI_DOE_amount_q1 <- doe_amount_q1
lobbying_firmyear$CLI_DOE_amount_q2 <- doe_amount_q2
lobbying_firmyear$CLI_DOE_amount_q3 <- doe_amount_q3
lobbying_firmyear$CLI_DOE_amount_q4 <- doe_amount_q4

lobbying_firmyear$CLI_EPA_amount_q1 <- epa_amount_q1
lobbying_firmyear$CLI_EPA_amount_q2 <- epa_amount_q2
lobbying_firmyear$CLI_EPA_amount_q3 <- epa_amount_q3
lobbying_firmyear$CLI_EPA_amount_q4 <- epa_amount_q4

# amount_split[900]
# climate_issue[900]
# climate_issue_proportion[900]
# epa_proportion[900]
# epa_amount[900]
# doe_amount[900]





#####

total_lobby_annual <- sapply(amount_split, FUN = function(x) sum(as.numeric(x), na.rm=T))
lobbying_firmyear$total_lobby_annual <- total_lobby_annual

total_lobby_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, q1_proportion)
total_lobby_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, q2_proportion)
total_lobby_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, q3_proportion)
total_lobby_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, q4_proportion)

lobbying_firmyear$total_lobby_q1 <- total_lobby_q1
lobbying_firmyear$total_lobby_q2 <- total_lobby_q2
lobbying_firmyear$total_lobby_q3 <- total_lobby_q3
lobbying_firmyear$total_lobby_q4 <- total_lobby_q4

#####

rm(list = setdiff(ls(), c("lobbying_firmyear", "issues")))

#####

timespan <- paste0("q", 1:4)
time_varying <- c("CLI_", "CLI_amount_", 
                  "CLI_DOE_", "CLI_EPA_", 
                  "CLI_DOE_amount_", "CLI_EPA_amount_",  
                  "total_lobby_", sprintf("CLI_%s_", issues), sprintf("CLI_%s_amount_", issues))
moving_list <- lapply(time_varying, function(x) paste0(x, timespan))


# check columns
# Flatten the moving_list if it's a list of vectors
flat_moving_list <- unlist(moving_list)

# Check if each column in flat_moving_list exists in lobbying_firmyear
column_check <- sapply(flat_moving_list, function(col) col %in% names(lobbying_firmyear))

# Print out the results
print(column_check)

# Optionally, print out the columns that are missing
missing_columns <- flat_moving_list[!column_check]
print(missing_columns)


## reshape data from wide to long format

lobbying_firmyear$unique_id <- paste(lobbying_firmyear$client_uuid, lobbying_firmyear$report_year)

lobbying_firmquarter <- reshape(lobbying_firmyear,
                                direction="long",
                                varying=moving_list,
                                times=timespan,
                                #timevar="year",
                                timevar="qtr",
                                idvar="unique_id")

for(i in issues) {
  
  names(lobbying_firmquarter)[names(lobbying_firmquarter)==sprintf("CLI_%s_q1", i)] <- sprintf("CLI_%s_quarter", i)
  names(lobbying_firmquarter)[names(lobbying_firmquarter)==sprintf("CLI_%s_amount_q1", i)] <- sprintf("CLI_%s_amount_quarter", i)
  # names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_ENV_q1"] <- "CLI_ENV_quarter"
  # names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_ENV_amount_q1"] <- "CLI_ENV_amount_quarter"
  
}

names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_q1"] <- "CLI_quarter"
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_amount_q1"] <- "CLI_amount_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_ENV_q1"] <- "CLI_ENV_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_ENV_amount_q1"] <- "CLI_ENV_amount_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_CAW_q1"] <- "CLI_CAW_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_CAW_amount_q1"] <- "CLI_CAW_amount_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_ENG_q1"] <- "CLI_ENG_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_ENG_amount_q1"] <- "CLI_ENG_amount_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_FUE_q1"] <- "CLI_FUE_quarter"
# names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_FUE_amount_q1"] <- "CLI_FUE_amount_quarter"
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_DOE_q1"] <- "CLI_DOE_quarter"
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_EPA_q1"] <- "CLI_EPA_quarter"
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_DOE_amount_q1"] <- "CLI_DOE_amount_quarter"
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="CLI_EPA_amount_q1"] <- "CLI_EPA_amount_quarter"
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="total_lobby_q1"] <- "total_lobby_quarter"

lobbying_firmquarter$qtr <- gsub("q", "", lobbying_firmquarter$qtr)

## drop some lobbying variables that are still annual - dont want to create confusion for instance
## that ENV issue codes for a whole year are actually for a specific quarter. If we want to make these
## quarterly, we can do that later.
# lobbying_firmquarter <- lobbying_firmquarter[,!names(lobbying_firmquarter) %in% c("issue_code", "gov_entity", "issue_text")]

#####

exposure_orbis_long <- read.csv("data/02_processed/exposure_orbis_client_quarter_long_REVISE.csv", stringsAsFactors = F)

#out <- merge(exposure_orbis, lobbying_firmyear, by.x = c("bvdid", "year"), by.y = c("bvdid", "report_year"))
exposure_orbis_lobbyview_long <- merge(exposure_orbis_long, lobbying_firmquarter, by.x = c("client_uuid", "year", "qtr"), by.y = c("client_uuid", "report_year", "qtr"), all.x=T)
#out3 <- merge(exposure_orbis_long, lobbying_firmyear, by.x = c("client_uuid", "year"), by.y = c("client_uuid", "report_year"))

sum(duplicated(exposure_orbis_lobbyview_long[, c("isin", "year", "qtr")]))
sum(duplicated(exposure_orbis_lobbyview_long[, c("gvkey", "year", "qtr")]))
sum(duplicated(exposure_orbis_lobbyview_long[, c("bvdid", "year", "qtr")]))
sum(duplicated(exposure_orbis_lobbyview_long[, c("client_uuid", "year", "qtr")]) & exposure_orbis_lobbyview_long$client_uuid!=(-1)) #good

test <- (duplicated(exposure_orbis_lobbyview_long[, c("client_uuid", "year", "qtr")]) & exposure_orbis_lobbyview_long$client_uuid!=(-1)) | (duplicated(exposure_orbis_lobbyview_long[, c("client_uuid", "year", "qtr")], fromLast=T) & exposure_orbis_lobbyview_long$client_uuid!=(-1))
if(sum(test)>0) {
  exposure_orbis_lobbyview_long <- exposure_orbis_lobbyview_long[which(!test), ]
  stop("Still need to deal with these additional duplicates!")
}
#View(exposure_orbis_lobbyview_long[duplicated(exposure_orbis_lobbyview_long[, c("client_uuid", "year")]) & exposure_orbis_lobbyview_long$client_uuid!=(-1),])

## need to double check about the duplication situation with "client_uuid". Assume
## this means a single company, but maybe not? What exactly is client_uuid versus
## isin, versus gvkey and bvdid?

## if we merge without all.x=T, we only get 18784 matches. This means that we have 
## about 1/3 of our firm-years from exposure_orbis actually getting a companion in LobbyView

## new variable captures whether firms lobbied on NONCLIMATE issues in a year
nonclimate <- !is.na(exposure_orbis_lobbyview_long$n_issue_codes) & !exposure_orbis_lobbyview_long$CLI_quarter
exposure_orbis_lobbyview_long$nonCLI_quarter <- F
exposure_orbis_lobbyview_long$nonCLI_quarter[which(nonclimate)] <- T


exposure_orbis_lobbyview_long$us_dummy <- ifelse(exposure_orbis_lobbyview_long$hqcountrycode=="US", 1, 0)
exposure_orbis_lobbyview_long$industry <- exposure_orbis_lobbyview_long$bvdsector
exposure_orbis_lobbyview_long$industry_year <- paste(exposure_orbis_lobbyview_long$industry, exposure_orbis_lobbyview_long$year)

exposure_orbis_lobbyview_long$ebit <- as.numeric(exposure_orbis_lobbyview_long$P_L_b4tax_usd) # some "n.a."s drop out
exposure_orbis_lobbyview_long$at <- as.numeric(exposure_orbis_lobbyview_long$total_assets_usd) # some "n.a."s drop out
exposure_orbis_lobbyview_long$ebit_at <- exposure_orbis_lobbyview_long$ebit / exposure_orbis_lobbyview_long$at

## few cases with zero denominator
invalid_orbis <- which(exposure_orbis_lobbyview_long$ebit_at %in% c(-Inf, Inf))
exposure_orbis_lobbyview_long$at[invalid_orbis] <- NA
exposure_orbis_lobbyview_long$ebit_at[invalid_orbis] <- NA

#View(exposure_orbis_lobbyview_long[which(exposure_orbis_lobbyview_long$ebit_at<(-1000)), ])
drop_clipper2011 <- exposure_orbis_lobbyview_long$conm=="CLIPPER WINDPOWER HOLDINGS LTD" & exposure_orbis_lobbyview_long$year==2011
exposure_orbis_lobbyview_long <- exposure_orbis_lobbyview_long[which(!drop_clipper2011), ]

exposure_orbis_lobbyview_long$CLI_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_quarter)
exposure_orbis_lobbyview_long$CLI_quarter[is.na(exposure_orbis_lobbyview_long$CLI_quarter)] <- 0

exposure_orbis_lobbyview_long$CLI_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_quarter)
exposure_orbis_lobbyview_long$CLI_quarter[is.na(exposure_orbis_lobbyview_long$CLI_quarter)] <- 0
exposure_orbis_lobbyview_long$CLI_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_amount_quarter)] <- 0

# exposure_orbis_lobbyview_long$CLI_CAW_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_CAW_quarter)
# exposure_orbis_lobbyview_long$CLI_CAW_quarter[is.na(exposure_orbis_lobbyview_long$CLI_CAW_quarter)] <- 0
# exposure_orbis_lobbyview_long$CLI_CAW_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_CAW_amount_quarter)] <- 0
# 
# exposure_orbis_lobbyview_long$CLI_ENG_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_ENG_quarter)
# exposure_orbis_lobbyview_long$CLI_ENG_quarter[is.na(exposure_orbis_lobbyview_long$CLI_ENG_quarter)] <- 0
# exposure_orbis_lobbyview_long$CLI_ENG_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_ENG_amount_quarter)] <- 0
# 
# exposure_orbis_lobbyview_long$CLI_ENV_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_ENV_quarter)
# exposure_orbis_lobbyview_long$CLI_ENV_quarter[is.na(exposure_orbis_lobbyview_long$CLI_ENV_quarter)] <- 0
# exposure_orbis_lobbyview_long$CLI_ENV_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_ENV_amount_quarter)] <- 0
# 
# exposure_orbis_lobbyview_long$CLI_FUE_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_FUE_quarter)
# exposure_orbis_lobbyview_long$CLI_FUE_quarter[is.na(exposure_orbis_lobbyview_long$CLI_FUE_quarter)] <- 0
# exposure_orbis_lobbyview_long$CLI_FUE_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_FUE_amount_quarter)] <- 0

for(i in issues) {
  
  nm1 <- sprintf("CLI_%s_quarter", i)
  nm2 <- sprintf("CLI_%s_amount_quarter", i)
  
  exposure_orbis_lobbyview_long[ , nm1] <- as.numeric(exposure_orbis_lobbyview_long[ , nm1])
  exposure_orbis_lobbyview_long[is.na(exposure_orbis_lobbyview_long[,nm1]) , nm1] <- 0
  exposure_orbis_lobbyview_long[is.na(exposure_orbis_lobbyview_long[,nm2]) , nm2] <- 0
  
  # exposure_orbis_lobbyview_long$CLI_FUE_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_FUE_quarter)
  # exposure_orbis_lobbyview_long$CLI_FUE_quarter[is.na(exposure_orbis_lobbyview_long$CLI_FUE_quarter)] <- 0
  # exposure_orbis_lobbyview_long$CLI_FUE_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_FUE_amount_quarter)] <- 0
  
}

exposure_orbis_lobbyview_long$total_lobby_quarter[is.na(exposure_orbis_lobbyview_long$total_lobby_quarter)] <- 0

exposure_orbis_lobbyview_long$total_lobby_quarter <- exposure_orbis_lobbyview_long$total_lobby_quarter / 1000

exposure_orbis_lobbyview_long$CLI_DOE_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_DOE_quarter)
exposure_orbis_lobbyview_long$CLI_DOE_quarter[is.na(exposure_orbis_lobbyview_long$CLI_DOE_quarter)] <- 0

exposure_orbis_lobbyview_long$CLI_EPA_quarter <- as.numeric(exposure_orbis_lobbyview_long$CLI_EPA_quarter)
exposure_orbis_lobbyview_long$CLI_EPA_quarter[is.na(exposure_orbis_lobbyview_long$CLI_EPA_quarter)] <- 0

exposure_orbis_lobbyview_long$CLI_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_amount_quarter)] <- 0
exposure_orbis_lobbyview_long$CLI_DOE_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_DOE_amount_quarter)] <- 0
exposure_orbis_lobbyview_long$CLI_EPA_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_EPA_amount_quarter)] <- 0


# check
names(exposure_orbis_lobbyview_long)
glimpse(exposure_orbis_lobbyview_long)

## write csv
fwrite(exposure_orbis_lobbyview_long, "data/02_processed/lobbying_df_quarterly_only_CLI_text.csv")

# write rdata
write_rds(exposure_orbis_lobbyview_long, "data/02_processed/lobbying_df_quarterly_only_CLI_text.rds")




# Create excel sheets for automobile sector -------------------------------

# Filter transportation sector
auto <- exposure_orbis_lobbyview_long |>
  filter(industry == "Transport Manufacturing")

# Reduce
auto <- auto |>
  select(gvkey, conm, year, registrant_name, issue_code, issue_text, op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  filter(!is.na(issue_text) & issue_text != "") |>
  unique()
  


# Function to create excel sheet
create_excel <- function(x){
  
  name <- deparse(substitute(x))
  
  # create a workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  
  # write contents
  writeDataTable(wb, 1, x, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
  # ignore the warning message: it is redundant
  # https://github.com/ycphs/openxlsx/issues/342
  
  # save the output
  saveWorkbook(wb, paste0(name, ".xlsx"), overwrite = TRUE)
}

if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/data/03_final/issues_texts/")}

# Create excel sheet
create_excel(auto)





### END