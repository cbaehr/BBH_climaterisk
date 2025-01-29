### Firms & Lobbying
### Data transformation

### Annual

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haven)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

# load lobbying data
lobby_client_old <- fread("data/01_raw/lobbyview/dataset___client_level.csv")
lobby_client <- fread("data/01_raw/lobbyview_20250103/clients_codebook/clients.csv")

lobby_text_old <- fread("data/01_raw/lobbyview/dataset___issue_text.csv")
lobby_text <- fread("data/01_raw/lobbyview_20250103/issue_text_codebook/issue_text.csv")

lobby_issue_old <- fread("data/01_raw/lobbyview/dataset___issue_level.csv")
lobby_issue <- fread("data/01_raw/lobbyview_20250103/issues_codebook/issues.csv")
#
lobby_report_old <- fread("data/01_raw/lobbyview/dataset___report_level.csv")
lobby_report <- fread("data/01_raw/lobbyview_20250103/reports_codebook/reports.csv")

# lobby_report <- fread("/Users/christianbaehr/Dropbox/BBH/BBH1/data/01_raw/lobbyview/dataset___report_level.csv")
# lobby_issue_old <- fread("/Users/christianbaehr/Dropbox/BBH/BBH1/data/01_raw/lobbyview/dataset___issue_level.csv")

lobby_report$report_quarter_code <- ifelse(lobby_report$filing_period_code=="H1", 12,
                                           ifelse(lobby_report$filing_period_code=="H2", 34,
                                                  ifelse(lobby_report$filing_period_code=="Q1", 1,
                                                         ifelse(lobby_report$filing_period_code=="Q2", 2,
                                                                ifelse(lobby_report$filing_period_code=="Q3", 3, 4)))))

lobby_report$n_quarters <- str_count(as.character(lobby_report$report_quarter_code), "")

lobby_issue_map <- fread("issues_codebook/government_entity_mapping.csv")

process_entity <- function(ids) {
  a <- gsub("\\{|\\}", "", ids)
  b <- strsplit(a, ",")[[1]]
  c <- lobby_issue_map$government_entity_name[as.numeric(b)]
  d <- paste(c, collapse=";")
  return(d)
}

lobby_issue$gov_entity <- sapply(lobby_issue$government_entity_ids, process_entity)

#lobby_issue$government_entity_ids[1]
#test <- gsub("\\{|\\}", "", lobby_issue$government_entity_ids[1])
#test2 <- strsplit(test, ",")[[1]]
#test3 <- lobby_issue_map$government_entity_name[as.numeric(test2)]
#test4 <- paste(test3, collapse=";")

#fema=grepl("FEDERAL EMERGENCY MANAGEMENT AGENCY", lobby_issue$gov_entity)
#fema_reports = lobby_issue$report_uuid[which(fema & grepl("ENV|CLI|CAW|FUE", lobby_issue$issue_code))]
#reports = lobby_report[which(lobby_report$report_uuid %in% fema_reports),]
#length(unique(reports$client_uuid))
#fema_clients <- lobby_client[which(lobby_client$client_uuid %in% reports$client_uuid) , ]

#dat <- read.csv("data/03_final/lobbying_df_annual_REVISE_normal.csv", stringsAsFactors = F)
#unique(dat$client_uuid[which(dat$client_uuid %in% fema_clients$client_uuid)])


#####

## remove nuisance characters
#lobby_issue_old$gov_entity <- gsub('"|\\{|\\}|([\\])|-', ' ', lobby_issue_old$gov_entity)
#lobby_issue_old$gov_entity <- gsub(',', ';', lobby_issue_old$gov_entity)
#lobby_issue_old$gov_entity <- gsub("\\s+", " ", lobby_issue_old$gov_entity) # remove redundant spaces
#lobby_issue_old$gov_entity <- trimws(lobby_issue_old$gov_entity)

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
