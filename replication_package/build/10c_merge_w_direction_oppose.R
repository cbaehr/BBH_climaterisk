### Firms & Lobbying
### Data transformation

### Annual

# load packages
pacman::p_load(data.table, tidyverse, haven)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

# load lobbying data
lobby_client <- fread("data/01_raw/lobbyview_20250103/clients_codebook/clients.csv")

lobby_text <- fread("data/01_raw/lobbyview_20250324/issue_text.csv")

lobby_issue <- fread("data/01_raw/lobbyview_20250324/issues.csv")

lobby_report <- fread("data/01_raw/lobbyview_20250103/reports_codebook/reports.csv")

lobby_bill <- fread("data/01_raw/lobbyview_20250103/bills_codebook/bills.csv")

lobby_report$report_quarter_code <- ifelse(lobby_report$filing_period_code=="H1", 12,
                                           ifelse(lobby_report$filing_period_code=="H2", 34,
                                                  ifelse(lobby_report$filing_period_code=="Q1", 1,
                                                         ifelse(lobby_report$filing_period_code=="Q2", 2,
                                                                ifelse(lobby_report$filing_period_code=="Q3", 3, 4)))))

lobby_report$n_quarters <- str_count(as.character(lobby_report$report_quarter_code), "")

lobby_issue_map <- fread("data/01_raw/lobbyview_20250103/issues_codebook/government_entity_mapping.csv")
lobby_issue_map <- lobby_issue_map[!duplicated(lobby_issue_map) , ]

process_entity <- function(ids) {
  a <- gsub("\\{|\\}", "", ids)
  b <- as.numeric(strsplit(a, ",")[[1]])
  c <- lobby_issue_map$government_entity_name[match(b, lobby_issue_map$government_entity_id)]
  d <- paste(c, collapse=";")
  return(d)
}

lobby_issue$gov_entity <- sapply(lobby_issue$government_entity_ids, process_entity)

glimpse(lobby_issue)

############

## remove special escape characters
lobby_text$issue_text <- gsub("[^A-z0-9. ]", " ", lobby_text$issue_text)
lobby_text$issue_text <- gsub("`|\\^|\\[|\\]|\\\\|_", " ", lobby_text$issue_text)
lobby_text$issue_text <- gsub("\\s+", " ", lobby_text$issue_text) # remove redundant spaces

# Convert issue_ordinal_position to integer in both dataframes before merging
lobby_issue[, issue_ordinal_position := as.integer(issue_ordinal_position)]
lobby_text[, issue_ordinal_position := as.integer(issue_ordinal_position)]

glimpse(lobby_issue)
glimpse(lobby_text)

lobby_issuetext <- merge(lobby_issue, lobby_text)
glimpse(lobby_issuetext)

## sometimes multiple issue codes or government entities per lobby report
#View(lobby_issue[duplicated(lobby_issue$report_uuid) | duplicated(lobby_issue$report_uuid, fromLast = T),])

lobby_issuetext_nodup <- aggregate(lobby_issuetext[, c("general_issue_code", "gov_entity", "issue_text", "bill_id_agg")], 
                                   by=list(lobby_issuetext$report_uuid), 
                                   FUN=function(x) paste(x, collapse = ";")) |>
  setNames(c("report_uuid", "issue_code", "gov_entity", "issue_text", "bill_id_agg"))

glimpse(lobby_issuetext_nodup)

# #####

# ## there are some cases in the exposure_orbis data with multiple client_uuids for 
# ## a single isin code. These are ok to include -- we already made sure we arent duplicating
# ## data. However, to properly merge the lobbyview data with the exposure_orbis data,
# ## we need to treat the various members of the same isin with the same client_uuid. 
# ## this will ensure they all get fed into the same aggregation process. I reassign
# ## new client_uuids that are unique at the isin level -- they are just
# ## "client_uuid1|client_uuid2|client_uuid3|..."

# exposure_orbis_wide <- read.csv("data/02_processed/exposure_orbis_client_quarter_wide_REVISE_NEW.csv", stringsAsFactors = F)

# # duplicates?
# exposure_orbis_wide |>
#   count(gvkey, isin, bvdid, lob_id) |>
#   filter(n > 1)
# # none


# relevant <- exposure_orbis_wide[grepl("\\|", exposure_orbis_wide$lob_id), ] # these are the only client_uuids we need to worry about
# glimpse(relevant)
# relevant |>
#   tibble() |>
#   select(lob_id) |>
#   head()

# getid <- function(x) {
#   id <- grep(x, relevant$client_uuid)
#   if(length(id)>0) {
#     return(id)
#   } else {
#     return(NA)
#   }
# }

# ## identify which members in lobby_client have a client_uuid that falls into one
# ## of the "new" client_uuids I define. I also identify the index of the corresponding
# ## "new" client_uuid in exposure_orbis.
# glimpse(lobby_client)
# alt_ids <- sapply(lobby_client$client_uuid, FUN=function(x) getid(x))
# table(length(alt_ids)) # they all match with at most one client_uuid in exposure_orbis

# ## now create a new variable in lobby_client that is an "alternative" client_uuid
# ## it is either the "new" client_uuid from exposure_orbis, or if there is no need for 
# ## a "new" id, it is just the original (same) client_uuid as before
# client_uuid_alt <- relevant$client_uuid[alt_ids]
# lobby_client$client_uuid_alt <- ifelse(is.na(client_uuid_alt), lobby_client$client_uuid, client_uuid_alt)

#View(lobby_client[1:1000, ])

#####


# Merge report + client data
lobbyview <- lobby_report |>
  left_join(lobby_client, by = "lob_id")


# 1) Check for multiple amendments per report
amendments <- lobbyview |>
  filter(is_amendment == TRUE) |>
  group_by(lob_id, registrant_id, filing_year, filing_period_code, gvkey, client_name) |>
  summarise(
    n_amendments = n(),
    amendment_amounts = paste(amount, collapse = "; "),
    # get mean amount
    mean_amendment_amount = mean(amount, na.rm = TRUE),
    # Check if non-NA amounts in the group differ
    amendment_amounts_differ = length(unique(na.omit(amount))) > 1
  ) |>
  ungroup()

## Some inspection
table(amendments$n_amendments)
#     1     2     3     4     5     6     7     8     9    10    12    13 
# 74492  6748   937   167    36    11     3     3     3     1     1     1 

# View(amendments |>
#   filter(n_amendments > 1) |>
#   select(lob_id, registrant_id, filing_year, filing_period_code, gvkey, client_name, n_amendments, amounts, amounts_differ) |>
#   arrange(lob_id, filing_year, filing_period_code))

table(amendments$amendment_amounts_differ)
# FALSE  TRUE 
# 81487   916 

# inspect amounts for multiple amendments
amendments |>
  filter(n_amendments > 1) |>
  select(n_amendments, amendment_amounts, mean_amendment_amount, amendment_amounts_differ)

glimpse(amendments)


# 3) Create corrected amount column
lobbying_corrected <- lobbyview |>
  # filter out amendments
  filter(is_amendment == FALSE) |>
  # join with amendments 
  left_join(amendments, by = c("lob_id", "registrant_id", "filing_year", "filing_period_code", "gvkey", "client_name"))

glimpse(lobbying_corrected)

# 4) Create corrected amount column
lobbying_corrected <- lobbying_corrected |>
  rename(amount_original = amount) |>
  mutate(
    amounts_differ = case_when(
      !is.na(mean_amendment_amount) & (is.na(amount_original) | mean_amendment_amount != amount_original) ~ TRUE,
      TRUE ~ FALSE
    ),
    amount = case_when(
      !is.na(mean_amendment_amount) & (is.na(amount_original) | mean_amendment_amount != amount_original) ~ mean_amendment_amount,
      TRUE ~ amount_original
    )
  )

glimpse(lobbying_corrected)

# # View n_amendments > 1
# View(lobbying_corrected |>
#   filter(n_amendments > 1) |>
#   distinct() |>
#   arrange(filing_year, filing_period_code))

# # look at gvkey 7186 
# View(lobbying_corrected |>
#   filter(gvkey == 7186) |>
#   distinct() |>
#   arrange(desc(amount)))


summary(lobbying_corrected$amount)

# get highest 10 amounts
lobbying_corrected |>
  filter(!is.na(gvkey)) |>
  arrange(desc(amount)) |>
  head(10)

glimpse(lobbying_corrected)

lobbying <- merge(lobbying_corrected, lobby_issuetext_nodup, all.x = T)

glimpse(lobbying)

lobbying$report_quarter_code <- as.character(lobbying$report_quarter_code)

# lobbying$amount <- gsub("\\$|,", "", lobbying$amount)

## we match with firm data based on BvDID, so all clients under the same bvdid are assigned a consistent name

## here I want to collapse the lobbying data to the FIRM-year level - which would mean no duplication of client_uuid-year-bvdid (remove client_uuid)

## just treat lobbying amount as zero if missing -> wont affect the amount calculations, because
## missing would just be dropped. But makes the mapply easier
# lobbying$amount[which(lobbying$amount=="")] <- 0
lobbying <- lobbying |> mutate(amount = ifelse(is.na(amount), 0, amount))
lobbying$gov_entity[lobbying$gov_entity==""] <- NA

# collapse.char <- aggregate(lobbying[, c("client_uuid", "client_name", "report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
#                   by=list(lobbying$report_year, lobbying$bvdid),
#                   FUN = function(x) paste(x, collapse = "|"))
# collapse.char <- aggregate(lobbying[, c("report_uuid", "issue_code", "gov_entity", "issue_text", "registrant_uuid", "registrant_name", "report_quarter_code", "amount")],
#                            by=list(lobbying$report_year, lobbying$client_uuid),
#                            FUN = function(x) paste(x, collapse = "|"))
glimpse(lobbying)

collapse.char <- aggregate(lobbying[, c("report_uuid", "issue_code", "gov_entity", "issue_text", "bill_id_agg", "registrant_id", "registrant_name", "report_quarter_code", "amount")],
                           by=list(lobbying$filing_year, lobbying$lob_id),
                           FUN = function(x) paste(x, collapse = "|"))
lobbying_firmyear <- collapse.char
names(lobbying_firmyear) <- c("filing_year", "lob_id",
                              "report_uuid", "issue_code", "gov_entity", "issue_text", "bill_id_agg",
                              "registrant_id", "registrant_name", "report_quarter_code", "amount_num")
# collapse.num <- aggregate(lobbying[, c("amount_num")],
#                    by=list(lobbying$report_year, lobbying$bvdid),
#                    FUN = function(x) sum(x, na.rm = T))

#lobbying_firmyear <- merge(collapse.char, collapse.num)


lobbying_firmyear$n_issue_codes <- str_count(lobbying_firmyear$issue_code, "\\|") + 1

bill <- read.csv("data/01_raw/lobbyview_20250103/filtered_firm_bill_plus_df.csv", stringsAsFactors = F)
sum(duplicated(bill[ , c("lob_id", "bill_id")]))

appears <- grepl(paste(unique(bill$bill_id), collapse="|"), lobbying_firmyear$bill_id_agg)
sum(appears)

#firm_appears <- lobbying_firmyear$lob_id %in% unique(bill$lob_id)
#sum(firm_appears & appears)

lobbying_firmyear <- lobbying_firmyear[appears, ]

## Only bills that are supporting climate legislation
support_bills <- bill[which(bill$label_name=="Oppose") , ]

## Check each lobbyview entry to see if it corresponds to a firm-bill pair from the support_bills dataset

check_match <- function(x) {
  lob_match <- support_bills$lob_id == x["lob_id"] ## get all matching bills that are SUPPORTED
  bill_match <- unlist(sapply(support_bills$bill_id, FUN=function(y) grepl(y, x["bill_id_agg"]))) ## get all bills that share a lob ID
  if(any(lob_match & bill_match)) {
    return(T)
  } else {return(F)}
}

support_lobbying <- apply(lobbying_firmyear, 1, FUN=function(x) check_match(x))

lobbying_firmyear <- lobbying_firmyear[support_lobbying , ]


###

# rm(list = setdiff(ls(), "lobbying_firmyear"))

#####

glimpse(lobbying_firmyear)

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

###

issue_code_split[1]

bill_id_agg_split <- lapply(lobbying_firmyear$bill_id_agg, FUN = function(x) strsplit(x, "\\|")[[1]])
bill_id_agg_split[1]

bill <- read.csv("data/01_raw/lobbyview_20250103/filtered_firm_bill_plus_df.csv", stringsAsFactors = F)
clmtbills <- paste(unique(bill$bill_id), collapse="|")

climate_bill <- lapply(bill_id_agg_split, FUN = function(x) grepl(clmtbills, x))

climate_bill[1]
climate_issue[1]

###


## now produce a vector of T/F for each firm-year indicating if quarter i is mentioned in
## a given report
q1 <- lapply(quarters, FUN = function(x) grepl("1", x))
q2 <- lapply(quarters, FUN = function(x) grepl("2", x))
q3 <- lapply(quarters, FUN = function(x) grepl("3", x))
q4 <- lapply(quarters, FUN = function(x) grepl("4", x))

## now iterate through both the climate dummy list and quarter dummy list. Co-occurences
## of climate TRUE and quarter TRUE implies firm-year lobbied on climate in quarter i
lobbying_firmyear$CLI_q1 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_bill, q1)
lobbying_firmyear$CLI_q2 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_bill, q2)
lobbying_firmyear$CLI_q3 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_bill, q3)
lobbying_firmyear$CLI_q4 <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_bill, q4)
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



gov_entity_split <- lapply(toupper(lobbying_firmyear$gov_entity), FUN = function(x) strsplit(x, "\\|")[[1]])

#test <- strsplit(unlist(gov_entity_split), ";")
#test <- unlist(test)
#sort(unique(test))

View(lobbying_firmyear[which(lobbying_firmyear$lob_id=="f55a30bb-9185-58ae-981a-36d39d6d3406") , ])
test2 <- lobbying_firmyear$gov_entity[which(lobbying_firmyear$lob_id=="f55a30bb-9185-58ae-981a-36d39d6d3406") ]

grep("ENVIRONMENTAL PROTECTION AGENCY", toupper(test2))

# define the set of relevant agencies
agencies <- c(DOE="ENERGY, DEPT OF",
              EPA="ENVIRONMENTAL PROTECTION AGENCY",
              FEMA="FEDERAL EMERGENCY MANAGEMENT AGENCY",
              COEQ="COUNCIL ON ENVIRONMENTAL QUALITY",
              DOT="TRANSPORTATION, DEPT OF",
              DOTY="TREASURY, DEPT OF",
              DOA="AGRICULTURE, DEPT OF",
              NOAA="NATL OCEANIC & ATMOSPHERIC ADMINISTRATION",
              HOUS="HOUSE OF REPRESENTATIVES",
              SEN="SENATE",
              WTHS="WHITE HOUSE OFFICE")

# looping through agencies to build agency specific lobbying dummies by year/quarter
for(i in 1:length(agencies)) {
  
  agent <- lapply(gov_entity_split, FUN = function(x) grepl(agencies[i], x))
  
  lobbying_firmyear[ , sprintf("CLI_%s_annual", names(agencies)[i])] <- mapply(FUN = function(x1, x2) {any(x1 & x2)}, climate_bill, agent)
  
  lobbying_firmyear[ , sprintf("CLI_%s_q1", names(agencies)[i])] <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_bill, agent, q1)
  lobbying_firmyear[ , sprintf("CLI_%s_q2", names(agencies)[i])] <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_bill, agent, q2)
  lobbying_firmyear[ , sprintf("CLI_%s_q3", names(agencies)[i])] <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_bill, agent, q3)
  lobbying_firmyear[ , sprintf("CLI_%s_q4", names(agencies)[i])] <- mapply(FUN = function(x1, x2, x3) {any(x1 & x2 & x3)}, climate_bill, agent, q4)
  
}

#summary(lobbying_firmyear$CLI_annual)
#summary(lobbying_firmyear$CLI_q1 | lobbying_firmyear$CLI_q2 | lobbying_firmyear$CLI_q3 | lobbying_firmyear$CLI_q4)
## same distro of T/F values as should be

#summary(lobbying_firmyear$CLI_DOE_annual)
#summary(lobbying_firmyear$CLI_DOE_q1 | lobbying_firmyear$CLI_DOE_q2 | lobbying_firmyear$CLI_DOE_q3 | lobbying_firmyear$CLI_DOE_q4)

## now move on to AMOUNT

## for each report in a firm-year, we compute the proportion of issues in that REPORT
## that are climate-related.

#climate_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENV|CAW|ENG|FUE", y))}[[1]])})
climate_issue_proportion <- lapply(issue_code_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl("ENV|CAW|ENG|FUE", y))}[[1]])})
gov_entity_split <- lapply(toupper(lobbying_firmyear$gov_entity), FUN = function(x) strsplit(x, "\\|")[[1]])

climate_issue_proportion[[1]]

bill <- read.csv("data/01_raw/lobbyview_20250103/filtered_firm_bill_plus_df.csv", stringsAsFactors = F)
clmtbills <- paste(unique(bill$bill_id), collapse="|")

lobbying_firmyear$bill_id_agg <- gsub("\\|", " \\| ", lobbying_firmyear$bill_id_agg)


bill_id_agg_split <- lapply(lobbying_firmyear$bill_id_agg, FUN = function(x) strsplit(x, "\\|")[[1]])
## Splitting on both semicolons and commas -- if in a single report you lobbied on multiple bills, only
## count those specific bills that are mentioned in the climate data as pro-climate lobbying (numerator) and put
## other bills from same report in the proportion denominator
bill_id_agg_proportion <- lapply(bill_id_agg_split, FUN = function(x) {sapply(strsplit(x, ";|,"), FUN = function(y) {mean(grepl(clmtbills, y))}[[1]])})

bill_id_agg_split[1]
bill_id_agg_proportion[1]
grep("hr2642-113", clmtbills)
grep("s954-113", clmtbills)
## Looks good 

amount_split <- lapply(lobbying_firmyear$amount, FUN = function(x) strsplit(x, "\\|")[[1]])

test1 <- unlist(lapply(amount_split, length))
test2 <- unlist(lapply(bill_id_agg_proportion, length))
sum(test1==test2)

climate_amount <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, bill_id_agg_proportion)
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
total_proportion_q1 <- mapply(FUN = function(x1, x2) {x1*x2}, q1_proportion, bill_id_agg_proportion)
total_proportion_q2 <- mapply(FUN = function(x1, x2) {x1*x2}, q2_proportion, bill_id_agg_proportion)
total_proportion_q3 <- mapply(FUN = function(x1, x2) {x1*x2}, q3_proportion, bill_id_agg_proportion)
total_proportion_q4 <- mapply(FUN = function(x1, x2) {x1*x2}, q4_proportion, bill_id_agg_proportion)

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

glimpse(lobbying_firmyear)

summary(lobbying_firmyear$CLI_amount_q1)
# Variation there

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


# amount_split[900]
# climate_issue[900]
# climate_issue_proportion[900]
# epa_proportion[900]
# epa_amount[900]
# doe_amount[900]

# looping through agencies
for(i in 1:length(agencies)) {
  
  agency_proportion <- lapply(gov_entity_split, FUN = function(x) {sapply(strsplit(x, ";"), FUN = function(y) {mean(grepl(agencies[i], y))}[[1]])})
  total_proportion_agency_q1 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q1_proportion, climate_issue_proportion, agency_proportion)
  total_proportion_agency_q2 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q2_proportion, climate_issue_proportion, agency_proportion)
  total_proportion_agency_q3 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q3_proportion, climate_issue_proportion, agency_proportion)
  total_proportion_agency_q4 <- mapply(FUN = function(x1, x2, x3) {x1*x2*x3}, q4_proportion, climate_issue_proportion, agency_proportion)
  
  agency_amount_q1 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_agency_q1)
  agency_amount_q2 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_agency_q2)
  agency_amount_q3 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_agency_q3)
  agency_amount_q4 <- mapply(FUN = function(x1, x2) {sum(as.numeric(x1) * x2)}, amount_split, total_proportion_agency_q4)
  
  lobbying_firmyear[ , sprintf("CLI_%s_amount_q1", names(agencies)[i])] <- agency_amount_q1
  lobbying_firmyear[ , sprintf("CLI_%s_amount_q2", names(agencies)[i])] <- agency_amount_q2
  lobbying_firmyear[ , sprintf("CLI_%s_amount_q3", names(agencies)[i])] <- agency_amount_q3
  lobbying_firmyear[ , sprintf("CLI_%s_amount_q4", names(agencies)[i])] <- agency_amount_q4
  
  
}






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

# rm(list = setdiff(ls(), c("lobbying_firmyear", "issues", "agencies")))

#####

timespan <- paste0("q", 1:4)
time_varying <- c("CLI_", "CLI_amount_",
                  paste0("CLI_", names(agencies), "_"),
                  paste0("CLI_", names(agencies), "_amount_"),
                  #"CLI_DOE_", "CLI_EPA_", 
                  #"CLI_DOE_amount_", "CLI_EPA_amount_",  
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
# none

## reshape data from wide to long format
names(lobbying_firmyear)

lobbying_firmyear$unique_id <- paste(lobbying_firmyear$lob_id, lobbying_firmyear$filing_year)

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
names(lobbying_firmquarter)[names(lobbying_firmquarter)=="total_lobby_q1"] <- "total_lobby_quarter"

# looping through agencies
for(i in 1:length(agencies)) {
  # change names from "q1" to "quarter"
  names(lobbying_firmquarter)[names(lobbying_firmquarter)==sprintf("CLI_%s_q1", names(agencies)[i])] <- sprintf("CLI_%s_quarter", names(agencies)[i])
  names(lobbying_firmquarter)[names(lobbying_firmquarter)==sprintf("CLI_%s_amount_q1", names(agencies)[i])] <- sprintf("CLI_%s_amount_quarter", names(agencies)[i])
}


lobbying_firmquarter$qtr <- gsub("q", "", lobbying_firmquarter$qtr)

## drop some lobbying variables that are still annual - dont want to create confusion for instance
## that ENV issue codes for a whole year are actually for a specific quarter. If we want to make these
## quarterly, we can do that later.
lobbying_firmquarter <- lobbying_firmquarter[,!names(lobbying_firmquarter) %in% c("issue_code", "gov_entity", "issue_text")]

save(lobbying_firmquarter, file="data/02_processed/lobbying_df_quarterly_REVISE_NEW_altclimatebills_oppose.rds")



#####

# Exposure Orbis Long --------------------------------------------------------

exposure_orbis_long <- read.csv("data/02_processed/exposure_orbis_client_quarter_long_REVISE_NEW.csv", stringsAsFactors = F)

glimpse(exposure_orbis_long)

#out <- merge(exposure_orbis, lobbying_firmyear, by.x = c("bvdid", "year"), by.y = c("bvdid", "report_year"))
exposure_orbis_lobbyview_long <- merge(exposure_orbis_long, lobbying_firmquarter, by.x = c("lob_id", "year", "qtr"), by.y = c("lob_id", "filing_year", "qtr"), all.x=T)
#out3 <- merge(exposure_orbis_long, lobbying_firmyear, by.x = c("client_uuid", "year"), by.y = c("client_uuid", "report_year"))

sum(duplicated(exposure_orbis_lobbyview_long[, c("isin", "year", "qtr")]))
# none
sum(duplicated(exposure_orbis_lobbyview_long[, c("gvkey", "year", "qtr")]))
# 14,720
sum(duplicated(exposure_orbis_lobbyview_long[, c("bvdid", "year", "qtr")]))
# none
sum(duplicated(exposure_orbis_lobbyview_long[, c("lob_id", "year", "qtr")]) & exposure_orbis_lobbyview_long$lob_id!=(-1)) #good
# none

test <- (duplicated(exposure_orbis_lobbyview_long[, c("lob_id", "year", "qtr")]) & exposure_orbis_lobbyview_long$lob_id!=(-1)) | (duplicated(exposure_orbis_lobbyview_long[, c("lob_id", "year", "qtr")], fromLast=T) & exposure_orbis_lobbyview_long$lob_id!=(-1))
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
exposure_orbis_lobbyview_long |>
  filter(bvdid == "US911975651") |>
  select(conm, year, qtr, ebit_at)
# drop_clipper2011 <- exposure_orbis_lobbyview_long$conm=="CLIPPER WINDPOWER HOLDINGS LTD" & exposure_orbis_lobbyview_long$year==2011
# exposure_orbis_lobbyview_long <- exposure_orbis_lobbyview_long[which(!drop_clipper2011), ]

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

exposure_orbis_lobbyview_long$ebit <- exposure_orbis_lobbyview_long$ebit / 1000000

exposure_orbis_lobbyview_long$CLI_amount_quarter[is.na(exposure_orbis_lobbyview_long$CLI_amount_quarter)] <- 0

# looping thru agency dummies to clean variables
for(i in 1:length(agencies)) {
  # change to numeric
  exposure_orbis_lobbyview_long[ , sprintf("CLI_%s_quarter", names(agencies)[i])] <- as.numeric(exposure_orbis_lobbyview_long[ , sprintf("CLI_%s_quarter", names(agencies)[i])])
  # change NA dummies to 0
  exposure_orbis_lobbyview_long[is.na(exposure_orbis_lobbyview_long[ , sprintf("CLI_%s_quarter", names(agencies)[i])]) , sprintf("CLI_%s_quarter", names(agencies)[i])] <- 0
  # change NA amounts to 0
  exposure_orbis_lobbyview_long[is.na(exposure_orbis_lobbyview_long[ , sprintf("CLI_%s_amount_quarter", names(agencies)[i])]) , sprintf("CLI_%s_amount_quarter", names(agencies)[i])] <- 0
  
}


arrow::write_parquet(exposure_orbis_lobbyview_long, "data/03_final/lobbying_df_quarterly_REVISE_NEW_altclimatebills_oppose.parquet")

# # check
# names(exposure_orbis_lobbyview_long)
# glimpse(exposure_orbis_lobbyview_long)
# 
# ## write csv
# fwrite(exposure_orbis_lobbyview_long, "data/03_final/lobbying_df_quarterly_REVISE_NEW_altclimatebills_oppose.csv")
# 
# # write rdata
# write_rds(exposure_orbis_lobbyview_long, "data/03_final/lobbying_df_quarterly_REVISE_NEW_altclimatebills_oppose.rds")


# exposure_orbis_lobbyview_long_qrt <- read_rds("data/03_final/lobbying_df_quarterly_REVISE.rds")


### END

