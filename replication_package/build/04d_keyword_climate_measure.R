### Build new climate measure based on keywords


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haven, tidytext, stopwords)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

# load lobbying data
# lobby_client_old <- fread("data/01_raw/lobbyview/dataset___client_level.csv")
lobby_client <- fread("data/01_raw/lobbyview_20250103/clients_codebook/clients.csv")

# lobby_text_old <- fread("data/01_raw/lobbyview/dataset___issue_text.csv")
# lobby_text <- fread("data/01_raw/lobbyview_20250103/issue_text_codebook/issue_text.csv")
lobby_text <- fread("data/01_raw/lobbyview_20250324/issue_text.csv")
# glimpse(lobby_text)

# lobby_issue_old <- fread("data/01_raw/lobbyview/dataset___issue_level.csv")
# lobby_issue <- fread("data/01_raw/lobbyview_20250103/issues_codebook/issues.csv")
lobby_issue <- fread("data/01_raw/lobbyview_20250324/issues.csv")

# lobby_report_old <- fread("data/01_raw/lobbyview/dataset___report_level.csv")
lobby_report <- fread("data/01_raw/lobbyview_20250103/reports_codebook/reports.csv")
#glimpse(lobby_report)

# lobby_report <- fread("/Users/christianbaehr/Dropbox/BBH/BBH1/data/01_raw/lobbyview/dataset___report_level.csv")
# lobby_issue_old <- fread("/Users/christianbaehr/Dropbox/BBH/BBH1/data/01_raw/lobbyview/dataset___issue_level.csv")

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

#glimpse(lobby_issue)

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


# Duplicates in lobby_issue and lobby_text -----------------------------------


# Convert issue_ordinal_position to integer in both dataframes before merging
lobby_issue[, issue_ordinal_position := as.integer(issue_ordinal_position)]
lobby_text[, issue_ordinal_position := as.integer(issue_ordinal_position)]

# glimpse(lobby_issue)
# glimpse(lobby_text)

# # Duplicates?
# lobby_issue |>
#   count(report_uuid) |>
#   filter(n > 1)

# View(lobby_issue |>
#   filter(report_uuid == "00003552-cb8b-432d-a5e4-0f7d22afce59") |>
#   glimpse())

# # Duplicates?
# lobby_issue |>
#   distinct() |>
#   count(report_uuid, general_issue_code, issue_ordinal_position) |>
#   filter(n > 1)
# government_entity_ids don't differ by report_uuid, general_issue_code, issue_ordinal_position

# View(lobby_issue |>
#   filter(report_uuid == "06b4e755-cde7-4e56-bb0d-0f367cbb5a03"))

# Merge
lobby_issuetext <- lobby_issue |>
  left_join(lobby_text, by = c("report_uuid", "general_issue_code", "issue_ordinal_position"))

# glimpse(lobby_issuetext)

# # duplicates?
# lobby_issuetext |>
#   count(report_uuid, general_issue_code, issue_ordinal_position) |>
#   filter(n > 1) |>
#   arrange(desc(n))

# View(lobby_issuetext |>
#   filter(report_uuid == "179620b8-ad2c-4036-8c28-c3bf9252dbae"))

# lobby_issuetext |>
#   count(report_uuid, general_issue_code, issue_ordinal_position, gov_entity) |>
#   filter(n > 1) |>
#   arrange(desc(n))

# distinct
lobby_issuetext_distinct <- lobby_issuetext |>
  distinct() |> 
  ungroup()

# # is gov_entity constant for each report_uuid?
# lobby_issuetext_distinct |>
#   distinct(report_uuid, gov_entity) |>
#   count(report_uuid) |>
#   filter(n > 1) |>
#   arrange(desc(n))
# # gov_entity is constant for each report_uuid

# glimpse(lobby_issuetext_distinct)

# Stack together at report_uuid - general_issue_code level
lobby_text_summarized <- lobby_issuetext_distinct %>%
  group_by(report_uuid, government_entity_ids, gov_entity) %>%
  summarize(
    issue_code = ifelse(all(is.na(general_issue_code) | general_issue_code == ""),
      NA,
      paste(unique(general_issue_code[!is.na(general_issue_code) & general_issue_code != ""]), collapse = "|")
    ),
    bill_id_agg = ifelse(all(is.na(bill_id_agg) | bill_id_agg == ""),
      NA,
      paste(unique(bill_id_agg[!is.na(bill_id_agg) & bill_id_agg != ""]), collapse = ", ")
    ),
    issue_text = ifelse(all(is.na(issue_text) | issue_text == ""),
      NA,
      paste(unique(issue_text), collapse = " | ")
    )
  ) |>
  ungroup()

# glimpse(lobby_text_summarized)

# # duplicates?
# lobby_text_summarized |>
#   count(report_uuid, general_issue_code, government_entity_ids, gov_entity) |>
#   filter(n > 1) |>
#   arrange(desc(n))
# # none

# # duplicates with only report_uuid?
# lobby_text_summarized |>
#   count(report_uuid) |>
#   filter(n > 1) |>
#   arrange(desc(n))

# View(lobby_text_summarized |>
#   filter(report_uuid == "bc32ade3-db93-458f-8ce1-495ea3036e4e"))




# Create measure based on keywords ------------------------------------------

# mitigation_kw <- c(
#     "climate change", "global warming", "greenhouse gas", "carbon emission", "cap and trade", "low carbon", 
#     "carbon pricing", "carbon capture", "carbon tax", "methane emission", "renewable energy", "clean energy", 
#     "climate mitigation", "climate", "Paris Agreement", "global warming", "Kyoto", 
#     "greenhouse gas", "clean power plan", "carbon emission", "clean energy"
# )

# adaptation_kw <- c(
#     "climate adaptation", "climate resilience", "climate risk", "extreme weather", "natural disaster", 
#     "coastal area", "sea level", "flood", "storm water", "extreme heat", "coastal erosion", "wildfire", 
#     "storm surge", "drought", "hurricane", "heatwave", "climate insurance", "federal emergency", 
#     "resilient building", "risk management"
# )


# head(tibble(lobby_text |> select(issue_text)))

## remove special escape characters
lobby_text_summarized$issue_text_clean <- gsub("[^A-z0-9. ]", " ", lobby_text_summarized$issue_text)
lobby_text_summarized$issue_text_clean <- gsub("`|\\^|\\[|\\]|\\\\|_", " ", lobby_text_summarized$issue_text_clean)
lobby_text_summarized$issue_text_clean <- gsub("\\s+", " ", lobby_text_summarized$issue_text_clean) # remove redundant spaces

# # Filter keyword
# lobby_text <- lobby_text |>
#   mutate(
#     CLI_mitigation = str_detect(tolower(issue_text_clean), paste(tolower(mitigation_kw), collapse = "|")),
#     CLI_adaptation = str_detect(tolower(issue_text_clean), paste(tolower(adaptation_kw), collapse = "|"))
#   )

# table(lobby_text$CLI_mitigation, useNA = "always")
# 101589 / (3059955 + 101589)
# # 3,2%
# table(lobby_text$CLI_adaptation, useNA = "always")
# 44641 / (3116903 + 44641)
# # 1,4%

# # Combine
# lobby_text <- lobby_text |>
#   mutate(
#     CLI_keywords = CLI_mitigation | CLI_adaptation
#   )

# table(lobby_text$CLI_keywords, useNA = "always")
# 142178 / (3019366 + 142178)
# # 4,5%

# # Inspect
# lobby_text |>
#   filter(CLI_keywords) |>
#   select(issue_text, CLI_mitigation, CLI_adaptation, CLI_keywords) |>
#   head(10)

# # get most frequent words in issue_text
# frequent_words <- lobby_text |>
#   unnest_tokens(word, issue_text) |>
#   count(word, sort = TRUE)

# frequent_words |>
#   head(100)

# # which of the top 1000 most frequent words are in the mitigation_kw or adaptation_kw?
# frequent_words |>
#   filter(word %in% mitigation_kw | word %in% adaptation_kw) |>
#   head(100)


# Define stopwords and other non-essential words
stopwords <- c(stopwords("en", "snowball"), stopwords("en", "marimo"), stopwords("en", "nltk"), stopwords("en", "stopwords-iso")) |>
  unique() |>
  sort()

# # which of the frequent words are in the stopwords list?
# frequent_words |>
#   mutate(is_stopword = word %in% stopwords) |>
#   filter(is_stopword == FALSE) |>
#   slice(1:200) |>
#   pull(word)


# specific stopwords
add_stopwords <- c(
  "h.r", "issues", "hr", "provisions", "legislation", "reform", "federal",
  "support", "program","national", "relating", "programs", "department",
  "including", "implementation", "fy", "policy", "agencies", "access",
  "reauthorization", "authorization", "american", "house", "senate",
  "title", "u.s", "monitor", "government", "law", "administration",
  "amend", "purposes", "regulations", "united", "regulation", "provide",
  "america", "p.l", "bills", "rule", "legislative", "pertaining",
  "proposed", "proposals", "enforcement", "congress", "policies",
  "representatives", "senators", "staff", "draft", "lobbied",
  "acting", "appropriations", "funding", "reduction", "issue",
  "extension", "congressional", "matters", "reimbursement",
  "continuing", "word", "impact"
)

# combine stopwords
stopwords <- c(stopwords, add_stopwords)

# mitigation_kw
# adaptation_kw

# lobby_text_sample <- lobby_text |>
#   # filter(str_detect(CLI_keywords, "TRUE")) |>
#   sample_n(2000)

glimpse(lobby_text_summarized)

lobby_text_joined <- lobby_text_summarized |>
  left_join(
    lobby_text_summarized |>
      # tokenize and remove stopwords
      unnest_tokens(word, issue_text) |>
      # remove special characters
      mutate(word = str_remove_all(word, "[^a-zA-Z]")) |>
      # remove extra whitespace
      mutate(word = str_squish(word)) |>
      # remove stopwords
      filter(!word %in% stopwords) |>
      # remove empty words
      filter(word != "") |>
      # add back together
      group_by(report_uuid, government_entity_ids, gov_entity) |>
      summarise(issue_text_clean_2 = paste(word, collapse = " ")),
    by = c("report_uuid", "government_entity_ids", "gov_entity")
  )

# # most frequent words in issue_text_clean
# lobby_text |>
#   unnest_tokens(word, issue_text_clean_2) |>
#   count(word, sort = TRUE) |>
#   head(100)

glimpse(lobby_text_joined)


# reduced keywords
mitigation_kw <- c(
    "climate change", "global warming", "greenhouse gas", "carbon emission", "cap trade", "low carbon", 
    "carbon pricing", "carbon capture", "carbon tax", "methane emission", "renewable energy", "clean energy", 
    "climate mitigation", "climate", "Paris Agreement", "global warming", "Kyoto", 
    "greenhouse gas", "clean power plan", "carbon emission", "clean energy"
)

adaptation_kw <- c(
    "climate adaptation", "climate resilience", "climate risk", "extreme weather", "natural disaster", 
    "coastal area", "sea level", "flood", "storm water", "extreme heat", "coastal erosion", "wildfire", 
    "storm surge", "drought", "hurricane", "heatwave", "climate insurance", "federal emergency", 
    "resilient building", "risk management"
)

# Count the number of keywords in issue_text_clean
lobby_text_joined <- lobby_text_joined |>
  mutate(
    n_mitigation_kw = str_count(tolower(issue_text_clean_2), paste(tolower(mitigation_kw), collapse = "|")),
    n_adaptation_kw = str_count(tolower(issue_text_clean_2), paste(tolower(adaptation_kw), collapse = "|")),
    n_words = str_count(tolower(issue_text_clean_2), "\\b\\w+\\b"),
    prop_mitigation_kw = n_mitigation_kw / n_words, 
    prop_adaptation_kw = n_adaptation_kw / n_words,
    prop_keywords = (n_mitigation_kw + n_adaptation_kw) / n_words
  )

# summary(lobby_text_joined$prop_keywords)
# hist(lobby_text_joined$prop_keywords)
# summary(lobby_text_joined$prop_mitigation_kw)
# hist(lobby_text_joined$prop_mitigation_kw)
# summary(lobby_text_joined$prop_adaptation_kw)
# hist(lobby_text_joined$prop_adaptation_kw)

glimpse(lobby_text_joined)


# merge with lobbying data

glimpse(lobby_report)
glimpse(lobby_text_joined)
glimpse(lobby_client)

# which columns are in both?
intersect(names(lobby_report), names(lobby_text_joined))
# only report_uuid

intersect(names(lobby_client), names(lobby_text_joined))
# none



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

lobbying <- merge(lobbying_corrected, lobby_text_joined, all.x = T)

# 
# lobbying <- lobby_report |>
#   left_join(
#     lobby_text_joined |>
#       select(-issue_text_clean) |>
#       rename(issue_text_clean = issue_text_clean_2),
#     by = c("report_uuid")
#   ) |>
#   left_join(
#     lobby_client,
#     by = c("lob_id")
#   )

glimpse(lobbying)

# # duplicates?
lobbying |>
  count(report_uuid) |>
  filter(n > 1) |>
  arrange(desc(n))
# none


lobbying$report_quarter_code <- as.character(lobbying$report_quarter_code)
lobbying <- lobbying |> mutate(amount = ifelse(is.na(amount), 0, amount))
lobbying$gov_entity[lobbying$gov_entity==""] <- NA

glimpse(lobbying)


# Quarterly ---------------------------------------------------------------

# get to firm-year-quarter level
# Tokenize issue_text_clean into bigrams and count them

# bigrams_qtr <- lobbying |>
#   unnest_tokens(bigram, issue_text_clean, token = "ngrams", n = 2) |>
#   select(filing_year, report_quarter_code, lob_id, bigram)

# # sample
# View(bigrams_qtr |>
# filter(lob_id == "9202dcc9-900f-5cab-840e-92a6acb2a0ab" & filing_year == 2021 & report_quarter_code == "3")
# )

# lobbying |>
#   filter(lob_id == "9202dcc9-900f-5cab-840e-92a6acb2a0ab" & filing_year == 2021 & report_quarter_code == "3") |>
#   select(issue_text_clean)

## bigrams work

lobbying_bigrams_qtr <- lobbying |>
  unnest_tokens(bigram, issue_text_clean, token = "ngrams", n = 2) |>
  group_by(filing_year, report_quarter_code, lob_id) |>
  summarise(n_bigrams = n()) |>
  ungroup()

lobbying_firmqtr <- lobbying |>
  group_by(filing_year, report_quarter_code, lob_id) |>
  summarise(
    report_uuid = paste(unique(report_uuid[!is.na(report_uuid) & report_uuid != ""]), collapse = "|"),
    issue_code = paste(unique(issue_code[!is.na(issue_code) & issue_code != ""]), collapse = "|"),
    gov_entity = paste(unique(gov_entity[!is.na(gov_entity) & gov_entity != ""]), collapse = "|"),
    issue_text = paste(unique(issue_text[!is.na(issue_text) & issue_text != ""]), collapse = "|"),
    issue_text_clean = paste(unique(issue_text_clean[!is.na(issue_text_clean) & issue_text_clean != ""]), collapse = "|"),
    registrant_id = paste(unique(registrant_id[!is.na(registrant_id) & registrant_id != ""]), collapse = "|"),
    registrant_name = paste(unique(registrant_name[!is.na(registrant_name) & registrant_name != ""]), collapse = "|"),
    amount = sum(amount, na.rm = TRUE),
    n_mitigation_kw = sum(n_mitigation_kw, na.rm = TRUE),
    n_adaptation_kw = sum(n_adaptation_kw, na.rm = TRUE),
    n_words = sum(n_words, na.rm = TRUE)
  ) |>
  left_join(lobbying_bigrams_qtr, by = c("filing_year", "report_quarter_code", "lob_id")) |>
  mutate(
    prop_mitigation_kw = n_mitigation_kw / n_bigrams,
    prop_adaptation_kw = n_adaptation_kw / n_bigrams,
    prop_keywords = (n_mitigation_kw + n_adaptation_kw) / n_bigrams
  ) |>
  ungroup()

glimpse(lobbying_firmqtr)

table(lobbying_firmqtr$report_quarter_code, useNA = "always")
# no NAs

# sample
lobbying_firmqtr |>
  filter(n_mitigation_kw > 0 | n_adaptation_kw > 0) |>
  sample_n(100) |>
  glimpse()

## Hal-year ----------------------------------------------------------------

# there are cases where the report_quarter_code is 12 and 34
# for these cases, we need to split the report_quarter_code into two quarters and distribute the amount and prop_keywords
# between the two quarters

# inspect
half_years <- lobbying_firmqtr |>
  filter(str_detect(report_quarter_code, "12|34"))

glimpse(half_years)

lobbying_firm_half <- lobbying_firmqtr |>
  # create two rows for each firm-halfyear
  filter(str_detect(report_quarter_code, "12|34")) |>
  separate_rows(report_quarter_code, sep = "") |>
  filter(report_quarter_code %in% c("1", "2", "3", "4")) |>
  group_by(filing_year, report_quarter_code, lob_id) |>
  mutate(
    amount = sum(amount, na.rm = TRUE) / 2,
    n_mitigation_kw = sum(n_mitigation_kw, na.rm = TRUE) / 2,
    n_adaptation_kw = sum(n_adaptation_kw, na.rm = TRUE) / 2,
    n_words = sum(n_words, na.rm = TRUE) / 2,
    n_bigrams = sum(n_bigrams, na.rm = TRUE) / 2,
    prop_mitigation_kw = n_mitigation_kw / n_bigrams,
    prop_adaptation_kw = n_adaptation_kw / n_bigrams,
    prop_keywords = (n_mitigation_kw + n_adaptation_kw) / n_bigrams
  ) |>
  ungroup()

glimpse(lobbying_firm_half)


## Combine quarterly and half-yearly data -----------------------------------

lobbying_firm_qtr_corrected <- lobbying_firmqtr |>
  filter(!str_detect(report_quarter_code, "12|34")) |>
  bind_rows(lobbying_firm_half)

nrow(lobbying_firm_qtr_corrected)
nrow(lobbying_firmqtr)
nrow(lobbying_firm_half)

glimpse(lobbying_firm_qtr_corrected)

# duplicates?
lobbying_firm_qtr_corrected |>
  count(lob_id, filing_year, report_quarter_code) |>
  filter(n > 1) |>
  arrange(desc(n))

# # check
# View(lobbying_firm_qtr_corrected |>
#   filter(lob_id == "04a4fa83-3de2-5dba-94a4-facc0ce4ee27" & filing_year == 2007 & report_quarter_code == "4") 
# )
# 
# View(lobbying_firmqtr |>
#   filter(lob_id == "04a4fa83-3de2-5dba-94a4-facc0ce4ee27" & filing_year == 2007) 
# )


# Some duplicates come from the fact that firms submit half-yearly reports and additional
# quarterly reports.

# To deal with this, we collapse by lob_id, filing_year, and report_quarter_code

lobbying_firm_qtr_corrected_2 <- lobbying_firm_qtr_corrected |>
  group_by(lob_id, filing_year, report_quarter_code) |>
  summarise(
    report_uuid = paste(unique(report_uuid[!is.na(report_uuid) & report_uuid != ""]), collapse = "|"),
    issue_code = paste(unique(issue_code[!is.na(issue_code) & issue_code != ""]), collapse = "|"),
    gov_entity = paste(unique(gov_entity[!is.na(gov_entity) & gov_entity != ""]), collapse = "|"),
    issue_text = paste(unique(issue_text[!is.na(issue_text) & issue_text != ""]), collapse = "|"),
    issue_text_clean = paste(unique(issue_text_clean[!is.na(issue_text_clean) & issue_text_clean != ""]), collapse = "|"),
    registrant_id = paste(unique(registrant_id[!is.na(registrant_id) & registrant_id != ""]), collapse = "|"),
    registrant_name = paste(unique(registrant_name[!is.na(registrant_name) & registrant_name != ""]), collapse = "|"),
    amount = sum(amount, na.rm = TRUE),
    n_mitigation_kw = sum(n_mitigation_kw, na.rm = TRUE),
    n_adaptation_kw = sum(n_adaptation_kw, na.rm = TRUE),
    n_words = sum(n_words, na.rm = TRUE),
    n_bigrams = sum(n_bigrams, na.rm = TRUE)
  ) |>
  distinct() |>
  ungroup()

glimpse(lobbying_firm_qtr_corrected_2)

# duplicates?
lobbying_firm_qtr_corrected_2 |>
  count(lob_id, filing_year, report_quarter_code) |>
  filter(n > 1) |>
  arrange(desc(n))
# none

# Calculate prop_keywords
lobbying_firm_qtr_corrected_2 <- lobbying_firm_qtr_corrected_2 |>
  mutate(
    prop_mitigation_kw = n_mitigation_kw / n_bigrams,
    prop_adaptation_kw = n_adaptation_kw / n_bigrams,
    prop_keywords = (n_mitigation_kw + n_adaptation_kw) / n_bigrams
  )

glimpse(lobbying_firm_qtr_corrected_2)

lobbying_firm_qtr_corrected_2 |>
filter(n_mitigation_kw > 0 | n_adaptation_kw > 0) |>
sample_n(100) |>
glimpse()

# Distribute amount by prop_keywords
lobbying_firm_qtr_corrected_2 <- lobbying_firm_qtr_corrected_2 |>
  mutate(
    CLI_kw = case_when(
      n_mitigation_kw > 0 | n_adaptation_kw > 0 ~ 1,
      TRUE ~ 0
    ),
    CLI_mitigation = case_when(
      n_mitigation_kw > 0 ~ 1,
      TRUE ~ 0
    ),
    CLI_adaptation = case_when(
      n_adaptation_kw > 0 ~ 1,
      TRUE ~ 0
    ),
    CLI_amount_kw = amount * prop_keywords,
    CLI_amount_mitigation = amount * prop_mitigation_kw,
    CLI_amount_adaptation = amount * prop_adaptation_kw
  )

glimpse(lobbying_firm_qtr_corrected_2)

# sample
lobbying_firm_qtr_corrected_2 |>
  filter(n_mitigation_kw > 0 | n_adaptation_kw > 0) |>
  sample_n(100) |>
  glimpse()

# Merge w/ orbis - exposure data ------------------------------------------


rm(list = setdiff(ls(), c("lobbying_firm_qtr_corrected_2")))

# Load exposure & orbis
exposure_orbis_lobby_long <- fread("data/02_processed/exposure_orbis_client_quarter_long_REVISE_NEW.csv")

glimpse(exposure_orbis_lobby_long)
glimpse(lobbying_firm_qtr_corrected_2)

df_qtr <- exposure_orbis_lobby_long |>
  left_join(
    lobbying_firm_qtr_corrected_2 |>
      mutate(
        report_quarter_code = as.numeric(report_quarter_code)
      ),
    by = c("lob_id", "year" = "filing_year", "qtr" = "report_quarter_code")
  )

glimpse(df_qtr)


sum(duplicated(df_qtr[, c("isin", "year", "qtr")]))
# none
sum(duplicated(df_qtr[, c("gvkey", "year", "qtr")]))
# 14,076
sum(duplicated(df_qtr[, c("bvdid", "year", "qtr")]))
# none
sum(duplicated(df_qtr[, c("lob_id", "year", "qtr")]) & df_qtr$lob_id!=(-1)) #good
# none

## new variable captures whether firms lobbied on NONCLIMATE issues in a year
nonclimate <- !is.na(df_qtr$report_uuid) & !df_qtr$CLI_kw
df_qtr$nonCLI_quarter <- F
df_qtr$nonCLI_quarter[which(nonclimate)] <- T


df_qtr$us_dummy <- ifelse(df_qtr$hqcountrycode=="US", 1, 0)
df_qtr$industry <- df_qtr$bvdsector
df_qtr$industry_year <- paste(df_qtr$industry, df_qtr$year)

df_qtr$ebit <- as.numeric(df_qtr$P_L_b4tax_usd) # some "n.a."s drop out
df_qtr$at <- as.numeric(df_qtr$total_assets_usd) # some "n.a."s drop out
df_qtr$ebit_at <- df_qtr$ebit / df_qtr$at

## few cases with zero denominator
invalid_orbis <- which(df_qtr$ebit_at %in% c(-Inf, Inf))
df_qtr$at[invalid_orbis] <- NA
df_qtr$ebit_at[invalid_orbis] <- NA

#View(exposure_orbis_lobbyview_long[which(exposure_orbis_lobbyview_long$ebit_at<(-1000)), ])
df_qtr |>
  filter(bvdid == "US911975651") |>
  select(conm, year, qtr, ebit_at)
# drop_clipper2011 <- exposure_orbis_lobbyview_long$conm=="CLIPPER WINDPOWER HOLDINGS LTD" & exposure_orbis_lobbyview_long$year==2011
# exposure_orbis_lobbyview_long <- exposure_orbis_lobbyview_long[which(!drop_clipper2011), ]

df_qtr$CLI_kw[is.na(df_qtr$CLI_kw)] <- 0
df_qtr$CLI_mitigation[is.na(df_qtr$CLI_mitigation)] <- 0
df_qtr$CLI_adaptation[is.na(df_qtr$CLI_adaptation)] <- 0

# Amounts
df_qtr$CLI_amount_kw[is.na(df_qtr$CLI_amount_kw)] <- 0
df_qtr$CLI_amount_mitigation[is.na(df_qtr$CLI_amount_mitigation)] <- 0
df_qtr$CLI_amount_adaptation[is.na(df_qtr$CLI_amount_adaptation)] <- 0


df_qtr$ebit <- df_qtr$ebit / 1000000

df_qtr <- df_qtr |>
  rename(total_lobby_quarter = amount) |>
  mutate(total_lobby_quarter = ifelse(is.na(total_lobby_quarter), 0, total_lobby_quarter / 1000))


# check
names(df_qtr)
glimpse(df_qtr)

# reduce to columns needed
df_qtr <- df_qtr |>
  select(
    -c(cusip_2023_4:cusip_2001_4)
  )

## write csv
# fwrite(exposure_orbis_lobbyview_long, "data/03_final/lobbying_df_quarterly_REVISE_NEW.csv")
# 
# # write rdata
# write_rds(exposure_orbis_lobbyview_long, "data/03_final/lobbying_df_quarterly_REVISE_NEW.rds")

arrow::write_parquet(df_qtr, "data/03_final/lobbying_df_quarterly_REVISE_NEW_altkeywords.parquet")

# 
# ## write csv
# fwrite(df_qtr, "data/03_final/lobbying_df_quarterly_REVISE_NEW_altkeywords.csv")
# 
# # write rdata
# write_rds(df_qtr, "data/03_final/lobbying_df_quarterly_REVISE_NEW_altkeywords.rds")

# exposure_orbis_lobbyview_long_qrt <- read_rds("data/03_final/lobbying_df_quarterly_REVISE.rds")

# df <- fread("data/03_final/lobbying_df_quarterly_REVISE_NEW_altkeywords.csv")
# 
# names(df)

### END










# Annual ------------------------------------------------------------------

# # Tokenize issue_text_clean into bigrams and count them
# lobbying_bigrams <- lobbying |>
#   unnest_tokens(bigram, issue_text_clean, token = "ngrams", n = 2) |>
#   group_by(filing_year, lob_id) |>
#   summarise(n_bigrams = n()) |>
#   ungroup()
# 
# # get to firm-year level
# lobbying_firmyear <- lobbying |>
#   group_by(filing_year, lob_id) |>
#   summarise(
#     report_uuid = paste(unique(report_uuid[!is.na(report_uuid) & report_uuid != ""]), collapse = "|"),
#     issue_code = paste(unique(issue_code[!is.na(issue_code) & issue_code != ""]), collapse = "|"),
#     gov_entity = paste(unique(gov_entity[!is.na(gov_entity) & gov_entity != ""]), collapse = "|"),
#     issue_text = paste(unique(issue_text[!is.na(issue_text) & issue_text != ""]), collapse = "|"),
#     issue_text_clean = paste(unique(issue_text_clean[!is.na(issue_text_clean) & issue_text_clean != ""]), collapse = "|"),
#     registrant_id = paste(unique(registrant_id[!is.na(registrant_id) & registrant_id != ""]), collapse = "|"),
#     registrant_name = paste(unique(registrant_name[!is.na(registrant_name) & registrant_name != ""]), collapse = "|"),
#     report_quarter_code = paste(unique(report_quarter_code[!is.na(report_quarter_code) & report_quarter_code != ""]), collapse = "|"),
#     amount = sum(amount, na.rm = TRUE),
#     n_mitigation_kw = sum(n_mitigation_kw, na.rm = TRUE),
#     n_adaptation_kw = sum(n_adaptation_kw, na.rm = TRUE),
#     n_words = sum(n_words, na.rm = TRUE)
#   ) |>
#   left_join(lobbying_bigrams, by = c("filing_year", "lob_id")) |>
#   mutate(
#     prop_mitigation_kw = n_mitigation_kw / n_bigrams,
#     prop_adaptation_kw = n_adaptation_kw / n_bigrams,
#     prop_keywords = (n_mitigation_kw + n_adaptation_kw) / n_bigrams
#   ) |>
#   ungroup()
# 
# glimpse(lobbying_firmyear)
# 
# # sample
# lobbying_firmyear |>
#   sample_n(100) |>
#   glimpse()
# 
#  # sample where n_mitigation_kw or n_adaptation_kw is > 0
#  lobbying_firmyear |>
#   filter(n_mitigation_kw > 0 | n_adaptation_kw > 0) |>
#   sample_n(100) |>
#   glimpse()
# 
# 
# ###
# 
# rm(list = setdiff(ls(), "lobbying_firmyear"))

#####