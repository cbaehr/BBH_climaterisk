### Build placebo df

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haven, tidytext, stopwords)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

# load lobbying data
# lobby_client_old <- fread("data/01_raw/lobbyview/dataset___client_level.csv")
lobby_client <- fread("data/01_raw/lobbyview_20250103/clients_codebook/clients.csv")

# # lobby_text_old <- fread("data/01_raw/lobbyview/dataset___issue_text.csv")
# lobby_text <- fread("data/01_raw/lobbyview_20250103/issue_text_codebook/issue_text.csv")

# glimpse(lobby_text)

# lobby_issue_old <- fread("data/01_raw/lobbyview/dataset___issue_level.csv")
lobby_issue <- fread("data/01_raw/lobbyview_20250103/issues_codebook/issues.csv")

# glimpse(lobby_issue)

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

# Look into lobby_issue ---------------------------------------------------

# glimpse(lobby_issue)

# # duplicates?
# lobby_issue |>
#   count(report_uuid, general_issue_code, gov_entity) |>
#   filter(n > 1) |>
#   arrange(desc(n))

# nrow(lobby_issue)
# # 2,559,161

# distinct
lobby_issuedistinct <- lobby_issue |>
  distinct() |> 
  ungroup()

# nrow(lobby_issuedistinct)
# # 2,558,971
# # some duplicates


## to wide ---------------------------------------------------------------

# assign 1 if lobbying on issue, 0 otherwise
lobby_issuedistinct_wide <- lobby_issuedistinct |>
  select(-issue_ordinal_position) |>
  mutate(lobbying = 1) |>
  pivot_wider(
    names_from = general_issue_code,
    values_from = lobbying,
    id_cols = c(report_uuid, government_entity_ids, gov_entity),
    values_fn = list(lobbying = max),
    values_fill = list(lobbying = 0)
  )

# glimpse(lobby_issuedistinct_wide)

# Combine ---------------------------------------------------------------


# glimpse(lobby_report)
# glimpse(lobby_issuedistinct_wide)
# glimpse(lobby_client)

# which columns are in both?
intersect(names(lobby_report), names(lobby_issuedistinct_wide))
# only report_uuid

intersect(names(lobby_client), names(lobby_issuedistinct_wide))
# none


lobbying <- lobby_report |>
  left_join(
    lobby_issuedistinct_wide,
    by = c("report_uuid")
  ) |>
  left_join(
    lobby_client,
    by = c("lob_id")
  )

# glimpse(lobbying)


# transform some stuff
lobbying <- lobbying |>
  mutate(
    report_quarter_code = as.character(report_quarter_code),
    amount = ifelse(is.na(amount), 0, amount),
    gov_entity = ifelse(gov_entity == "", NA, gov_entity)
  )

# glimpse(lobbying)



# Half-year reports --------------------------------------------------------

# get reports for half-year
lobbying_half <- lobbying |>
  filter(str_detect(report_quarter_code, "12|34")) |>
  separate_rows(report_quarter_code, sep = "") |>
  filter(report_quarter_code %in% c("1", "2", "3", "4")) |>
  mutate(amount = amount / 2)


# Combine quarterly and half-yearly data -----------------------------------

lobbying_corrected <- lobbying |>
  filter(!str_detect(report_quarter_code, "12|34")) |>
  bind_rows(lobbying_half)

# glimpse(lobbying_corrected)

# Assign 0 to NAs in the issue columns
lobbying_corrected <- lobbying_corrected |>
  mutate(across(
    c(ENV:MON),
    ~ifelse(is.na(.), 0, .)
  ))

# glimpse(lobbying_corrected)


# Aggregate to firm-year-quarter level -------------------------------------

# glimpse(lobbying_corrected)

lobbying_firmqtr <- lobbying_corrected %>%
  group_by(lob_id, filing_year, report_quarter_code) %>%
  summarise(
    report_uuid = paste(unique(report_uuid[!is.na(report_uuid) & report_uuid != ""]), collapse = "|"),
    gov_entity = paste(unique(gov_entity[!is.na(gov_entity) & gov_entity != ""]), collapse = "|"),
    amount = sum(amount, na.rm = TRUE),
    across(
      ENV:MON,  # Assuming these are the issue columns
      max
    #   ,
    #   .names = "max_{col}"
    )
  )

# # remove max_ prefix from column names
# names(lobbying_firmqtr) <- gsub("max_", "", names(lobbying_firmqtr))


glimpse(lobbying_firmqtr)


# Duplicates?
lobbying_firmqtr |>
  count(lob_id, filing_year, report_quarter_code) |>
  filter(n > 1) |>
  arrange(desc(n))
# none

# remove empty lobbying_firmqtr
lobbying_firmqtr <- lobbying_firmqtr |>
  filter(lob_id != "")


# Distribute amount to each issue -----------------------------------------

# get issue columns names from ENV to MON
issue_columns <- names(lobbying_firmqtr)[which(names(lobbying_firmqtr)=="ENV"):which(names(lobbying_firmqtr)=="MON")]

lobbying_firmqtr_dist <- lobbying_firmqtr %>%
  rowwise() %>%
  mutate(
    # Calculate the number of issues with value 1
    n_issues = sum(c_across(all_of(issue_columns)) == 1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    # Distribute the amount to each issue
    across(all_of(issue_columns), ~ ifelse(. == 1, amount / n_issues, 0), .names = "{.col}_amount")
  )

# glimpse(lobbying_firmqtr_dist)

# # inspect
# lobbying_firmqtr_dist |>
#   filter(n_issues > 1) |>
#   sample_n(100) |>
#   glimpse()


# View(lobbying_firmqtr_dist |>
#   filter(lob_id == "2a3329a0-b426-50ff-bc6e-056583cf58e1" & filing_year == 2004 & report_quarter_code == "2")
# )

# # duplicates?
# lobbying_firmqtr_dist |>
#   count(lob_id, filing_year, report_quarter_code) |>
#   filter(n > 1) |>
#   arrange(desc(n))
# # none


# Merge w/ orbis - exposure data ------------------------------------------

rm(list = setdiff(ls(), c("lobbying_firmqtr_dist")))

# Load exposure & orbis
exposure_orbis_lobby_long <- fread("data/02_processed/exposure_orbis_client_quarter_long_REVISE_NEW.csv")

# glimpse(exposure_orbis_lobby_long)
# glimpse(lobbying_firmqtr_dist)

df_qtr <- exposure_orbis_lobby_long |>
  left_join(
    lobbying_firmqtr_dist |>
      mutate(
        report_quarter_code = as.numeric(report_quarter_code)
      ),
    by = c("lob_id", "year" = "filing_year", "qtr" = "report_quarter_code")
  )

# glimpse(df_qtr)


# sum(duplicated(df_qtr[, c("isin", "year", "qtr")]))
# # none
# sum(duplicated(df_qtr[, c("gvkey", "year", "qtr")]))
# # 14,076
# sum(duplicated(df_qtr[, c("bvdid", "year", "qtr")]))
# # none
# sum(duplicated(df_qtr[, c("lob_id", "year", "qtr")]) & df_qtr$lob_id!=(-1)) #good
# # none


df_qtr <- df_qtr |>
  mutate(
    us_dummy = ifelse(hqcountrycode == "US", 1, 0),
    industry = bvdsector,
    industry_year = paste(industry, year),
    ebit = as.numeric(P_L_b4tax_usd),
    at = as.numeric(total_assets_usd),
    ebit_at = ebit / at
  )

## few cases with zero denominator
invalid_orbis <- which(df_qtr$ebit_at %in% c(-Inf, Inf))

df_qtr <- df_qtr |>
  mutate(
    at = ifelse(is.na(at), 0, at),
    ebit_at = ifelse(is.na(ebit_at), 0, ebit_at)
  )


# Convert issue lobbying columns to 0 if NA and some other transformations
issue_columns <- names(df_qtr)[which(names(df_qtr)=="ENV"):which(names(df_qtr)=="MON")]
issue_columns_amount <- paste0(issue_columns, "_amount")

df_qtr <- df_qtr %>%
  rename(
    total_lobby_quarter = amount
  ) %>%
  mutate(
    across(
      all_of(issue_columns),
      ~ifelse(is.na(.), 0, .)
    ),
    across(
      all_of(issue_columns_amount),
      ~ifelse(is.na(.), 0, .)
    ),
    n_issues = ifelse(is.na(n_issues), 0, n_issues),
    ebit = ebit / 1000000,
    total_lobby_quarter = ifelse(is.na(total_lobby_quarter), 0, total_lobby_quarter / 1000)
  )

# # check
# names(df_qtr)
# glimpse(df_qtr)


df_qtr <- data.frame(df_qtr)

glimpse(df_qtr)

#Normalize variables for interpretation 

## continuous variables in regression models
df_wide_cont_vars <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", 
                       "cc_sent_ew", "op_sent_ew", "rg_sent_ew", "ph_sent_ew",
                       "cc_pos_ew", "op_pos_ew", "rg_pos_ew", "ph_pos_ew",
                       "cc_neg_ew", "op_neg_ew", "rg_neg_ew", "ph_neg_ew",
                       "cc_risk_ew", "op_risk_ew", "rg_risk_ew", "ph_risk_ew")
## pull from main data
df_wide_cont <- df_qtr[, df_wide_cont_vars]
## rescale to standard normal
df_wide_cont <- data.frame(apply(df_wide_cont, 2, as.numeric))
df_wide_cont <- scale(df_wide_cont)
## slot back into main df_wide
df_qtr[, df_wide_cont_vars] <- df_wide_cont


# Transform some to numeric
df_qtr <- df_qtr %>%
  mutate(
    across(c(ends_with("_ew"), ends_with("_amount"),
             "total_assets_usd", "n_employees", "operating_rev_usd", "P_L_b4tax_usd"
             ), as.numeric)
    )

# List of additional variables to standardize
additional_vars <- c("ebit", "ebit_at", "us_dummy", "total_lobby_quarter")

# Standardize and create new columns with '_scaled' suffix
for (var in additional_vars) {
  df_qtr[[paste0(var, "_scaled")]] <- as.numeric(scale(df_qtr[[var]], center = TRUE, scale = TRUE))
}

names(df_qtr)

# check
table(df_qtr$cc_expo_ew, useNA = "ifany") # looks fine
table(df_qtr$op_expo_ew, useNA = "ifany") # looks fine
table(df_qtr$total_assets_usd, useNA = "ifany") # looks fine
table(df_qtr$n_employees, useNA = "ifany") # looks fine
table(df_qtr$operating_rev_usd, useNA = "ifany") # looks fine
table(df_qtr$P_L_b4tax_usd, useNA = "ifany") # looks fine
table(df_qtr$total_lobby_quarter, useNA = "ifany") # looks fine

# # Drop quarter "2020_2" - first covid quarters
# df <- df %>%
#   filter(!(yearqtr %in% c("2020_2", "2020_3", "2020_4", 
#                          "2021_1", "2021_2", "2021_3", "2021_4")))

## write csv
fwrite(df_qtr, "data/03_final/lobbying_df_quarterly_REVISE_NEW_placebos.csv")

# write rdata
write_rds(df_qtr, "data/03_final/lobbying_df_quarterly_REVISE_NEW_placebos.rds")


# exposure_orbis_lobbyview_long_qrt <- read_rds("data/03_final/lobbying_df_quarterly_REVISE.rds")


### END











# # Annual ------------------------------------------------------------------

# # Tokenize issue_text_clean into bigrams and count them
# lobbying_bigrams <- lobbying |>
#   unnest_tokens(bigram, issue_text_clean, token = "ngrams", n = 2) |>
#   group_by(filing_year, lob_id) |>
#   summarise(n_bigrams = n()) |>
#   ungroup()

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

# glimpse(lobbying_firmyear)

# # sample
# lobbying_firmyear |>
#   sample_n(100) |>
#   glimpse()

#  # sample where n_mitigation_kw or n_adaptation_kw is > 0
#  lobbying_firmyear |>
#   filter(n_mitigation_kw > 0 | n_adaptation_kw > 0) |>
#   sample_n(100) |>
#   glimpse()


# ###

# rm(list = setdiff(ls(), "lobbying_firmyear"))

#####