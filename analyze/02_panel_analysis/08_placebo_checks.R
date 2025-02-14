# Check which placebos are actual placebos

# Some issues are not placebos because they are related to climate lobbying.
# We do three things to find out which issues are not placebos:
# 1. Check which issues firms lobby on simultaneously when they lobby on climate
# We do this first outside of the dataset to avoid any look-ahead bias.
# 2. We check within the whole dataset which issues are related to climate lobbying.
# 3. We check which issues are lobbied on when climate keywords are present.

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haven, tidytext, stopwords, haschaR)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}


# Load LobbyView data
lobby_text <- fread("data/01_raw/lobbyview_20250103/issue_text_codebook/issue_text.csv")

lobby_issue <- fread("data/01_raw/lobbyview_20250103/issues_codebook/issues.csv")

lobby_report <- fread("data/01_raw/lobbyview_20250103/reports_codebook/reports.csv")

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


# Load final df to see which years are in the sample
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

names(df)

# Years in sample
df |>
  count(year, qtr) |>
  arrange(year, qtr)

# first observation is 2001, Q1
# Hence, out of sample is 1999 - 2000, Q4

rm(df)

# Build lobbying df

glimpse(lobby_issue)

lobby_issuedistinct <- lobby_issue |>
  distinct() |>
  ungroup()

lobby_textdistinct <- lobby_text |>
  distinct() |>
  ungroup() |>
  mutate(issue_ordinal_position = as.numeric(issue_ordinal_position))

glimpse(lobby_issuedistinct)
glimpse(lobby_textdistinct)


# Merge
lobby_issuetext <- lobby_issuedistinct |>
  left_join(lobby_textdistinct, by = c("report_uuid", "general_issue_code", "issue_ordinal_position"))


# Stack together at report_uuid - general_issue_code level
lobby_text_summarized <- lobby_issuetext %>%
  group_by(report_uuid) %>%
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

## remove special escape characters
lobby_text_summarized$issue_text_clean <- gsub("[^A-z0-9. ]", " ", lobby_text_summarized$issue_text)
lobby_text_summarized$issue_text_clean <- gsub("`|\\^|\\[|\\]|\\\\|_", " ", lobby_text_summarized$issue_text_clean)
lobby_text_summarized$issue_text_clean <- gsub("\\s+", " ", lobby_text_summarized$issue_text_clean) # remove redundant spaces


# Define stopwords and other non-essential words
stopwords <- c(stopwords("en", "snowball"), stopwords("en", "marimo"), stopwords("en", "nltk"), stopwords("en", "stopwords-iso")) |>
  unique() |>
  sort()

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
      group_by(report_uuid) |>
      summarise(issue_text_clean_2 = paste(word, collapse = " ")),
    by = c("report_uuid")
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
    n_adaptation_kw = str_count(tolower(issue_text_clean_2), paste(tolower(adaptation_kw), collapse = "|"))
  )

glimpse(lobby_text_joined)

# NAs to 0
lobby_text_joined <- lobby_text_joined |>
  mutate(
    n_mitigation_kw = ifelse(is.na(n_mitigation_kw), 0, n_mitigation_kw),
    n_adaptation_kw = ifelse(is.na(n_adaptation_kw), 0, n_adaptation_kw)
  )

# remove text to save memory
lobby_text_joined <- lobby_text_joined |>
  select(-c(issue_text_clean_2, issue_text, issue_text_clean))

glimpse(lobby_text_joined)
# which files are left?
ls()

# keep only lobby_text_joined and lobby_report
rm(list = ls()[!ls() %in% c("lobby_text_joined", "lobby_report")])


# merge
glimpse(lobby_report)

lobby_text_joined <- lobby_text_joined |>
  left_join(lobby_report, by = "report_uuid")

glimpse(lobby_text_joined)

# Half-year reports --------------------------------------------------------

# get reports for half-year
lobbying_half <- lobby_text_joined |>
  filter(str_detect(report_quarter_code, "12|34")) |>
  separate_rows(report_quarter_code, sep = "") |>
  filter(report_quarter_code %in% c("1", "2", "3", "4")) |>
  mutate(amount = amount / 2)


# Combine quarterly and half-yearly data -----------------------------------

lobbying_corrected <- lobby_text_joined |>
  mutate(report_quarter_code = as.character(report_quarter_code)) |>
  filter(!str_detect(report_quarter_code, "12|34")) |>
  bind_rows(lobbying_half)

glimpse(lobbying_corrected |> filter(filing_year < 2001))

# Define climate-related issue codes
climate_issues <- c("ENV", "CAW", "ENG", "FUE")

# Filter for out-of-sample period (1999-2000)
climate_cooccurrence_oos <- lobbying_corrected |>
  filter(filing_year < 2001) |>
  # Split issue_codes into separate rows
  separate_rows(issue_code, sep = "\\|") |>
  # Create indicator for climate-related reports
  group_by(report_uuid) |>
  mutate(
    is_climate_report = any(issue_code %in% climate_issues)
  ) |>
  # Keep only reports that have at least one climate issue
  filter(is_climate_report == TRUE) |>
  # Count co-occurrence of other issues
  ungroup() |>
  count(issue_code) |>
  # Calculate percentage of climate reports that include each issue
  mutate(
    pct = n / sum(n) * 100
  ) |>
  arrange(desc(n)) |>
  filter(!(issue_code %in% climate_issues))

# View results
print(climate_cooccurrence_oos, n = Inf)

# 2. Now same for entire sample --------------------------------------------

climate_cooccurrence_all <- lobbying_corrected |>
  # Split issue_codes into separate rows
  separate_rows(issue_code, sep = "\\|") |>
  # Create indicator for climate-related reports
  group_by(report_uuid) |>
  mutate(
    is_climate_report = any(issue_code %in% climate_issues)
  ) |>
  # Keep only reports that have at least one climate issue
  filter(is_climate_report == TRUE) |>
  # Count co-occurrence of other issues
  ungroup() |>
  count(issue_code) |>
  # Calculate percentage of climate reports that include each issue
  mutate(
    pct = n / sum(n) * 100
  ) |>
  arrange(desc(n)) |>
  filter(!(issue_code %in% climate_issues))

# View results
print(climate_cooccurrence_all, n = Inf)


# 3. Check which issues are lobbied on when climate keywords are present --------------------------------------------

climate_cooccurrence_kw <- lobbying_corrected |>
  filter(n_mitigation_kw > 0 | n_adaptation_kw > 0) |>
  # Split issue_codes into separate rows
  separate_rows(issue_code, sep = "\\|") |>
  # Create indicator for climate-related reports
  group_by(report_uuid) |>
  mutate(
    is_climate_report = any(issue_code %in% climate_issues)
  ) |>
  # Keep only reports that have at least one climate issue
  filter(is_climate_report == TRUE) |>
  # Count co-occurrence of other issues
  ungroup() |>
  count(issue_code) |>
  # Calculate percentage of climate reports that include each issue
  mutate(
    pct = n / sum(n) * 100
  ) |>
  arrange(desc(n)) |>
  filter(!(issue_code %in% climate_issues))

# View results
print(climate_cooccurrence_kw, n = Inf)


# Combine into one df -----------------------------------------------------

# Calculate mean pct across methods for each issue
climate_cooccurrence <- bind_rows(
  climate_cooccurrence_oos |> mutate(type = "Out of sample (< 2001)"),
  climate_cooccurrence_all |> mutate(type = "Total sample (1999-2023)"),
  climate_cooccurrence_kw |> mutate(type = "Climate keywords")
) |>
  group_by(issue_code) |>
  mutate(mean_pct = mean(pct, na.rm = TRUE)) |>
  ungroup()

# Get order of issues based on out of sample percentages
issue_order <- climate_cooccurrence |>
  arrange(desc(mean_pct)) |>
  pull(issue_code) |>
  unique()

# Plot --------------------------------------------------------------------
climate_cooccurrence |>
  mutate(
    issue_code = factor(issue_code, levels = rev(issue_order)),
    type = factor(type, levels = c("Out of sample (< 2001)", "Total sample (1999-2023)", "Climate keywords"))
  ) |>
  ggplot(aes(y = issue_code, x = pct)) +
  geom_bar(stat = "identity") +
  facet_wrap(~type) +
  theme_hanno() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Percentage of co-occurrence with climate lobbying", 
    y = NULL
  )

# Save plot
ggsave("results/figures/descriptives/climate_cooccurrence.pdf", width = 8.5, height = 11)



