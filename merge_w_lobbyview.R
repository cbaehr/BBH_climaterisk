### Firms & Lobbying
### Data transformation

rm(list=ls())

# load packages
library(data.table)
library(tidyverse)

# set working directory
setwd("~/Dropbox (Princeton)/BBH/BBH1")


# load lobbying data
lobby_client <- fread("data/LOBBYING LobbyView/dataset___client_level.csv")
lobby_text <- fread("data/LOBBYING LobbyView/dataset___issue_text.csv")
lobby_issue <- fread("data/LOBBYING LobbyView/dataset___issue_level.csv")
lobby_bills <- fread("data/LOBBYING LobbyView/dataset___bills.csv")
lobby_report <- fread("data/LOBBYING LobbyView/dataset___report_level.csv")

# load firm climate risk data
# create one data-frame with yearly and quarterly exposure data + yearly control variables
firm_data <- fread("data/df_quarterly_fb.csv") |>
  mutate(gvkey = as.character(gvkey)) |>
  filter(!is.na(gvkey)) |>
  # add _q as identifier for quarterly data
  rename_at(vars(c(cc_expo_ew:ph_sent_ew,ccpos:phsent)), ~ paste0(., "_q")) |>
  # merge with yearly firm data
  left_join(fread("data/df_year_fb.csv") |>
              mutate(gvkey = as.character(gvkey)) |>
              filter(!is.na(gvkey)) |>
              # select only exposure data: control variables come from the quarterly dataframe
              select(isin:ph_sent_ew,ccpos:phsent) |>
              # add _y as identifier for yearly data
              rename_at(vars(-c(isin,year)), ~ paste0(., "_y")),
            by = c("isin","year"))


# Merging -----------------------------------------------------------------

# Baseline dataset: all reports from lobbying disclosure act separated by issue
df <- lobby_issue |>
  select(-c(2,3)) |>
  # merge with text of report
  left_join(lobby_text, by = "report_uuid") |>
  # merge with report data
  left_join(lobby_report, by = "report_uuid") |>
  # merge with client data
  left_join(lobby_client, by = "client_uuid") |>
  mutate(gvkey = as.character(gvkey)) |>
  rename(year = report_year) |>
  # merge with firm data
  left_join(firm_data, by = c("gvkey", "year", "report_quarter_code" = "quarter"))


# only observations with climate exposure data
cc <- df |>
  filter(!is.na(cc_expo_ew_y))

# write csv
fwrite(df, file="data/lobbying_df_fb.csv")
fwrite(cc, file="data/lobbying_df_reduced_fb.csv")




# Wide dataframe with binary issue code indicators ------------------------

# Note that this dataframe does neither contain report-issue text data 
# nor information on which institutions were lobbied
# because these information vary by issue within each report.

df_wide <- lobby_issue |>
  select(-c(issue_ordi,gov_entity)) |>
  distinct() |>
  mutate(issue_bin = 1) |>
  # from long to wide
  pivot_wider(names_from = issue_code, values_from = issue_bin, values_fill = 0) |>
  # merge with report data
  left_join(lobby_report, by = "report_uuid") |>
  # merge with client data
  left_join(lobby_client, by = "client_uuid") |>
  mutate(gvkey = as.character(gvkey)) |>
  rename(year = report_year) |>
  # merge with firm data
  left_join(firm_data, by = c("gvkey", "year", "report_quarter_code" = "quarter"))

# write csv
fwrite(df, file="data/lobbying_df_wide.csv")


### End