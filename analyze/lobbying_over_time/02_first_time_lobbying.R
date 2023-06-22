### Firms & Climate Lobbying over time
### Descriptive

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor)


# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}



# load data
df <- fread("data/lobbying_df_wide.csv")



# Climate change lobbying over time ---------------------------------------

# Binary variable: climate issues
df <- df |>
  mutate(CLI = ifelse(ENV == 1 |
                        CAW == 1 |
                        ENG == 1 |
                        FUE == 1,
                      1, 0))

df <- df |>
  mutate(year_quarter = paste0(year,"_",report_quarter_code))



# Code first entry --------------------------------------------------------

df <- df |>
  # get number of reports by firm in given year
  group_by(year_quarter, gvkey) |>
  mutate(
    n_reports_q = ifelse(!is.na(gvkey), n(), NA),
    n_climate_reports_q = ifelse(!is.na(gvkey), sum(CLI), NA),
    climate_report_q_bin = ifelse(n_climate_reports_q > 0, 1, 0)
  ) |>
  ungroup() |>
  arrange(gvkey, year_quarter)

insp <- df |> select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin) |>
  distinct()
# worked

# Next: code first time lobbying
# Get firms that lobby on climate at any point
firms_cl <- df |>
  select(gvkey, climate_report_q_bin) |>
  filter(climate_report_q_bin == 1) |>
  distinct(gvkey) |>
  pull()

df <- df |>
  group_by(gvkey) |>
  mutate(
    firms_climate = ifelse(gvkey %in% firms_cl, 1, 0),
    year_quarter_first_climate = ifelse(firms_climate == 1, 
                                      first(year_quarter[climate_report_q_bin == 1]),
                                      NA),
    first_climate_report_q_bin = ifelse(year_quarter == year_quarter_first_climate, 1, 0)
    )

# insp <- df |> 
#   select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
#                      firms_climate, year_quarter_first_climate, first_climate_report_q_bin) |>
#   distinct()
# worked



# Code climate lobbying after not having lobbied during the previo --------

df <- df %>%
  group_by(gvkey) %>%
  mutate(first_climate_report_q_bin = ifelse(climate_report_q_bin == 1 & lag(climate_report_q_bin, default = 0) == 0, 1, 0)) %>%
  ungroup()
