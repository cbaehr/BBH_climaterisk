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

insp <- df |> select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin)
# worked
