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





