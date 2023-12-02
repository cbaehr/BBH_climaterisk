### Firms & Lobbying
### Analysis

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, modelsummary)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


## Load data
#Lobbying analysis dataset
df <- read_rds(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.rds")



# Number of firms & years -------------------------------------------------

## Number of firms
df |> distinct(gvkey) |> count() # 11826

## Years analyzed
df |> 
  filter(!is.na(op_expo_ew)) |>
  distinct(year) |>
  pull() # 2000-2020

df |> 
  filter(!is.na(CLI_quarter)) |>
  distinct(year) |>
  pull()


# Table 2: Auto Stats -----------------------------------------------------

# BMW & YearQrt 2020_3
df |> filter(str_detect(conm, "BAYERI")) |> filter(year == "2019") |>
  select(op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  # round to two digits
  mutate(across(all_of(c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")), ~ round(., 2)))

# GM & YearQrt 2020_4
df |> filter(str_detect(conm, "GENERAL MOTORS COMPANY")) |> filter(year == "2019") |>
  select(op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  # round to two digits
  mutate(across(all_of(c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")), ~ round(., 2)))

# Toyota & YearQrt 2020_3
df |> filter(str_detect(conm, "TOYOTA MOTOR")) |> filter(year == "2019") |>
  select(op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  # round to two digits
  mutate(across(all_of(c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")), ~ round(., 2)))



# Summary stats -----------------------------------------------------------

# ##Transform exposure variables *100 for easier interpretation
# # Identify the subset of variables to be multiplied by 100
# variables_to_multiply <- c("cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")
# 
# # Multiply the selected variables by 100
# df <- df |>
#   mutate(across(all_of(variables_to_multiply), ~ . * 100))
# 
# ##Transform financial variables
# 
# # Identify the subset of variables to be divided by 1000000 to show in millions
# variables_to_divide <- c("ebit", "at", "total_lobby")
# 
# # Divide the selected variables by 1000000
# df <- df |>
#   mutate(across(all_of(variables_to_divide), ~ . / 1000000))
# 
# #Create new variable that is ebit/assets
# df$ebit_at <- df$ebit / df$at


##Summary statistics for all variables
datasummary(
  (`Climate Lobbying Occurrence` = CLI_quarter) + (`Climate Lobbying Expenditure` = CLI_amount_quarter) + 
    (Opportunity = op_expo_ew) + (Regulatory = rg_expo_ew) + (Physical = ph_expo_ew) + 
    (`Earnings Before Interest and Taxes (EBIT) ($M)` = ebit) + (`EBIT/Total Assets (Productivity)` = ebit_at) + 
    (`Total Lobbying Per Quarter($M)` = total_lobby_quarter) ~ Mean + SD + Min + Max + N,
  data = df,
  title = 'Summary Statistics',
  align = 'lccccc',
  fmt = 3,
  output = 'latex'
)

##Exposure scores for top 10 industries
# select for relevant variables
df_ind <- df |> 
  # filter out empty industry
  filter(industry != "") |> 
  # select variables we need
  select(isin, year, industry, cc_expo_ew, op_expo_ew, rg_expo_ew, ph_expo_ew)


##calculate summary for exposure variables and identify top 10 
# #Overall exposure
# # Step 1: Calculate the mean of overall exposure
# avg_overall <- df_ind |>
#   group_by(industry) |>
#   summarize(mean = mean(cc_expo_ew, na.rm = TRUE))
# 
# # Step 2: Sort the dataframe based on the mean values in descending order
# sorted_avg_overall <- avg_overall[order(-avg_overall$mean), ]
# 
# # Step 3: Select the top 10 rows from the sorted dataframe
# top10_avg_overall <- sorted_avg_overall[1:10, ]
# 
# ov_industries <- as.character(top10_avg_overall$industry)
# 
# 
# #Step 4: Make datasummary table for these 10 industries 
# # select for relevant variables
# df_ind_ov <- df_ind |> 
#   # filter out empty industry
#   filter(industry %in% ov_industries) |> 
#   # select variables we need
#   select(isin, year, industry, cc_expo_ew, op_expo_ew, rg_expo_ew, ph_expo_ew)
# 
# #data summary
# datasummary((Industry = industry) ~ cc_expo_ew * (Mean + SD + Min + Max + N),
#             data = df_ind_ov,
#             title = 'Overall Exposure for Top 10 Industries',
#             fmt = 3,
#             output = 'latex'
# )

#Opp exposure
# Step 1: Calculate the mean of opp exposure
avg_opp <- df_ind |>
  group_by(industry) |>
  summarize(mean = mean(op_expo_ew, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_opp <- avg_opp[order(-avg_opp$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_opp <- sorted_avg_opp[1:10, ]

opp_industries <- as.character(top10_avg_opp$industry)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_opp <- df_ind |> 
  # filter out empty industry
  filter(industry %in% opp_industries) |> 
  # select variables we need
  select(isin, year, industry, cc_expo_ew, op_expo_ew, rg_expo_ew, ph_expo_ew)

#data summary
datasummary((Industry = industry) ~ op_expo_ew*(Mean + SD + Min + Max + N),
                       data = df_ind_opp,
                       title = 'Opportunity Exposure by Industry',
                       fmt = 3,
                      output = 'latex')

#Reg exposure
# Step 1: Calculate the mean of opp exposure
avg_reg <- df_ind |>
  group_by(industry) |>
  summarize(mean = mean(rg_expo_ew, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_reg <- avg_reg[order(-avg_reg$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_reg <- sorted_avg_reg[1:10, ]

reg_industries <- as.character(top10_avg_reg$industry)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_reg <- df_ind |> 
  # filter out empty industry
  filter(industry %in% reg_industries) |> 
  # select variables we need
  select(isin, year, industry, cc_expo_ew, op_expo_ew, rg_expo_ew, ph_expo_ew)

datasummary((Industry = industry) ~ rg_expo_ew*(Mean + SD + Min + Max + N),
                       data = df_ind_reg,
                       title = 'Regulatory Exposure',
                       fmt = 3, 
                   output = 'latex')

#Phy exposure
# Step 1: Calculate the mean of phy exposure
avg_ph <- df_ind |>
  group_by(industry) |>
  summarize(mean = mean(ph_expo_ew, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_ph <- avg_ph[order(-avg_ph$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_ph <- sorted_avg_ph[1:10, ]

ph_industries <- as.character(top10_avg_ph$industry)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_ph <- df_ind |> 
  # filter out empty industry
  filter(industry %in% ph_industries) |> 
  # select variables we need
  select(isin, year, industry, cc_expo_ew, op_expo_ew, rg_expo_ew, ph_expo_ew)

datasummary((Industry = industry) ~ ph_expo_ew*(Mean + SD + Min + Max + N),
                       data = df_ind_ph,
                       title = 'Physical Exposure by Industry',
                       fmt = 3, 
                   output = 'latex')

# END