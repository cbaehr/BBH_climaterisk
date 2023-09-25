### Firms & Lobbying
### Analysis

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, modelsummary)

## Load data
#Lobbying analysis dataset
df <- fread("data/03_final/lobbying_df_wide_reduced.csv")

##Transform exposure variables *100 for easier interpretation
# Identify the subset of variables to be multiplied by 100
variables_to_multiply <- c("cc_expo_ew_y", "op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")

# Multiply the selected variables by 100
df <- df |>
  mutate(across(all_of(variables_to_multiply), ~ . * 100))

##Transform financial variables

# Identify the subset of variables to be divided by 1000000 to show in millions
variables_to_divide <- c("ebit", "at", "total_lobby")

# Divide the selected variables by 1000000
df <- df |>
  mutate(across(all_of(variables_to_divide), ~ . / 1000000))

#Create new variable that is ebit/assets
df$ebit_at <- df$ebit / df$at


##Summary statistics for all variables
datasummary((Overall = cc_expo_ew_y) + (Opportunity = op_expo_ew_y) + (Regulatory = rg_expo_ew_y) + (Physical = ph_expo_ew_y) + (`Earnings Before Interest and Taxes (EBIT) ($M)` = ebit) + (`EBIT/Total Assets (Productivity)` = ebit_at) + (`Total Lobbying Per Year($M)` = total_lobby) ~ Mean + SD + Min + P25 + P75 + Max + N,
            data = df,
            title = 'Summary Statistics',
            align = 'lccccccc',
            fmt = 3,
            output = 'latex')

##Exposure scores for top 10 industries
# select for relevant variables
df_ind <- df |> 
  # filter out empty bvd_sector
  filter(bvd_sector != "") |> 
  # select variables we need
  select(isin, year, bvd_sector, cc_expo_ew_y, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)


##calculate summary for exposure variables and identify top 10 
#Overall exposure
# Step 1: Calculate the mean of overall exposure
avg_overall <- df_ind |>
  group_by(bvd_sector) |>
  summarize(mean = mean(cc_expo_ew_y, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_overall <- avg_overall[order(-avg_overall$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_overall <- sorted_avg_overall[1:10, ]

ov_industries <- as.character(top10_avg_overall$bvd_sector)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_ov <- df_ind |> 
  # filter out empty bvd_sector
  filter(bvd_sector %in% ov_industries) |> 
  # select variables we need
  select(isin, year, bvd_sector, cc_expo_ew_y, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)

#data summary
overall <- datasummary((Industry = bvd_sector) ~ cc_expo_ew_y*(Mean + SD + Min + P25 + P75 + Max + N),
            data = df_ind_ov,
            title = 'Overall Exposure for Top 10 Industries',
            fmt = 3,
            output = 'latex')

#Opp exposure
# Step 1: Calculate the mean of opp exposure
avg_opp <- df_ind |>
  group_by(bvd_sector) |>
  summarize(mean = mean(op_expo_ew_y, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_opp <- avg_opp[order(-avg_opp$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_opp <- sorted_avg_opp[1:10, ]

opp_industries <- as.character(top10_avg_opp$bvd_sector)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_opp <- df_ind |> 
  # filter out empty bvd_sector
  filter(bvd_sector %in% opp_industries) |> 
  # select variables we need
  select(isin, year, bvd_sector, cc_expo_ew_y, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)

#data summary
datasummary((Industry = bvd_sector) ~ op_expo_ew_y*(Mean + SD + Min + P25 + P75 + Max + N),
                       data = df_ind_opp,
                       title = 'Opportunity Exposure by Industry',
                       fmt = 3,
                      output = 'latex')

#Reg exposure
# Step 1: Calculate the mean of opp exposure
avg_reg <- df_ind |>
  group_by(bvd_sector) |>
  summarize(mean = mean(rg_expo_ew_y, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_reg <- avg_reg[order(-avg_reg$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_reg <- sorted_avg_reg[1:10, ]

reg_industries <- as.character(top10_avg_reg$bvd_sector)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_reg <- df_ind |> 
  # filter out empty bvd_sector
  filter(bvd_sector %in% reg_industries) |> 
  # select variables we need
  select(isin, year, bvd_sector, cc_expo_ew_y, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)

datasummary((Industry = bvd_sector) ~ rg_expo_ew_y*(Mean + SD + Min + P25 + P75 + Max + N),
                       data = df_ind_reg,
                       title = 'Regulatory Exposure',
                       fmt = 3, 
                   output = 'latex')

#Phy exposure
# Step 1: Calculate the mean of phy exposure
avg_ph <- df_ind |>
  group_by(bvd_sector) |>
  summarize(mean = mean(ph_expo_ew_y, na.rm = TRUE))

# Step 2: Sort the dataframe based on the mean values in descending order
sorted_avg_ph <- avg_ph[order(-avg_ph$mean), ]

# Step 3: Select the top 10 rows from the sorted dataframe
top10_avg_ph <- sorted_avg_ph[1:10, ]

ph_industries <- as.character(top10_avg_ph$bvd_sector)


#Step 4: Make datasummary table for these 10 industries 
# select for relevant variables
df_ind_ph <- df_ind |> 
  # filter out empty bvd_sector
  filter(bvd_sector %in% ph_industries) |> 
  # select variables we need
  select(isin, year, bvd_sector, cc_expo_ew_y, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)

datasummary((Industry = bvd_sector) ~ ph_expo_ew_y*(Mean + SD + Min + P25 + P75 + Max + N),
                       data = df_ind_ph,
                       title = 'Physical Exposure by Industry',
                       fmt = 3, 
                   output = 'latex')

##
