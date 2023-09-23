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


##Summary statistics for exposure variables
datasummary((Overall = cc_expo_ew_y) + (Opportunity = op_expo_ew_y) + (Regulatory = rg_expo_ew_y) + (Physical = ph_expo_ew_y) + (`Earnings Before Interest and Taxes (EBIT) ($M)` = ebit) + (`EBIT/Total Assets (Productivity)` = ebit_at) + (`Total Lobbying Per Year($M)` = total_lobby) ~ Mean + SD + Min + P25 + P75 + Max + N,
            data = df,
            title = 'Summary Statistics',
            align = 'lccccccc',
            fmt = 3,
            output = 'latex')


##Summary statistics for control variables 
datasummary((`EBIT ($M)` = ebit) + (`EBIT/Total Assets (Productivity)` = ebit_at) + (`Total Lobbying Per Year($M)` = total_lobby) ~ Mean + SD + P25 + P75 + N,
            data = df,
            title = 'Control Variables',
            align = 'lccccc',
            fmt = 2,
            output = 'latex')

##Exposure scores for top 10 industries
# select for relevant variables
df_ind <- df |> 
  # filter out empty bvd_sector
  filter(bvd_sector != "") |> 
  # select variables we need
  select(isin, year, bvd_sector, cc_expo_ew_y, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)


#calculate summary for exposure variables
#note - need to figure out how to order by largest to smallest and how to only include the top 10 by industry
overall <- datasummary((Industry = bvd_sector) ~ cc_expo_ew_y*(Mean + SD + N),
            data = df_ind,
            title = 'Overall Exposure by Industry',
            fmt = 3)

opp <- datasummary((Industry = bvd_sector) ~ opexpo*(Mean + SD + N),
                       data = df2,
                       title = 'Opportunity Exposure by Industry',
                       fmt = 3)

reg <- datasummary((Industry = bvd_sector) ~ rgexpo*(Mean + SD + N),
                       data = df2,
                       title = 'Regulatory Exposure by Industry',
                       fmt = 3)

phy <- datasummary((Industry = bvd_sector) ~ phexpo*(Mean + SD + N),
                       data = df2,
                       title = 'Physical Exposure by Industry',
                       fmt = 3)
