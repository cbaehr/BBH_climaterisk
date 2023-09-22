### Firms & Lobbying
### Analysis

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, modelsummary)

## Load data
#Lobbying analysis dataset
df <- fread("data/03_final/lobbying_df_wide_reduced.csv")

#Exposure dataset
# load data
df2 <- fread("data/02_processed/exposure_year.csv", colClasses = c("sic"="character"))


##Summary statistics for exposure variables
datasummary((Overall = cc_expo_ew_y) + (Opportunity = op_expo_ew_y) + (Regulatory = rg_expo_ew_y) + (Physical = ph_expo_ew_y) ~ Mean + SD + P25 + P75 + N,
            data = df,
            title = 'Climate Change Exposure (Annual)',
            align = 'lccccc',
            fmt = 2,
            output = 'latex')


##Summary statistics for control variables 
datasummary((`Earnings Before Interest and Taxes ($M)` = ebit) + (`Total Assets ($M)` = at) + (`Total Lobbying Per Year($)` = total_lobby) ~ Mean + SD + N,
            data = df,
            title = 'Control Variables',
            align = 'lccccc',
            fmt = 2,
            output = 'latex')

##Exposure scores for top 10 industries
# select for relevant variables
df2 <- df2 |> 
  # filter out empty bvd_sector
  filter(bvd_sector != "") |> 
  # select variables we need
  select(isin, year, bvd_sector, ccexp, opexpo, rgexpo, phexpo)


#calculate summary for exposure variables
#note - need to figure out how to order by largest to smallest and how to only include the top 10 by industry
overall <- datasummary((Industry = bvd_sector) ~ ccexp*(Mean + SD + N),
            data = df2,
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
