### Firms & Lobbying
### Analysis

rm(list=ls())


# Load packages
pacman::p_load(tidyverse, data.table, modelsummary)

# Load data
df <- fread("data/03_final/lobbying_df_wide_reduced.csv")


##Summary statistics for control variables 
datasummary((`Earnings Before Interest and Taxes ($M)` = ebit) + (`Total Assets ($M)` = at) + (`Total Lobbying Per Year(#)` = total_lobby) ~ Mean + SD + P25 + P75 + N,
            data = cc_wide,
            title = 'Control Variables Summary Statistics',
            align = 'lccccc',
            fmt = 0,
            output = 'latex')