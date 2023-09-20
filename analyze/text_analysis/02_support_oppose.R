### Firms & Lobbying
### Text Analysis

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, openxlsx)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="vh4264" ) {setwd("/home/vh4264/bbh1/")}


# Load data
df <- fread("data//lobbying_df_reduced_fb.csv") |>
  # Remove observations with no text
  filter(issue_text != "") |>
  data.frame() |>
  mutate(quarter = paste0(year, "_", report_quarter_code))

# Transform Variables -----------------------------------------------------

df <- df |>
  mutate(
    # Dummy variable: climate issues
    CLI = ifelse(issue_code %in% c("ENV", "CAW", "ENG", "FUE"), 1, 0),
    # US headquarter
    us_dummy = ifelse(hqcountrycode == "US", 1, 0)) |>
  # Total annual lobbying (total dollars)
  group_by(gvkey, year) |>
  mutate(total_lobby = sum(amount_num)) |>
  ungroup()

# Code industry variable
df$industry <- df$bvd_sector
df <- df[which(df$industry!=""), ]
df$industry_year <- paste(df$industry, df$year)


# Only Climate Lobbying Reports -------------------------------------------

df <- df |> 
  filter(CLI == 1)



# Search for keyword ------------------------------------------------------

support <- df |>
  filter(str_detect(issue_text, "support")) |>
  select(gvkey, registrant_name, year, issue_code, issue_text, industry, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)

oppose <- df |>
  filter(str_detect(issue_text, "oppose")) |>
  select(gvkey, registrant_name, year, issue_code, issue_text, industry, op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y)


# Build excel sheets ------------------------------------------------------

create_excel <- function(x){
  
  name <- deparse(substitute(x))
  
  # create a workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")
  
  # write contents
  writeDataTable(wb, 1, x, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
  # ignore the warning message: it is redundant
  # https://github.com/ycphs/openxlsx/issues/342
  
  # save the output
  saveWorkbook(wb, paste0(name, "reports.xlsx"), overwrite = TRUE)
}

if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/data/issues_texts_support_oppose/")}

create_excel(support)
create_excel(oppose)