### Firms & Lobbying
### Data transformation

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse)

# set working directory
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

# load lobbying data
lobby_client <- fread("data/01_raw/lobbyview/dataset___client_level.csv")
lobby_text <- fread("data/01_raw/lobbyview/dataset___issue_text.csv")
lobby_issue <- fread("data/01_raw/lobbyview/dataset___issue_level.csv")
lobby_bills <- fread("data/01_raw/lobbyview/dataset___bills.csv")
lobby_report <- fread("data/01_raw/lobbyview/dataset___report_level_FIXED.csv")

# load firm climate risk data
# create one data-frame with yearly and quarterly exposure data + yearly control variables
# firm_data <- fread("data/df_quarterly_fb.csv") |>
#   mutate(gvkey = as.character(gvkey)) |>
#   filter(!is.na(gvkey)) |>
#   # add _q as identifier for quarterly data
#   rename_at(vars(c(cc_expo_ew:ph_sent_ew,ccpos:phsent)), ~ paste0(., "_q")) |>
#   # merge with yearly firm data
#   left_join(fread("data/df_year_fb.csv") |>
#               mutate(gvkey = as.character(gvkey)) |>
#               filter(!is.na(gvkey)) |>
#               # select only exposure data: control variables come from the quarterly dataframe
#               select(isin:ph_sent_ew,ccpos:phsent) |>
#               # add _y as identifier for yearly data
#               rename_at(vars(-c(isin,year)), ~ paste0(., "_y")),
#             by = c("isin","year"))



# Load firm level data ----------------------------------------------------

# load firm climate risk data
# create one data-frame with yearly and quarterly exposure data + yearly control variables
firm_data <- fread("data/02_processed/exposure_quarterly.csv") |>
  #mutate(gvkey = as.character(gvkey)) |>
  mutate(isin = as.character(isin), year=as.numeric(year)) |>
  #filter(!is.na(gvkey)) |>
  filter(!is.na(isin)) |>
  # add _q as identifier for quarterly data
  rename_at(vars(c(cc_expo_ew:ph_sent_ew,ccexp:phsent)), ~ paste0(., "_q"))

firm_data_year <- fread("data/02_processed/exposure_year.csv") |>
  #mutate(gvkey = as.character(gvkey)) |>
  mutate(isin = as.character(isin)) |>
  #filter(!is.na(gvkey)) |>
  filter(!is.na(isin)) |>
  # select only exposure data: control variables come from the quarterly dataframe
  select(isin:ph_sent_ew,ccexp:phsent) |>
  rename_at(vars(-c(isin,year)), ~ paste0(., "_y"))

# Check for duplicates
firm_data <- firm_data[!duplicated(firm_data), ] # none
firm_data_year <- firm_data_year[!duplicated(firm_data_year), ]

firm_data <- left_join(firm_data, firm_data_year, by=c("isin","year"))


# Merging -----------------------------------------------------------------

# Baseline dataset: all reports from lobbying disclosure act separated by issue
lobbying <- lobby_issue |>
  # cases where issue enters report multiple time due to different text entries:
  # we are only interested in each issue; not the sub-issues
  # --> remove issue_ordi and get only distinct rows to avoid duplicates
  select(-c(issue_ordi)) |>
  distinct() |>
  # merge with text of report
  left_join(lobby_text |> 
              select(-c(issue_ordi)) |>
              # cases where issue in report has multiple text entries:
              # combine text for these cases so that every report-issue only enters once
              group_by(report_uuid,issue_code) |>
              mutate(issue_text = paste(issue_text, collapse = " ")) |>
              distinct(), 
            by = c("report_uuid","issue_code")) |>
  # merge with report data
  left_join(lobby_report, by = "report_uuid") |>
  # merge with client data
  left_join(lobby_client, by = "client_uuid") |>
  mutate(gvkey = as.character(gvkey)) |>
  rename(year = report_year)

## drop NA gvkeys. Theres no way we can connect these to Compustat data
lobbying <- lobbying[!is.na(lobbying$gvkey), ]

##########

## drop a few observations that seem duplicated. Identical except one row specifies lobbying the House and the second row
## specifies lobbying House AND Senate. Seems doubtful these are genuinely distinct lobbying disbursements.
lobbying <- lobbying[!duplicated(lobbying[, c("gvkey", "year", "report_quarter_code", "report_uuid", "issue_code")]), ]

lobbying$amount_num <- gsub("\\$|,", "", lobbying$amount)
lobbying$amount_num <- as.numeric(lobbying$amount_num)
#test2 <- aggregate(lobbying$amount_num, by=list(lobbying$gvkey, lobbying$year, lobbying$report_quarter_code, lobbying$report_uuid, lobbying$issue_code, lobbying$issue_text, lobbying$primary_naics), FUN=function(x) sum(x, na.rm=T))
#names(test2) <- c("gvkey", "year", "report_quarter_code", "report_uuid", "issue_code", "issue_text", "primary_naics", "amount_num")

lobbying_num <- aggregate(lobbying$amount_num, by=list(lobbying$gvkey, lobbying$year, lobbying$report_quarter_code, lobbying$issue_code), FUN=function(x) sum(x, na.rm=T))
names(lobbying_num) <- c("gvkey", "year", "report_quarter_code", "issue_code", "amount_num")

lobbying_text <- aggregate(lobbying[, c("issue_text", "registrant_uuid", "registrant_name", "primary_naics")], 
                           by=list(lobbying$gvkey, lobbying$year, lobbying$report_quarter_code, lobbying$issue_code), 
                           FUN=function(x) paste(x, collapse = "|"))
names(lobbying_text) <- c("gvkey", "year", "report_quarter_code", "issue_code", "issue_text", "registrant_uuid", "registrant_name", "primary_naics")

lobbying <- merge(lobbying_num, lobbying_text)

##########


### there are 21 gvkey duplicates in firm_data
# we will deal with this later
# for now we drop these cases
dupl <- firm_data |>
  group_by(gvkey, year, quarter) |>
  filter(n()>1) |>
  arrange(gvkey, year, quarter) |>
  mutate(identifier = paste0(gvkey,"_",year,"_",quarter)) |>
  distinct(identifier) |>
  pull(identifier)

firm_data_reduced <- firm_data |>
  mutate(identifier = paste0(gvkey,"_",year,"_",quarter)) |>
  filter(!identifier %in% dupl)

# merge
lobbying$gvkey <- as.numeric(lobbying$gvkey)
df <- lobbying |>
  left_join(firm_data_reduced, by = c("gvkey", "year", "report_quarter_code" = "quarter"))

# only observations with climate exposure data
cc <- df |>
  filter(!is.na(cc_expo_ew_y))

# write csv
fwrite(df, file="data/03_final//lobbying_df.csv")
fwrite(cc, file="data/03_final/lobbying_df_reduced_fb.csv")


# Wide dataframe with binary issue code indicators ------------------------

# Note that this dataframe does neither contain report-issue text data 
# nor information on which institutions were lobbied
# because these information vary by issue within each report.

# lobbying_wide <- lobby_issue |>
#   select(-c(issue_ordi,gov_entity)) |>
#   distinct() |>
#   mutate(issue_bin = 1) |>
#   # from long to wide
#   pivot_wider(names_from = issue_code, values_from = issue_bin, values_fill = 0) |>
#   # merge with report data
#   left_join(lobby_report, by = "report_uuid") |>
#   # merge with client data
#   left_join(lobby_client, by = "client_uuid") |>
#   mutate(gvkey = as.character(gvkey)) |>
#   rename(year = report_year)


#numeric year-qtr-gvkey ids to speed indexing
lobbying$gvkey_n <- as.numeric(factor(lobbying$gvkey))
lobbying$unique_id <- as.numeric(paste0(lobbying$year, lobbying$report_quarter_code, lobbying$gvkey_n))

for(i in unique(lobbying$unique_id)) {
  temp <- lobbying[which(lobbying$unique_id == i), ]
  if (nrow(temp) > 1) {
    
    if( any(temp$amount_num>0) & length(unique(temp$amount_num))==1 ) {
      ## if all issue codes have exactly the same amounts, assume redundancy
      ## and distribute the single amount across all issues evenly
      temp$amount_num <- temp$amount_num[1] / nrow(temp)
    } else if( any(temp$amount_num>0) & sum(temp$amount_num>0)==1 ) {
      ## if one issue code has a non-zero amount but all other issue codes are
      ## zero, then distribute the single amount evenly across all issues
      temp$amount_num <- sum(temp$amount_num, na.rm=T) / nrow(temp)
    } else {
      
    }
    ## replace existing amount nums with distributed values
    lobbying$amount_num[which(lobbying$unique_id == i)] <- temp$amount_num
    
  }
  
}

## drop text variables that differ across issue codes (screw up the pivot, can solve later if we want)
lobbying <- lobbying[, !names(lobbying) %in% c("issue_text", "registrant_uuid", "registrant_name", "primary_naics")]
lobbying$issue_bin <- 1

lobbying_wide <- pivot_wider(lobbying, names_from = c("issue_code"), values_from=c("issue_bin", "amount_num"), values_fill = 0)
#View(lobbying_wide[duplicated(lobbying_wide[, c("gvkey", "year", "report_quarter_code")]) | duplicated(lobbying_wide[, c("gvkey", "year", "report_quarter_code")], fromLast = T), ])
## maintain existing names for issue code dummies. dont have to rewrite downstream code
names(lobbying_wide) <- gsub("issue_bin_", "", names(lobbying_wide))

# merge with firm_data
df_wide <- lobbying_wide |>
  left_join(firm_data_reduced, by = c("gvkey", "year", "report_quarter_code" = "quarter"))

#filter for observations with climate attention data 
cc_wide <- df_wide |>
  filter(!is.na(cc_expo_ew_y))

names(df_wide)[names(df_wide)=="assets"] <- "at"
names(cc_wide)[names(cc_wide)=="assets"] <- "at"




# Transform variables wide_reduced ----------------------------------------


# dummy variable: climate issues
cc_wide <- cc_wide |>
  mutate(CLI = ifelse(ENV == 1 | 
                        CAW == 1 |
                        ENG == 1 |
                        FUE == 1,
                      1,0))

cc_wide$CLI_dollars <- apply(cc_wide[, c("amount_num_ENV", "amount_num_CAW", "amount_num_ENG", "amount_num_FUE")],
                        1, function(x) sum(x, na.rm=T) / 1000000)



# Control variables -------------------------------------------------------

#US dummy variable 
cc_wide <- cc_wide |>
  mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

#Rename CO2 emissions variable 
# cc_wide <- cc_wide |>
#   rename(co2_emissions = En_En_ER_DP023)

#Total annual lobbying (total dollars)
cc_wide <- cc_wide |>
  group_by(gvkey, year) |>
  # mutate(total_lobby = n_distinct(report_uuid))
  mutate(total_lobby = sum(c_across(grep("amount_num", names(cc_wide), value=T))))


#Log and lag emissions variable 
# cc_wide <- cc_wide |>
#   mutate(log_co2 = log(co2_emissions + 1))

cc_wide <- cc_wide |>
  # group by unit (in our case: firm)
  group_by(gvkey) |>
  # arrange by year
  arrange(year) |>
  # for one year
  mutate(#log_co2_l1 = lag(log_co2, 1),
    total_lobby_l1 = lag(total_lobby, 1)) |>
  #ungroup
  ungroup()


cc_wide$industry <- cc_wide$bvd_sector
cc_wide <- cc_wide[which(cc_wide$industry!=""), ]
cc_wide$industry_year <- paste(cc_wide$industry, cc_wide$year)

sum(duplicated(cc_wide[, c("year", "report_quarter_code", "gvkey")]))

## continuous variables in regression models
cc_wide_cont_vars <- c("cc_expo_ew_y", "cc_expo_ew_q", "op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y",
                  "ebit", "at", "total_lobby")
## pull from main data
cc_wide_cont <- cc_wide[, cc_wide_cont_vars]
## rescale to standard normal
cc_wide_cont <- scale(cc_wide_cont)
## slot back into main cc_wide
cc_wide[, cc_wide_cont_vars] <- cc_wide_cont


# write csv
fwrite(df_wide, file="data/03_final/lobbying_df_wide.csv")
fwrite(cc_wide, file="data/03_final/lobbying_df_wide_reduced.csv")

#cc_wide 225025 by 422
#df_wide 1464710 by 422


### End