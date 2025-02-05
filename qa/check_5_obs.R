
rm(list=ls())

# install_github("insongkim/concordance", dependencies=TRUE)
# Load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary, readxl)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

## Obs 1 ---------------------------------------------------------------------

d1 <- df[which(df$year==2001 & df$qtr==1 & df$isin=="CA91701P1053") , ]
## Missing both lobbying and exposure data -- so this firm should NOT be present in
## either of those datasets. I can still QA Orbis covariates.


## Should NOT have a match in Lobbyview
lobby_client <- fread("data/01_raw/lobbyview_20250103/clients_codebook/clients.csv")
which(lobby_client$bvdid==d1$bvdid)
which(lobby_client$gvkey==d1$gvkey)
which(lobby_client$gvkey==d1$gvkey)
grep("URANIUM", toupper(lobby_client$client_name), value=T)
View(lobby_client[which(lobby_client$client_name=="URANIUM ONE") , ])
## There IS a Uranium One in LobbyView, but it has no corresponding bvdid or gvkey,
## so we have no choice but to not match with it.

orbis <- read.csv("data/02_processed/orbis_11_06_2023.csv", stringsAsFactors = F)

View(orbis[which(orbis$isin==d1$isin) , ])
## NACE and BvD sector align
## Total assets line up exactly for 2001

exposureq <- read.csv("data/01_raw/exposure/firmquarter_score_2023Q4_Version_2024_Aug.csv", stringsAsFactors = F)

which(exposureq$isin==d1$isin & exposureq$year==d1$year & exposureq$quarter==d1$qtr)
which(exposureq$gvkey==d1$gvkey & exposureq$year==d1$year & exposureq$quarter==d1$qtr)
## No match for the exposure data, which we expect because exposure data is NA for this ob.


## Obs 2 ---------------------------------------------------------------------

d2 <- df[which(df$year==2014 & df$qtr==3 & df$isin=="US7847741011") , ]
## Missing both lobbying and exposure data -- so this firm should NOT be present in
## either of those datasets. I can still QA Orbis covariates.

which(lobby_client$bvdid==d2$bvdid)
which(lobby_client$gvkey==d2$gvkey)
which(lobby_client$gvkey==d2$gvkey)
grep("STEC", toupper(lobby_client$client_name), value=T)
## Dont have a Lobbyview match as expected
## Dont see any firms that look like "STEC, INC." in Lobbyview

View(orbis[which(orbis$isin==d2$isin) , ])
## NACE and BvD sector align
## Total assets missing for 2014 as expected ("n.a" in Orbis)

which(exposureq$isin==d2$isin & exposureq$year==d2$year & exposureq$quarter==d2$qtr)
which(exposureq$gvkey==d2$gvkey & exposureq$year==d2$year & exposureq$quarter==d2$qtr)
View(exposureq[which(exposureq$gvkey==d2$gvkey) , ])
## No exposure data for 2014 as expected because NA for this ob. Exposure data only up to 2013

## Obs 3 ---------------------------------------------------------------------

d3 <- df[which(df$year==2013 & df$qtr==3 & df$isin=="US45170X2053") , ]

which(lobby_client$bvdid==d3$bvdid)
which(lobby_client$gvkey==d3$gvkey)
which(lobby_client$gvkey==d3$gvkey)
View(lobby_client[which(lobby_client$gvkey==d3$gvkey) , ])
## Has a Lobbyview match. Company in the panel is named "IDENTIV, INC." but in Lobbyview
## is named "3VR SECURITY, INC."
## A Google confirms these are the same company: https://www.superiortelephone.com/wp-content/uploads/2019/04/3VR-by-Identiv-OverviewBrochure.pdf

## Panel indicates they did NOT lobby in 2013 Q3
lobby_report <- fread("data/01_raw/lobbyview_20250103/reports_codebook/reports.csv")
which(lobby_report$lob_id == lobby_client$lob_id[which(lobby_client$gvkey==d3$gvkey)])
View(lobby_report[which(lobby_report$lob_id == lobby_client$lob_id[which(lobby_client$gvkey==d3$gvkey)]) , ])
## All of their lobbying actvity was in 2009-2010. So our observation is correct.

View(df[which(df$isin=="US45170X2053" & df$year %in% c(2009, 2010)) , ])
## Looking at the firms 2009-2010 observations, we get the quarter lobbied correct (Q4 in 2009 and Q1 in 2010)
## The panel shows that they do NOT lobby on climate issues. Check with the issue-level data

## Check what issues and targets they lobbied
lobby_issue <- fread("data/01_raw/lobbyview_20250103/issues_codebook/issues.csv")
View(lobby_issue[which(lobby_issue$report_uuid %in% lobby_report$report_uuid[which(lobby_report$lob_id == lobby_client$lob_id[which(lobby_client$gvkey==d3$gvkey)])]) , ])
## Only lobbied the House and Senate, and did NOT lobby on climate-related issues.
## So we correctly code them as having no climate lobbying activity
## Targets not relevant since it is not "climate" lobbying activity

View(orbis[which(orbis$isin==d3$isin) , ])
## NACE and BvD sector align
## We get their total assets for 2013 correct (58759)

which(exposureq$isin==d3$isin & exposureq$year==d3$year & exposureq$quarter==d3$qtr)
which(exposureq$gvkey==d3$gvkey & exposureq$year==d3$year & exposureq$quarter==d3$qtr)
View(exposureq[which(exposureq$gvkey==d3$gvkey | exposureq$isin==d3$isin) , ])
## It does link to exposure observation -- even though exposure for this firm-quarter is zero,
## it still gets a non-zero value in the panel after the z-score transformation

## But I can do the z-transformation directly on the exposure data and should get a pretty close result to d3
as.numeric(scale(exposureq$cc_expo_ew))[which((exposureq$gvkey==d3$gvkey | exposureq$isin==d3$isin) & exposureq$year==d3$year & exposureq$quarter==d3$qtr)]
d3$cc_expo_ew
## This value should be relatively close to the value in d3
## And it is (-0.2275352 vs. -0.226903)

as.numeric(scale(exposureq$op_sent_ew))[which((exposureq$gvkey==d3$gvkey | exposureq$isin==d3$isin) & exposureq$year==d3$year & exposureq$quarter==d3$qtr)]
d3$op_sent_ew
## Also close

## Obs 4 ---------------------------------------------------------------------

d4 <- df[which(df$year==2004 & df$qtr==2 & df$isin=="US7394214024") , ]
## Should be a match in exposure but no match in Lobbyview

which(lobby_client$bvdid==d4$bvdid)
which(lobby_client$gvkey==d4$gvkey)
which(lobby_client$gvkey==d4$gvkey)
View(lobby_client[which(lobby_client$gvkey==d4$gvkey) , ])
d4$conm
grep("PRAECIS", lobby_client$client_name, value=T)
View(lobby_client[grep("PRAECIS", lobby_client$client_name) , ])
## There is a Praecis in Lobbyview but they have no corresponding gvkeys or bvdids,
## so thus no way to match

View(orbis[which(orbis$isin==d4$isin) , ])
## NACE and BvD sector align
## We get their total assets for 2004 correct (154307)

which(exposureq$isin==d4$isin & exposureq$year==d4$year & exposureq$quarter==d4$qtr)
which(exposureq$gvkey==d4$gvkey & exposureq$year==d4$year & exposureq$quarter==d4$qtr)
View(exposureq[which(exposureq$gvkey==d4$gvkey | exposureq$isin==d4$isin) , ])

## Do the z-transformation thing again
as.numeric(scale(exposureq$cc_expo_ew))[which((exposureq$gvkey==d4$gvkey | exposureq$isin==d4$isin) & exposureq$year==d4$year & exposureq$quarter==d4$qtr)]
d4$cc_expo_ew
## Very close

as.numeric(scale(exposureq$op_risk_ew))[which((exposureq$gvkey==d4$gvkey | exposureq$isin==d4$isin) & exposureq$year==d4$year & exposureq$quarter==d4$qtr)]
d4$op_risk_ew
## Also close

## Obs 5 ---------------------------------------------------------------------

## Need to find one that has non-zero lobbying in a given quarter based on Lobbyview
candidates <- df$isin[which(df$CLI_quarter != 0)]
set.seed(123)
sample(candidates, 1)

d5 <- df[which(df$year==2010 & df$qtr==2 & df$isin=="US59156R1086") , ]
## This one will work
## Should have a Lobbyview match with climate lobbying
## Should also have exposure data
## But no info on assets/Orbis


which(lobby_client$bvdid==d5$bvdid)
which(lobby_client$gvkey==d5$gvkey)
which(lobby_client$gvkey==d5$gvkey)
View(lobby_client[which(lobby_client$gvkey==d5$gvkey) , ])
## Has a match -- Metlife, Inc. is correct

## Panel indicates they DID lobby in 2010 Q2
which(lobby_report$lob_id == lobby_client$lob_id[which(lobby_client$gvkey==d5$gvkey)])
View(lobby_report[which(lobby_report$lob_id == lobby_client$lob_id[which(lobby_client$gvkey==d5$gvkey)]) , ])
## Lobbying data confirms they lobbied in 2010 Q2

## Check what issues and targets they lobbied
reportids_idx <- which(lobby_report$lob_id==lobby_client$lob_id[which(lobby_client$gvkey==d5$gvkey)] & lobby_report$filing_year==2010 & lobby_report$filing_period_code %in% c("H1", "Q2"))
View(lobby_issue[which(lobby_issue$report_uuid %in% lobby_report$report_uuid[reportids_idx]) , ])
## Confirms that one of the reports was Energy lobbying (ENG)
## Also confirms that they lobbied both the House and Senate (entity IDs 1 and 2)

## Next need to confirm they spent $16,666 on climate lobbying in that quarter

## First, subset to just the specific report code pertaining to climate lobbying
issue_subset <- lobby_issue[which(lobby_issue$report_uuid %in% lobby_report$report_uuid[reportids_idx]) , ]
env_reportid <- issue_subset$report_uuid[which(issue_subset$general_issue_code == "ENG")]

lobby_report$amount[which(lobby_report$report_uuid==env_reportid)]
## $50000 in lobbying on that issue

sum(issue_subset$report_uuid==env_reportid)
## There are three reports corresponding to that specific report code (the 50000 is split three ways). 
## Only one report pertains to climate issues. Since we assume they distribute lobbying dollars evenly 
## across all reports, this comes out to $50000/3=$16,666 spent on lobbying climate issues. Exactly what 
## we record in the data.

## Now does $8333 go towards lobbying the senate and house as the panel implies?
## Yes because we again assume that the $16666 in climate lobbying is distributed evenly across all targets
## of the lobbying report, and so that means they split the $16666 between House and Senate. 
## $16666/2=$8333










