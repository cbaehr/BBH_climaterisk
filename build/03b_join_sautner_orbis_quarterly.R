
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data")}

pacman::p_load(fastLink, tidyverse)

rm(list = ls())

## load annual exposure data
exposureq <- read.csv("01_raw/exposure/cc_firmquarter_2021Q4_03082021_OSF.csv", stringsAsFactors = F)

## drop firms without a one-to-one mapping from gvkey to isin
#exposureq <- exposureq[which( !((duplicated(exposureq[, c("gvkey", "year")]) | duplicated(exposureq[, c("gvkey", "year")], fromLast=T)) & !is.na(exposureq$gvkey)) ), ]

exposure_cs <- exposureq[, c("gvkey", "isin", "hqcountrycode")]
exposure_cs <- exposure_cs[!duplicated(exposure_cs) , ]
## this is just to collapse down from year-firm level to cross-sectional (not repeated across years)

#View(exposure_cs[(duplicated(exposure_cs$gvkey) | duplicated(exposure_cs$gvkey, fromLast=T)) & !is.na(exposure_cs$gvkey), ])
#View(exposure_cs[(duplicated(exposure_cs$isin) | duplicated(exposure_cs$isin, fromLast=T)) & !is.na(exposure_cs$isin), ])

#exposure_cs <- exposure_cs[!(is.na(exposure_cs$gvkey & is.na(exposure_cs$isin))), ]
## doesnt look like there are any cases where the gvkey AND isin are both missing

## drop cases in which the gvkey OR isin is duplicated, and they are NOT NA.
## duplication implies that a single isin code corresponds to multiple gvkey/bvdids.
## keeping these would mean that we are using a single row of Orbis firm data for 
## MULTIPLE lobbyview rows. Might keep these for robustness check later, but drop now for main (~600 rows).
## (aside: we want to keep cases if ONE is NA, because we might still be able to match on the OTHER)
duplicate_gvkey <- (duplicated(exposure_cs$gvkey) | duplicated(exposure_cs$gvkey, fromLast=T)) & !is.na(exposure_cs$gvkey)
duplicate_isin <- (duplicated(exposure_cs$isin) | duplicated(exposure_cs$isin, fromLast=T)) & !is.na(exposure_cs$isin)
exposure_cs <- exposure_cs[!(duplicate_gvkey | duplicate_isin), ]

## reshape main exposure data from long to wide - we want all the variables, but
## just for the subset of firms we identified above
exposureq$yearqtr <- paste(exposureq$year, exposureq$quarter, sep="_")
exposureq <- exposureq[, !names(exposureq) %in% c("gvkey", "hqcountrycode")] # omitting gvkey and hq code so they arent repeated
#exposureq_wide <- reshape(exposureq, idvar = "isin", timevar = "year", direction = "wide", sep = "_")
exposureq_wide <- reshape(exposureq, idvar = "isin", timevar = "yearqtr", direction = "wide", sep = "_")
exposureq_wide <- merge(exposureq_wide, exposure_cs) # only keep firms with isin in exposure_cs (legit candidates)

## ^ this is the set of possible firms from the Sautner data (12786 total) - prioritize maximizing number of 
## these firms with matches in Orbis 

rm(list = setdiff(ls(), "exposureq_wide")) # exposureq_wide all we need to keep from above

#####

## read in orbis firm covariate data
orbis <- read.csv("02_processed/orbis_11_06_2023.csv", stringsAsFactors = F)

orbis <- orbis[!is.na(orbis$isin), ] # since merging with sautner on isin, no need for NA isin rows
# sum(duplicated(orbis$isin))
## no duplicated isins in orbis

## merge orbis with 
exposure_orbis_wide <- merge(exposureq_wide, orbis, by="isin")

## matches with 12009 of 12786 possible matches - pretty good

## revisit this later -- look at the nonmatches and see if any closeness

#####

# a <- exposureq_wide$isin[which(!exposureq_wide$isin %in% exposure_orbis_wide$isin)]
# b <- orbis$isin[which(!orbis$isin %in% exposure_orbis_wide$isin)]
# View(data.frame(sort(unique(a))))
# View(data.frame(sort(unique(b))))
# matches.out <- fastLink(dfA = exposureq_wide[which(!exposureq_wide$isin %in% exposure_orbis_wide$isin), ], dfB = orbis, varnames = c("isin"), threshold.match = 0.5)

## no success with fuzzy matches, even when lowering the threshold

#####


rm(list = setdiff(ls(), c("exposure_orbis_wide")))
## exposureq_wide all we need to keep from above. These are now the "candidate"
## firms - those we could link across sautner-orbis. Want to maximize matches with
## this set of firms and lobbyview

#####

## load in lobbyview
lobby_client <- read.csv("01_raw/lobbyview/dataset___client_level.csv", stringsAsFactors = F)

## convert empty bvdid to NA - 
lobby_client$bvdid[which(lobby_client$bvdid=="")] <- NA

## drop lobby rows where both bvdid and gvkey missing - no chance of matching these
## with sautner-orbis 
lobby_client <- lobby_client[which(!(is.na(lobby_client$bvdid) & is.na(lobby_client$gvkey))), ]


######

## no na bvdids in exposure_orbis_wide - no concerns with merging on this var
sum(is.na(exposure_orbis_wide$bvdid))
sum(exposure_orbis_wide$bvdid=="")

## NA bvdids in lobby_client data could screw up when were merging on that variable.
## replace NAs with "-1", because this value isnt present in lobbyview. So it will render these
## obsolete
## (but we dont want to drop these rows, because the gvkey could still be valuable)
lobby_client$bvdid[is.na(lobby_client$bvdid)] <- "-1"

## same logic for gvkeys
exposure_orbis_wide$gvkey[is.na(exposure_orbis_wide$gvkey)] <- (-1)
#exposure_orbis_wide$gvkey[is.na(exposure_orbis_wide$gvkey)] <- (-2) # use different value for NAs - otherwise would match on this

## have properly accounted for the possibility of NAs in merging variables.

## merge exposure_orbis and lobbyview by bvdid (drop gvkey from lobby to avoid multiple variables with same name)
a <- merge(exposure_orbis_wide, lobby_client[,names(lobby_client)!="gvkey"], by="bvdid", all.x = T)
## we are keeping ALL exposure-orbis rows. This is based on our assumption that all firms which we cannot match
## with lobbyview did NOT lobby at all. We still keep these data in our sample, and treat lobbying amount and
## presence as all zeroes. Pretty confident there is no duplication of data here, because these guys get no
## lobbying data, and we are dealing with duplicated bvdid or gvkeys already.

## merge exposure_orbis and lobbyview by gvkey
b <- merge(exposure_orbis_wide, lobby_client[,names(lobby_client)!="bvdid"], by="gvkey")
## align column names in gvkey matches with those of bvdid matches -> for merging
b <- b[,names(a)]

d <- rbind(a, b) # stack the two datasets together - then drop duplicates
d <- d[!duplicated(d), ]

#sum(duplicated(d$bvdid))
#View(d[duplicated(d$bvdid)|duplicated(d$bvdid, fromLast=T), ]) # bunch of duplicated bvdids, single isin

#sum(duplicated(d$gvkey))
#View(d[duplicated(d$gvkey)|duplicated(d$gvkey, fromLast=T), ]) # bunch of duplicate gvkeys, single isin

## first identify the duplicated bvdid or gvkeys
duplicates <- d[(duplicated(d$bvdid)|duplicated(d$bvdid, fromLast=T)) | ((duplicated(d$gvkey)|duplicated(d$gvkey, fromLast=T)) & d$gvkey!=(-1)), ]

## this aggregation is intended to identify the NUMBER OF ROWS in the duplication dataset that share
## a single, gvkey, bvdid, and isin combination (and if one of them contains an NA). If this number is two, and one has non-NA lobbyview,
## then we will put that row back into the main data and discard the NA row.
numrows <- aggregate(duplicates$client_uuid, by=list(duplicates$bvdid, duplicates$isin, duplicates$gvkey), FUN=function(x) is.na(x))
names(numrows) <- c("bvdid", "isin", "gvkey", "NAs") # give meaningful names

duplicates <- merge(duplicates, numrows) # fold back into the duplicates data -- without specifying a merge variable, this will use all three of the IDs to merge
duplicates$NA_col <- sapply(duplicates$NAs, FUN = function(x) {length(x)==2 & sum(x)==1}) # TRUE if exactly two rows with this ID combo AND one is NA
duplicates$keep_override <- duplicates$NA_col & !is.na(duplicates$client_uuid) # if NA_col condition TRUE and this particular rows client_uuid is not NA, we want to keep it

summary(duplicates$keep_override)
names(duplicates)

duplicates_out <- duplicates[, c("bvdid", "isin", "cc_expo_ew_2016_1",
                                 "gvkey", "hqcountrycode", "conm",
                                 "total_assets_usd_2016", "client_uuid", "client_name", "keep_override", "NAs", "NA_col")]
View(duplicates_out)
duplicates_out <- duplicates_out[,names(duplicates_out)!="NAs"]
#write.csv(duplicates_out, "xx_other/exposure_orbis_bvdidORgvkey_duplicated_quarterly.csv", row.names=F)
## these are the cases where we do have a match between orbis-lobbying (either gvkey or bvdid),
## but the relationship between gvkey/bvdid <-> isin is NOT one-to-one

#####

## these are the remaining rows that have multiple client_uuid's associated with a single
## isin code. This is overwhelmingly because the company name in Lobbyview is marginally screwed
## up, but nonetheless LobbyView codes them as separate companies.

## note there are a few problem cases that actually have duplicated client_uuids - will need
## to deal with these separately
clientnm_issues <- duplicates[which(!duplicates$NA_col & !is.na(duplicates$client_name)), ]

report <- read.csv("01_raw/lobbyview/dataset___report_level.csv", stringsAsFactors = F)
report <- report[which(report$client_uuid %in% clientnm_issues$client_uuid), ]
sum(duplicated(report$client_uuid))
sum(duplicated(report[, c("client_uuid", "report_uuid")])) #no report IDs duplicated

## want to create new variables for each row that contain 1) all the rows OWN lobbying years 
## and 2) all lobbying years for different rows with the SAME isin. We will then check
## whether row i shares any lobbying years with its other isin brethren. If NOT,
## then we feel comfortable folding row i back into the main dataframe
for(i in 1:nrow(clientnm_issues)) {
  if(i==1) {
    ## empty vectors for storage
    client_uuid_yrs <- c()
    isin_yrs <- c()
    
  }
  ## retrieve all the reporting years for a given client_uuid
  self <- which(report$client_uuid==clientnm_issues$client_uuid[i])
  self_yrs <- unique(report$report_year[which(report$client_uuid==clientnm_issues$client_uuid[i])])
  
  ## retrieve all reporting years for OTHER client_uuids under the SAME isin
  others <- which(clientnm_issues$isin == clientnm_issues$isin[i])
  others <- setdiff(others, i) # remove self (want to see if i has same or different years as all OTHERS)
  others_yrs <- unique(report$report_year[which(report$client_uuid %in% clientnm_issues$client_uuid[others])])
  
  ## stash as new elements
  client_uuid_yrs <- c(client_uuid_yrs, list(self_yrs))
  isin_yrs <- c(isin_yrs, list(others_yrs))
}

distinctyear_test <- mapply(FUN = function(x1, x2) {!any(x1 %in% x2)}, client_uuid_yrs, isin_yrs)
sum(distinctyear_test)
## these are the ones where we can easily fold these back into the main data.
## Because if they fail this test, it means that these client_uuids all falling under
## the same isin code do NOT have any years in common. So there is no concern of double
## counting or anything like that


overlapping_years <- clientnm_issues[!distinctyear_test,]
## these are the cases where years from one client_uuid overlap with years from another client_uuid
## all under the same isin umbrella. These cases could be a problem if e.g. the lobbying dollars
## for a given year are double counted

## this is all to produce years and dollar amounts for lobbying by each client_uuid,
## so Vincent can analyze for potential duplications. 
for(i in 1:nrow(overlapping_years)) {
  if(i==1) {
    client_uuid_amt <- c()
    client_uuid_yrs <- c()
  }
  self <- which(report$client_uuid==overlapping_years$client_uuid[i])
  self_amt <- report$amount[which(report$client_uuid==overlapping_years$client_uuid[i])]
  self_yrs <- report$report_year[which(report$client_uuid==overlapping_years$client_uuid[i])]
  
  client_uuid_amt <- c(client_uuid_amt, list(self_amt))
  client_uuid_yrs <- c(client_uuid_yrs, list(self_yrs))
}

overlapping_years$client_uuid_amounts <- client_uuid_amt
overlapping_years$client_uuid_years <- client_uuid_yrs

## function to make the df long (currently all years/amounts housed in lists)
rowtodf <- function(x) {
  out <- data.frame(bvdid=x["bvdid"],
                    isin=x["isin"],
                    gvkey=x["gvkey"],
                    hqcountrycode=x["hqcountrycode"],
                    conm=x["conm"],
                    client_uuid=x["client_uuid"],
                    client_name=x["client_name"],
                    year=x["client_uuid_years"],
                    amount=x["client_uuid_amounts"])
  return(out)
}

overlapping_yrs_long <- apply(overlapping_years, 1, rowtodf)
out <- do.call(rbind, overlapping_yrs_long)
write.csv(out, "xx_other/duplicates_reportyears_overlap.csv", row.names = F)

#####


d <- d[!(duplicated(d$bvdid)|duplicated(d$bvdid, fromLast=T)), ]## need to OMIT for now those cases which have multiple bvdids to a single isin
d <- d[!( (duplicated(d$gvkey)|duplicated(d$gvkey, fromLast=T)) & d$gvkey!=(-1) ), ]## need to OMIT for now those cases which have multiple gvkeys to a single isin

## want to incorporate rows from duplicates where keep_override is TRUE back into d. These are the rows
## which have lobbyview data and are not truly duplicated in the main data.
d <- rbind(d, duplicates[which(duplicates$keep_override), names(d)])
d <- rbind(d, clientnm_issues[distinctyear_test, names(d)]) # keep those rows with duplicate isins that pass the "distinct years"

unique(d$gvkey[duplicated(d$gvkey)]) # only -1 is duplicated for gvkey. This is ok because these are all essentially rows
# with no corresponding lobbyview row. They will get 0's for lobbyview variables.

## now lets see if we can push up the matches for exposure_orbis_wide with lobby_client
## for those lobby firms NOT in d (which are already matched)

#(!exposure_orbis_wide$gvkey %in% c$gvkey) & !is.na(exposure_orbis_wide$gvkey)
#(!exposure_orbis_wide$bvdid %in% c$bvdid) & !is.na(exposure_orbis_wide$bvdid)

unmatched <- (!exposure_orbis_wide$gvkey %in% d$gvkey) | (!exposure_orbis_wide$bvdid %in% d$bvdid)
e <- exposure_orbis_wide[unmatched, ]

## how to push up matches? Either 1) detech systematic differences between 
## umatched gvkeys and lobbyview gvkeys OR bvdids and lobbyview bvdids. Or 2)
## use fuzzy matching to detect cases in lobbyview which are close but dont match exactly
## with cases in orbis

#####

## maximized pure matches on gvkey and bvdid. Now want to check whether there are 
## any "fuzzy" matches we can still obtain with the remaining sautner-orbis data
## bunch of efforts to wring out additional matches -- no luck

## check if any unmatched cases somehow match perfectly (and arent duplicate throwaways)
sum(e$gvkey %in% lobby_client$gvkey & (!e$gvkey %in% duplicates$gvkey))
sum(e$bvdid %in% lobby_client$bvdid & !is.na(e$bvdid) & (!e$bvdid %in% duplicates$bvdid))
## nope -- all remaining cases do NOT match perfectly with any rows in LobbyView

## do number of characters differ?
table(nchar(e$gvkey))
table(nchar(lobby_client$gvkey))
## somewhat for gvkey - lot more 5's and 6's in exposure_orbis

table(nchar(e$bvdid))
table(nchar(lobby_client$bvdid))
## lot more 12's in lobbyview than exposure_orbis

class(e$bvdid)
class(lobby_client$bvdid)
## both character

sum(substr(lobby_client$bvdid, 1, 2) == "US")
sum(substr(e$bvdid, 1, 2) == "US")
## lot more US prefixes in lobby_client than exposure_orbis - all lobbyview is US, only ~50% of exposure_orbis


e_us <- e[which(substr(e$bvdid, 1, 2) == "US"), ]

## within us only, number of characters
table(nchar(e_us$bvdid))
table(nchar(lobby_client$bvdid))
## lot more 12's for lobbyview - is it the "L" suffix?

## look at random sample of ids
sample(e_us$bvdid, 20)
sample(lobby_client$bvdid, 20)

## check if we just add L suffix to exposure_orbis if it produces matches
sum(paste0(e$bvdid, "L") %in% lobby_client$bvdid & (!e$bvdid %in% duplicates$bvdid))
## nope

# matches.out <- fastLink(dfA = e[!e$bvdid %in% duplicates$bvdid, ], dfB = lobby_client, varnames = c("bvdid"), threshold.match = 0.75)
# matches.out <- fastLink(dfA = e[!e$gvkey %in% duplicates$gvkey, ], dfB = lobby_client, varnames = c("gvkey"), threshold.match = 0.5)

## no success with fuzzy matches, even when lowering the threshold

## check levenshtein distance
#test <- lapply(e_us$bvdid, FUN = function(x) as.numeric(adist(x, lobby_client$bvdid)))
#test2 <- sapply(test, FUN = function(x) min(x))

#sum(test2==0)
#sum(e_us$bvdid[test2==0] %in% duplicates$bvdid)

## no real luck

## takes a LONG TIME
#e_us_nogvkeydups <- e_us[!is.na(e_us$gvkey), ]
#test <- lapply(e_us_nogvkeydups$gvkey, FUN = function(x) as.numeric(adist(x, lobby_client$gvkey)))
#test2 <- sapply(test, FUN = function(x) min(x, na.rm=T))
#sum(test2==0)
#sum(e_us_nogvkeydups$gvkey[test2==0] %in% duplicates$gvkey) # all the zero distance matches are just duplicates

## no luck getting additional matches

######

## the legit matches are stored in d -- rename
exposure_orbis_lobby_wide <- d
#View(exposure_orbis_lobby_wide[duplicated(exposure_orbis_lobby_wide$client_uuid) | duplicated(exposure_orbis_lobby_wide$client_uuid, fromLast=T),])
duplicate_client_uuid <- (duplicated(exposure_orbis_lobby_wide$client_uuid) | duplicated(exposure_orbis_lobby_wide$client_uuid, fromLast=T)) & !is.na(exposure_orbis_lobby_wide$client_uuid)
## we ignore cases where client_uuid is NA because these are precisely the rows which
## we assume did NO lobbying whatsoever, and we assume they are zeroes for lobbying measures.

exposure_orbis_lobby_wide <- exposure_orbis_lobby_wide[!duplicate_client_uuid, ]
exposure_orbis_lobby_wide$client_uuid[is.na(exposure_orbis_lobby_wide$client_uuid)] <- (-1) # for convenience. Ensures no merges screwed up b/c no client_uuids =(-1)


sum(duplicated(exposure_orbis_lobby_wide$isin))
sum(duplicated(exposure_orbis_lobby_wide$bvdid))

#timespan <- 1999:2023
timespan <- apply(expand.grid(c(1999:2023), c(1:4)), 1, paste, collapse="_")

time_varying <- c("cc_expo_ew_", "cc_risk_ew_", "cc_pos_ew_", "cc_neg_ew_",
                  "cc_sent_ew_", "op_expo_ew_", "op_risk_ew_", "op_pos_ew_",
                  "op_neg_ew_", "op_sent_ew_", "rg_expo_ew_", "rg_risk_ew_",
                  "rg_pos_ew_", "rg_neg_ew_", "rg_sent_ew_", "ph_expo_ew_", 
                  "ph_risk_ew_", "ph_pos_ew_", "ph_neg_ew_", "ph_sent_ew_",
                  "total_assets_usd_", "n_employees_", "operating_rev_usd_",
                  "P_L_b4tax_usd_")
moving_list <- lapply(time_varying, function(x) paste0(x, timespan))


for(i in c("total_assets_usd_", "n_employees_", "operating_rev_usd_", "P_L_b4tax_usd_")) {
  for(j in 1999:2023) {
    nm <- paste0(i, j)
    exposure_orbis_lobby_wide[, paste0(nm, "_1")] <- exposure_orbis_lobby_wide[, nm]
    exposure_orbis_lobby_wide[, paste0(nm, "_2")] <- exposure_orbis_lobby_wide[, nm]
    exposure_orbis_lobby_wide[, paste0(nm, "_3")] <- exposure_orbis_lobby_wide[, nm]
    exposure_orbis_lobby_wide[, paste0(nm, "_4")] <- exposure_orbis_lobby_wide[, nm]
    exposure_orbis_lobby_wide <- exposure_orbis_lobby_wide[,names(exposure_orbis_lobby_wide)!=nm]
  }
}


# adding columns with all missing values where necessary to get balanced 
# time-dependent variables (needed for reshape function)
missing_cols <- unlist(moving_list)[!(unlist(moving_list) %in% names(exposure_orbis_lobby_wide))]
exposure_orbis_lobby_wide[, missing_cols] <- NA

## reshape data from wide to long format
exposure_orbis_lobby_long <- reshape(exposure_orbis_lobby_wide,
                                     direction="long",
                                     varying=moving_list,
                                     times=timespan,
                                     #timevar="year",
                                     timevar="yearqtr",
                                     idvar="isin")

exposure_orbis_lobby_long <- exposure_orbis_lobby_long[,!grepl("year_", names(exposure_orbis_lobby_long))]
exposure_orbis_lobby_long <- exposure_orbis_lobby_long[,!grepl("quarter_", names(exposure_orbis_lobby_long))]

names(exposure_orbis_lobby_long) <- gsub("_1999_1", "", names(exposure_orbis_lobby_long))

exposure_orbis_lobby_long$year <- sapply(exposure_orbis_lobby_long$yearqtr, FUN = function(x) strsplit(x, "_")[[1]][1])
exposure_orbis_lobby_long$qtr <- sapply(exposure_orbis_lobby_long$yearqtr, FUN = function(x) strsplit(x, "_")[[1]][2])
# summary(exposure_orbis_lobby_long$op_expo_ew[which(exposure_orbis_lobby_long$year==2010)])
# summary(exposure_orbis_lobby_wide$op_expo_ew_2010)
# 
# exposure_orbis_lobby_long$n_employees <- as.numeric(exposure_orbis_lobby_long$n_employees)
# exposure_orbis_lobby_wide$n_employees_2010 <- as.numeric(exposure_orbis_lobby_wide$n_employees_2010)
# summary(exposure_orbis_lobby_long$n_employees[which(exposure_orbis_lobby_long$year==2010)])
# summary(exposure_orbis_lobby_wide$n_employees_2010)
## ^ all suggest that the reshape was successful

write.csv(exposure_orbis_lobby_wide, "02_processed/exposure_orbis_client_quarter_wide_REVISE.csv", row.names = F)
write.csv(exposure_orbis_lobby_long, "02_processed/exposure_orbis_client_quarter_long_REVISE.csv", row.names = F)

## want to think about whether we want to assume that all firms in exposure-orbis, but 
## that cant be matched with LobbyView, are actually just all NON LOBBYING firms??? Do 
## we want to include these in our final dataset as such???

## 6 cases of duplicated client_uuid in orbis wide data. Want to remove? YES. Done

sum(duplicated(exposure_orbis_lobby_long[, c("isin", "yearqtr")]))
sum(duplicated(exposure_orbis_lobby_long[, c("gvkey", "yearqtr")]))
sum(duplicated(exposure_orbis_lobby_long[, c("bvdid", "yearqtr")]))
