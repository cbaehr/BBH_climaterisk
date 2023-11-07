
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("/Users/vincentheddesheimer/Dropbox (Princeton)/BBH/BBH1/data")}

pacman::p_load(fastLink, tidyverse)

rm(list = ls())

## load annual exposure data
exposurey <- read.csv("01_raw/exposure/cc_firmyear_2021Q4_03082021_OSF.csv", stringsAsFactors = F)

## drop firms without a one-to-one mapping from gvkey to isin
#exposurey <- exposurey[which( !((duplicated(exposurey[, c("gvkey", "year")]) | duplicated(exposurey[, c("gvkey", "year")], fromLast=T)) & !is.na(exposurey$gvkey)) ), ]

exposure_cs <- exposurey[, c("gvkey", "isin", "hqcountrycode")]
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
exposurey <- exposurey[, !names(exposurey) %in% c("gvkey", "hqcountrycode")] # omitting gvkey and hq code so they arent repeated
exposurey_wide <- reshape(exposurey, idvar = "isin", timevar = "year", direction = "wide", sep = "_")
exposurey_wide <- merge(exposurey_wide, exposure_cs) # only keep firms with isin in exposure_cs (legit candidates)

## ^ this is the set of possible firms from the Sautner data (12786 total) - prioritize maximizing number of 
## these firms with matches in Orbis 

rm(list = setdiff(ls(), "exposurey_wide")) # exposurey_wide all we need to keep from above

#####

## read in orbis firm covariate data
orbis <- read.csv("02_processed/orbis_11_06_2023.csv", stringsAsFactors = F)

orbis <- orbis[!is.na(orbis$isin), ] # since merging with sautner on isin, no need for NA isin rows
# sum(duplicated(orbis$isin))
## no duplicated isins in orbis

## merge orbis with 
exposure_orbis_wide <- merge(exposurey_wide, orbis, by="isin")

## matches with 12009 of 12786 possible matches - pretty good

## revisit this later -- look at the nonmatches and see if any closeness

#####

# a <- exposurey_wide$isin[which(!exposurey_wide$isin %in% exposure_orbis_wide$isin)]
# b <- orbis$isin[which(!orbis$isin %in% exposure_orbis_wide$isin)]
# View(data.frame(sort(unique(a))))
# View(data.frame(sort(unique(b))))
# matches.out <- fastLink(dfA = exposurey_wide[which(!exposurey_wide$isin %in% exposure_orbis_wide$isin), ], dfB = orbis, varnames = c("isin"), threshold.match = 0.5)

## no success with fuzzy matches, even when lowering the threshold

#####


rm(list = setdiff(ls(), c("exposure_orbis_wide")))
## exposurey_wide all we need to keep from above. These are now the "candidate"
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
exposure_orbis_wide$gvkey[is.na(exposure_orbis_wide$gvkey)] <- (-2) # use different value for NAs - otherwise would match on this

## have properly accounted for the possibility of NAs in merging variables.

## merge exposure_orbis and lobbyview by bvdid (drop gvkey from lobby to avoid multiple variables with same name)
a <- merge(exposure_orbis_wide, lobby_client[,names(lobby_client)!="gvkey"], by="bvdid", all.x=T)
## we are keeping ALL exposure-orbis rows. This is based on our assumption that all firms which we cannot match
## with lobbyview did NOT lobby at all. We still keep these data in our sample, and treat lobbying amount and
## presence as all zeroes. Pretty confident there is no duplication of data here, because these guys get no
## lobbying data, and we are dealing with duplicated bvdid or gvkeys already.


## merge exposure_orbis and lobbyview by gvkey
b <- merge(exposure_orbis_wide, lobby_client[,names(lobby_client)!="bvdid"], by="gvkey")
## align column names in gvkey matches with those of bvdid matches -> for merging
b <- b[,names(a)]

d <- rbind(a, b) # stack the two datasets together - then drop duplicates


### Inspect duplicates
# inspect
# This id comes up in duplicates data, but it comes up only once in the original data
# WHY DUPLICATE?
inspect_a <- a |> filter(bvdid == "AEFEB33018") 
inspect_b <- b |> filter(bvdid == "AEFEB33018")
inspect_d <- d |> filter(bvdid == "AEFEB33018")


## Function that creates column that lists all columns for which values across rows are different
func <- function(X) {
  paste(names(
    Filter(function(z) length(z) > 1,
           lapply(X, unique))
  ), collapse = ";")
}

# get columns with different values
get_col_insp <- inspect_d %>% mutate(col_diff = func(cur_data()))

d <- d[!duplicated(d), ]

#sum(duplicated(d$bvdid))
#View(d[duplicated(d$bvdid)|duplicated(d$bvdid, fromLast=T), ]) # bunch of duplicated bvdids, single isin

#sum(duplicated(d$gvkey))
#View(d[duplicated(d$gvkey)|duplicated(d$gvkey, fromLast=T), ]) # bunch of duplicate gvkeys, single isin


duplicates <- d[(duplicated(d$bvdid)|duplicated(d$bvdid, fromLast=T)) | (duplicated(d$gvkey)|duplicated(d$gvkey, fromLast=T)), ]
names(duplicates)
duplicates <- duplicates[, c("bvdid", "isin", "cc_expo_ew_2016",
                             "gvkey", "hqcountrycode", "conm",
                             "total_assets_usd_2016", "client_uuid", "client_name")]
write.csv(duplicates, "xx_other/exposure_orbis_bvdidORgvkey_duplicated.csv", row.names=F)
## these are the cases where we do have a match between orbis-lobbying (either gvkey or bvdid),
## but the relationship between gvkey/bvdid <-> isin is NOT one-to-one

# get columns with different values
get_col <- d %>% 
  group_by(bvdid) %>%
  mutate(col_diff = func(cur_data())) %>%
  ungroup() %>%
  filter(bvdid %in% duplicates$bvdid)

table(get_col$col_diff)


d <- d[!(duplicated(d$bvdid)|duplicated(d$bvdid, fromLast=T)), ]## need to OMIT for now those cases which have multiple bvdids to a single isin
d <- d[!(duplicated(d$gvkey)|duplicated(d$gvkey, fromLast=T)), ]## need to OMIT for now those cases which have multiple gvkeys to a single isin

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

timespan <- 1999:2023

time_varying <- c("cc_expo_ew_", "cc_risk_ew_", "cc_pos_ew_", "cc_neg_ew_",
                  "cc_sent_ew_", "op_expo_ew_", "op_risk_ew_", "op_pos_ew_",
                  "op_neg_ew_", "op_sent_ew_", "rg_expo_ew_", "rg_risk_ew_",
                  "rg_pos_ew_", "rg_neg_ew_", "rg_sent_ew_", "ph_expo_ew_", 
                  "ph_risk_ew_", "ph_pos_ew_", "ph_neg_ew_", "ph_sent_ew_",
                  "total_assets_usd_", "n_employees_", "operating_rev_usd_",
                  "P_L_b4tax_usd_")

moving_list <- lapply(time_varying, function(x) paste0(x, timespan))

# adding columns with all missing values where necessary to get balanced 
# time-dependent variables (needed for reshape function)
missing_cols <- unlist(moving_list)[!(unlist(moving_list) %in% names(exposure_orbis_lobby_wide))]
exposure_orbis_lobby_wide[, missing_cols] <- NA

## reshape data from wide to long format
exposure_orbis_lobby_long <- reshape(exposure_orbis_lobby_wide,
                                     direction="long",
                                     varying=moving_list,
                                     times=timespan,
                                     timevar="year",
                                     idvar="isin")

names(exposure_orbis_lobby_long) <- gsub("_1999", "", names(exposure_orbis_lobby_long))

# summary(exposure_orbis_lobby_long$op_expo_ew[which(exposure_orbis_lobby_long$year==2010)])
# summary(exposure_orbis_lobby_wide$op_expo_ew_2010)
# 
# exposure_orbis_lobby_long$n_employees <- as.numeric(exposure_orbis_lobby_long$n_employees)
# exposure_orbis_lobby_wide$n_employees_2010 <- as.numeric(exposure_orbis_lobby_wide$n_employees_2010)
# summary(exposure_orbis_lobby_long$n_employees[which(exposure_orbis_lobby_long$year==2010)])
# summary(exposure_orbis_lobby_wide$n_employees_2010)
## ^ all suggest that the reshape was successful

write.csv(exposure_orbis_lobby_wide, "02_processed/exposure_orbis_client_wide_REVISE.csv", row.names = F)
write.csv(exposure_orbis_lobby_long, "02_processed/exposure_orbis_client_long_REVISE.csv", row.names = F)

## want to think about whether we want to assume that all firms in exposure-orbis, but 
## that cant be matched with LobbyView, are actually just all NON LOBBYING firms??? Do 
## we want to include these in our final dataset as such???

## 6 cases of duplicated client_uuid in orbis wide data. Want to remove? YES. Done



