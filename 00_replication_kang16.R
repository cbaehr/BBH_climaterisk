# Replication Kang 2016



library(haven)

policies <- read_dta("~/Dropbox (Princeton)/BBH/BBH1/data/Kang (2016) replication/Datafortheweb/IntermediateDatasets/Policies/policylist.dta")

bills <- read_dta("~/Dropbox (Princeton)/BBH/BBH1/data/Kang (2016) replication/Datafortheweb/IntermediateDatasets/Policies/billseclist.dta")

lobbying <- read_dta("~/Dropbox (Princeton)/BBH/BBH1/data/Kang (2016) replication/Datafortheweb/IntermediateDatasets/Lobbying/lobbyingbills_cleaned.dta")

entity <- read_dta("~/Dropbox (Princeton)/BBH/BBH1/data/Kang (2016) replication/Datafortheweb/IntermediateDatasets/Lobbying/sdata110th_client.dta")
