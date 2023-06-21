
# identify set of firms from Sautner that have marriage with lobbyview

sautner <- read.csv("/Users/christianbaehr/Dropbox/BBH/BBH1/data/CC Exposure/cc_firmquarter_2021Q4_03082021_OSF (1).csv", stringsAsFactors = F)
sum(is.na(sautner$gvkey))
sum(duplicated(sautner[, c("gvkey", "year", "quarter")])) #5535 duplicates
sautner <- sautner[!is.na(sautner$gvkey), ]
sautner <- sautner[!duplicated(sautner$gvkey), ]



lobby <- read.csv("/Users/christianbaehr/Dropbox/BBH/BBH1/data/LOBBYING LobbyView/dataset___client_level.csv", stringsAsFactors = F)
lobby <- lobby[!is.na(lobby$gvkey), ]
sum(duplicated(lobby$gvkey))
View(lobby[duplicated(lobby$gvkey) | duplicated(lobby$gvkey, fromLast = T), ])

m <- merge(sautner, lobby, by="gvkey")

View(lobby[!lobby$gvkey %in% m$gvkey, ])

a<- sort(lobby$gvkey[lobby$gvkey %in% m$gvkey])
b <- sort(sautner$gvkey[!sautner$gvkey %in% m$gvkey])

a <- c(a, rep(NA, length(b)-length(a)))
View(cbind(a, b))

cstat <- read.csv("/Users/christianbaehr/Dropbox/BBH/BBH1/data/Misc/compustat_northamerica.csv", stringsAsFactors = F)
View(cstat[which(cstat$gvkey %in% lobbydup$gvkey), ])


lobbydup <- lobby[duplicated(lobby$gvkey) | duplicated(lobby$gvkey, fromLast = T), ]
length(unique(lobbydup$gvkey))





