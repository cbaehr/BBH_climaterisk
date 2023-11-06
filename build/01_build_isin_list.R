
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")

exposurey <- read.csv("01_raw/exposure/cc_firmyear_2021Q4_03082021_OSF.csv", stringsAsFactors = F)
exposureq <- read.csv("01_raw/exposure/cc_firmquarter_2021Q4_03082021_OSF.csv", stringsAsFactors = F)

isin_list <- unique(c(exposurey$isin, exposureq$isin))
gvkey_list <- unique(c(exposurey$gvkey, exposureq$gvkey))

write.table(isin_list, "/Users/christianbaehr/Desktop/isin_list.txt", col.names=F, row.names=F)
write.table(gvkey_list, "/Users/christianbaehr/Desktop/gvkey_list.txt", col.names=F, row.names=F)
