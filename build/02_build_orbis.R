
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/01_raw/")

orbis1 <- readxl::read_xlsx("orbis/bbh_orbis_11_06_2023_1_6000.xlsx", sheet = 2)
orbis2 <- readxl::read_xlsx("orbis/bbh_orbis_11_06_2023_6001_12000.xlsx", sheet = 2)
orbis3 <- readxl::read_xlsx("orbis/bbh_orbis_11_06_2023_12001_12969.xlsx", sheet = 2)

orbis <- do.call(rbind, list(orbis1, orbis2, orbis3))
#dropcols <- sprintf("P/L before tax\nth USD %i...%i", c(2023:1999), c(129:153))
#orbis <- orbis[, !names(orbis) %in% dropcols]


names(orbis) <- c("row", "conm", "inactive", "quoted", "branch", "owndata", "woco",
                  "country_iso_code", "nace_core_4digit", "consolidation_code",
                  "lastavail", "bvdsector", "nace_main_section",
                  "nace_primary", "nace_secondary", "naics_2017_core_4digit",
                  "naics_2017_primary", "naics_2017_secondary", "naics_2022_core_4digit",
                  "naics_2022_primary", "naics_2022_secondary", "sic_core_3digit",
                  "sic_primary", "sic_secondary", "bvdid", "bvdaccount", "isin",
                  "isin_all",
                  paste0("total_assets_usd_", 2023:1999),
                  paste0("n_employees_", 2023:1999),
                  paste0("operating_rev_usd_", 2023:1999),
                  paste0("P_L_b4tax_usd_", 2023:1999))


#write.csv(orbis, "../02_processed/orbis_11_03_2023.csv", row.names = F)
#write.csv(orbis, "../02_processed/orbis_11_05_2023.csv", row.names = F)
write.csv(orbis, "../02_processed/orbis_11_06_2023.csv", row.names = F)
