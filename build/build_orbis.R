
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/01_raw/")

orbis1 <- readxl::read_xlsx("orbis/bbh_orbis_11_03_2023_1.xlsx", sheet = 2)
orbis2 <- readxl::read_xlsx("orbis/bbh_orbis_11_03_2023_2.xlsx", sheet = 2)
orbis3 <- readxl::read_xlsx("orbis/bbh_orbis_11_03_2023_3.xlsx", sheet = 2)

orbis <- do.call(rbind, list(orbis1, orbis2, orbis3))

names(orbis) <- c("row", "conm", "inactive", "quoted", "branch", "owndata",
                  "woco", "cty_iso_code", "nace_core_4digit", "consolidation_code",
                  "lastavail", "operating_rev_lastavail", "n_employees_lastavail",
                  "nace_main_section", "nace_primary", "nace_secondary",
                  "naics_2017_primary", "naics_2017_secondary", "naics_2022_core_4digit",
                  "naics_2022_primary", "naics_2022_secondary", "sic_core_3digit",
                  "sic_primary", "sic_secondary", "bvdid", "isin_ALL",
                  paste0("operating_rev_", 2023:1994),
                  "profit_loss_beforetax_lastavail",
                  paste0("profit_loss_beforetax_", 2023:1994),
                  paste0("total_asset_year", 1:29),
                  "n_employees_lastavail",
                  paste0("n_employees_", 1:29),
                  "isin")

write.csv(orbis, "../02_processed/orbis_11_03_2023.csv", row.names = F)
