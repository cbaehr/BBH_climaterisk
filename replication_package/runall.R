
rm(list = ls())

if(Sys.info()["user"]=="vincentheddesheimer" ) {
  #setwd("~/Documents/GitHub/BBH_climaterisk/replication_package/")
  github <- "~/Documents/GitHub/BBH_climaterisk/replication_package/"
  results_path <- "~/Dropbox (Princeton)/BBH/BBH1"
}
if(Sys.info()["user"]=="christianbaehr" ) {
  #setwd("/Users/christianbaehr/Documents/GitHub/BBH_climaterisk/replication_package/")
  github <- "/Users/christianbaehr/Documents/GitHub/BBH_climaterisk/replication_package/"
  results_path <- "/Users/christianbaehr/Dropbox/BBH/BBH1/"
}

source_gh <- function(script, github_path=github) {
  rm(list = setdiff(ls(), c("script", "github_path", "source_gh", "github", "results_path")))
  setwd(github_path)
  source(script)
}


  ## Create necessary folder structure -------------------------------------------
  
  if(!dir.exists(paste0(results_path, "data/02_processed"))) {
    dir.create(paste0(results_path, "data/02_processed"))
  }
  if(!dir.exists(paste0(results_path, "data/03_final"))) {
    dir.create(paste0(results_path, "data/03_final"))
  }
  if(!dir.exists(paste0(results_path, "data/03_final/issues_texts"))) {
    dir.create(paste0(results_path, "data/03_final/issues_texts"))
  }
  if(!dir.exists(paste0(results_path, "data/xx_other"))) {
    dir.create(paste0(results_path, "data/xx_other"))
  }
  if(!dir.exists(paste0(results_path, "results/tables"))) {
    dir.create(paste0(results_path, "results/tables"))
  }
  if(!dir.exists(paste0(results_path, "results/tables/mobility"))) {
    dir.create(paste0(results_path, "results/tables/mobility"))
  }
  if(!dir.exists(paste0(results_path, "results/figures"))) {
    dir.create(paste0(results_path, "results/figures"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/regressions"))) {
    dir.create(paste0(results_path, "results/figures/regressions"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/panelmatch"))) {
    dir.create(paste0(results_path, "results/figures/panelmatch"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/descriptives"))) {
    dir.create(paste0(results_path, "results/figures/descriptives"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/did"))) {
    dir.create(paste0(results_path, "results/figures/did"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/exposures"))) {
    dir.create(paste0(results_path, "results/figures/exposures"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/sensitivity"))) {
    dir.create(paste0(results_path, "results/figures/sensitivity"))
  }
  if(!dir.exists(paste0(results_path, "results/figures/text_analysis"))) {
    dir.create(paste0(results_path, "results/figures/text_analysis"))
  }
  
  
  #list.files("build/")
  #source_gh("build/01_build_isin_list.R")
  #source_gh("build/02_build_orbis.R")
  #source_gh("build/03c_join_sautner_orbis_quarterly_new.R")
  #source_gh("build/04c_merge_w_lobbyview_NEW.R")
  #source_gh("build/06_data_quality_checks.R")
  
  
  source_gh("build/04d_keyword_climate_measure.R")
  source_gh("build/05b_normalize_variables_new.R")
  source_gh("build/08_merge_w_coalitions.R")
  source_gh("build/10_merge_w_direction.R")
  source_gh("build/10b_merge_w_direction_support.R")
  source_gh("build/10b_normalize_variables_new_w_direction.R")
  source_gh("build/10c_merge_w_direction_oppose.R")
  source_gh("build/10d_normalize_variables_new_w_direction.R")
  source_gh("build/10e_normalize_variables_new_w_direction_support.R")
  source_gh("build/10f_normalize_variables_new_kw.R")
  source_gh("build/11_build_placebo_df.R")
  source_gh("build/12_case_study_data_creation.R")
  source_gh("build/xx_retrieve_firms_insong.R")

