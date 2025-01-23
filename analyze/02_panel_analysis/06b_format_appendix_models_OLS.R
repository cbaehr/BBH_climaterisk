
rm(list = ls())
options("modelsummary_format_numeric_latex" = "plain")
pacman::p_load(tidyverse, modelsummary, fixest)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

stars <- function(x, ldv=F) {
  op <- x[1] / x[2]
  rg <- x[3] / x[4]
  ph <- x[5] / x[6]
  opstar <- ifelse(abs(op)>=2.576, "***", ifelse(abs(op)>=1.96, "**", ifelse(abs(op)>=1.645, "*", "")))
  rgstar <- ifelse(abs(rg)>=2.576, "***", ifelse(abs(rg)>=1.96, "**", ifelse(abs(rg)>=1.645, "*", "")))
  phstar <- ifelse(abs(ph)>=2.576, "***", ifelse(abs(ph)>=1.96, "**", ifelse(abs(ph)>=1.645, "*", "")))
  out <- round(x, 3)
  out[1] <- paste0(out[1], opstar)
  out[3] <- paste0(out[3], rgstar)
  out[5] <- paste0(out[5], phstar)
  #out_df <- data.frame(Coef=c(out[c(1, 3, 5)]), SE=c(out[c(2, 4, 6)]))
  #out_df <- data.frame(term=c("Oppo.", "Reg.", "Phy."), estimate=c(x[c(1, 3, 5)]), std.error=c(x[c(2, 4, 6)]))
  out_df <- data.frame(term=c("Opportunity", "Regulatory", "Physical"), estimate=c(out[c(1, 3, 5)]), std.error=c(out[c(2, 4, 6)]))
  if(length(x)>=7) {
    if(names(x)[7]=="op_expo_ew:rg_expo_ew") {
      oprg <- x[7] / x[8]
      opph <- x[9] / x[10]
      rgph <- x[11] / x[12]
      oprgstar <- ifelse(abs(oprg)>=2.576, "***", ifelse(abs(oprg)>=1.96, "**", ifelse(abs(oprg)>=1.645, "*", "")))
      opphstar <- ifelse(abs(opph)>=2.576, "***", ifelse(abs(opph)>=1.96, "**", ifelse(abs(opph)>=1.645, "*", "")))
      rgphstar <- ifelse(abs(rgph)>=2.576, "***", ifelse(abs(rgph)>=1.96, "**", ifelse(abs(rgph)>=1.645, "*", "")))
      #out <- round(x, 3)
      out[7] <- paste0(out[7], oprgstar)
      out[9] <- paste0(out[9], opphstar)
      out[11] <- paste0(out[11], rgphstar)
      
      out_df_append <- data.frame(term=c("Op. x Rg.", "Op. x Ph.", "Rg. x Ph."), 
                                  estimate=c(out[c(7, 9, 11)]), std.error=c(out[c(8, 10, 12)]))
      out_df <- rbind(out_df, out_df_append)
    } else if(ldv) {
      ldv_t <- x[7] / x[8]
      ldvstar <- ifelse(abs(ldv_t)>=2.576, "***", ifelse(abs(ldv_t)>=1.96, "**", ifelse(abs(ldv_t)>=1.645, "*", "")))
      out[7] <- paste0(out[7], ldvstar)
      
      out_df_append <- data.frame(term="Lagged DV", 
                                  estimate=out[7], std.error=out[8])
      out_df <- rbind(out_df, out_df_append)
      rownames(out_df)[4] <- "lagged_dv"
    }
  } 
  return(out_df)
}

compute_wald <- function(fixest_mod, var1, var2) {
  a <- fixest_mod$coefficients[var1] #var1 coef
  b <- fixest_mod$coefficients[var2] #var2 coef
  a_var <- vcov(fixest_mod)[var1, var1] #var1 variance
  b_var <- vcov(fixest_mod)[var2, var2] #var2 variance
  ab_cov <- vcov(fixest_mod)[var1, var2] #var1-2 covariance
  wald <- a-b / sqrt(a_var + b_var - 2 * ab_cov) #wald stat
  return(wald)
}

process_stata <- function(output, colnum, type) {
  if(type=="exposure") {
    op_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=op_expo_ew"), colnum]))
    op_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=op_expo_ew")+1, colnum]))
  } else if(type=="sentiment") {
    op_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=op_sent_ew"), colnum]))
    op_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=op_sent_ew")+1, colnum]))
  } else {
    op_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=op_risk_ew"), colnum]))
    op_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=op_risk_ew")+1, colnum]))
  }
  
  op_se <- op_coef/op_tstat
  
  if(type=="exposure") {
    rg_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=rg_expo_ew"), colnum]))
    rg_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=rg_expo_ew")+1, colnum]))
  } else if(type=="sentiment") {
    rg_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=rg_sent_ew"), colnum]))
    rg_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=rg_sent_ew")+1, colnum]))
  } else {
    rg_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=rg_risk_ew"), colnum]))
    rg_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=rg_risk_ew")+1, colnum]))
  }
  rg_se <- rg_coef/rg_tstat
  
  if(type=="exposure") {
    ph_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=ph_expo_ew"), colnum]))
    ph_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=ph_expo_ew")+1, colnum]))
  } else if(type=="sentiment") {
    ph_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=ph_sent_ew"), colnum]))
    ph_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=ph_sent_ew")+1, colnum]))
  } else {
    ph_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=ph_risk_ew"), colnum]))
    ph_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=ph_risk_ew")+1, colnum]))
  }
  ph_se <- ph_coef/ph_tstat
  
  if( type!="exposure") {
    out <- c(op_coef, op_se, rg_coef, rg_se, ph_coef, ph_se)
  } else if( output[which(output$X. == "=opp_reg"), colnum] != "=") {
    
    op_rg_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=opp_reg"), colnum]))
    op_rg_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=opp_reg")+1, colnum]))
    op_rg_se <- op_rg_coef / op_rg_tstat
    
    op_ph_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=opp_phy"), colnum]))
    op_ph_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=opp_phy")+1, colnum]))
    op_ph_se <- op_ph_coef / op_ph_tstat
    
    rg_ph_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=reg_phy"), colnum]))
    rg_ph_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=reg_phy")+1, colnum]))
    rg_ph_se <- rg_ph_coef / rg_ph_tstat
    
    out <- c(op_coef, op_se, rg_coef, rg_se, ph_coef, ph_se, 
             op_rg_coef, op_rg_se, op_ph_coef, op_ph_se, rg_ph_coef, rg_ph_se)
    names(out) <- c("op_expo_ew", "op_expo_ew", "rg_expo_ew", "rg_expo_ew", "ph_expo_ew", "ph_expo_ew", 
                    "op_expo_ew:rg_expo_ew", "op_expo_ew:rg_expo_ew", "op_expo_ew:ph_expo_ew", "op_expo_ew:ph_expo_ew",
                    "rg_expo_ew:ph_expo_ew", "rg_expo_ew:ph_expo_ew")
  } else {
    out <- c(op_coef, op_se, rg_coef, rg_se, ph_coef, ph_se)
  }
  return(out)
}

## Occurrence --------------------------------------------------


## Main OLS ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION.RData")
o_q_iy <- models[[5]] #Column 5 - main result for firm-quarter panel, industry-by-year FE

o_q_iy_ready <- c(o_q_iy$coefficients["op_expo_ew"], o_q_iy$se["op_expo_ew"],
                  o_q_iy$coefficients["rg_expo_ew"], o_q_iy$se["rg_expo_ew"],
                  o_q_iy$coefficients["ph_expo_ew"], o_q_iy$se["ph_expo_ew"])

o_q_iy_N <- o_q_iy$nobs

o_q_iy_R <- round(r2(o_q_iy, type = "ar2"), 3) #adjusted R2

o_q_iy_Wald <- c(round(compute_wald(o_q_iy, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(o_q_iy, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(o_q_iy, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

o_q_iy_out <- c(`Num. Obs.` = o_q_iy_N,
                `Adjusted R-Squared` =  o_q_iy_R,
                `Industry x Year FE` = '\\checkmark',
                `Firm FE` = ' ',
                `Firm Controls` = '\\checkmark',
                `Lagged DV` = ' ',
                `Climate Measure` = 'Exposure',
                `Estimation` = 'OLS',
                `Panel` = 'Firm-Qtr.',
                `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_Wald[1]),
                `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_Wald[2]),
                `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_Wald[3]))

## Occ: Year FE ---------------------------------------------------

o_q_iyf <- models[[7]] #Column 7 - add year FE

o_q_iyf_ready <- c(o_q_iyf$coefficients["op_expo_ew"], o_q_iyf$se["op_expo_ew"],
                  o_q_iyf$coefficients["rg_expo_ew"], o_q_iyf$se["rg_expo_ew"],
                  o_q_iyf$coefficients["ph_expo_ew"], o_q_iyf$se["ph_expo_ew"])

o_q_iyf_N <- o_q_iyf$nobs

o_q_iyf_R <- round(r2(o_q_iyf, type = "ar2"), 3) #adjusted R2

o_q_iyf_Wald <- c(round(compute_wald(o_q_iyf, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(o_q_iyf, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(o_q_iyf, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

o_q_iyf_out <- c(`Num. Obs.` = o_q_iyf_N,
                `Adjusted R-Squared` =  o_q_iyf_R,
                `Industry x Year FE` = '\\checkmark',
                `Firm FE` = '\\checkmark',
                `Firm Controls` = '\\checkmark',
                `Lagged DV` = ' ',
                `Climate Measure` = 'Exposure',
                `Estimation` = 'OLS',
                `Panel` = 'Firm-Qtr.',
                `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iyf_Wald[1]),
                `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iyf_Wald[2]),
                `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iyf_Wald[3]))

## Occ: Sentiment ---------------------------------------------------

o_q_sent_iy <- models[[9]] #Column 9 - industry-by-year FE with sentiment coefs

o_q_sent_iy_ready <- c(o_q_sent_iy$coefficients["op_sent_ew"], o_q_sent_iy$se["op_sent_ew"],
                  o_q_sent_iy$coefficients["rg_sent_ew"], o_q_sent_iy$se["rg_sent_ew"],
                  o_q_sent_iy$coefficients["ph_sent_ew"], o_q_sent_iy$se["ph_sent_ew"])

o_q_sent_iy_N <- o_q_sent_iy$nobs

o_q_sent_iy_R <- round(r2(o_q_sent_iy, type = "ar2"), 3) #adjusted R2

o_q_sent_iy_Wald <- c(round(compute_wald(o_q_sent_iy, "op_sent_ew", "rg_sent_ew"), 3), 
                  round(compute_wald(o_q_sent_iy, "op_sent_ew", "ph_sent_ew"), 3), 
                  round(compute_wald(o_q_sent_iy, "rg_sent_ew", "ph_sent_ew"), 3)) #Wald stats

o_q_sent_iy_out <- c(`Num. Obs.` = o_q_sent_iy_N,
                 `Adjusted R-Squared` =  o_q_sent_iy_R,
                 `Industry x Year FE` = '\\checkmark',
                 `Firm FE` = ' ',
                 `Firm Controls` = '\\checkmark',
                 `Lagged DV` = ' ',
                 `Climate Measure` = 'Sentiment',
                 `Estimation` = 'OLS',
                 `Panel` = 'Firm-Qtr.',
                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_sent_iy_Wald[1]),
                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_sent_iy_Wald[2]),
                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_sent_iy_Wald[3]))

## Occ: Risk ---------------------------------------------------

o_q_risk_iy <- models[[10]] #Column 10 - industry-by-year FE with risk coefs

o_q_risk_iy_ready <- c(o_q_risk_iy$coefficients["op_risk_ew"], o_q_risk_iy$se["op_risk_ew"],
                       o_q_risk_iy$coefficients["rg_risk_ew"], o_q_risk_iy$se["rg_risk_ew"],
                       o_q_risk_iy$coefficients["ph_risk_ew"], o_q_risk_iy$se["ph_risk_ew"])

o_q_risk_iy_N <- o_q_risk_iy$nobs

o_q_risk_iy_R <- round(r2(o_q_risk_iy, type = "ar2"), 3) #adjusted R2

o_q_risk_iy_Wald <- c(round(compute_wald(o_q_risk_iy, "op_risk_ew", "rg_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy, "op_risk_ew", "ph_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy, "rg_risk_ew", "ph_risk_ew"), 3)) #Wald stats

o_q_risk_iy_out <- c(`Num. Obs.` = o_q_risk_iy_N,
                     `Adjusted R-Squared` =  o_q_risk_iy_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = ' ',
                     `Climate Measure` = 'Risk',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_risk_iy_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_risk_iy_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_risk_iy_Wald[3]))

## Occ: Interact ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_interaction_MODELS_REVISION.RData")

o_q_iy_intr <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_intr_ready <- c(o_q_iy_intr$coefficients["op_expo_ew"], o_q_iy_intr$se["op_expo_ew"],
                       o_q_iy_intr$coefficients["rg_expo_ew"], o_q_iy_intr$se["rg_expo_ew"],
                       o_q_iy_intr$coefficients["ph_expo_ew"], o_q_iy_intr$se["ph_expo_ew"],
                       o_q_iy_intr$coefficients["op_expo_ew:rg_expo_ew"], o_q_iy_intr$se["op_expo_ew:rg_expo_ew"],
                       o_q_iy_intr$coefficients["op_expo_ew:ph_expo_ew"], o_q_iy_intr$se["op_expo_ew:ph_expo_ew"],
                       o_q_iy_intr$coefficients["rg_expo_ew:ph_expo_ew"], o_q_iy_intr$se["rg_expo_ew:ph_expo_ew"])

o_q_iy_intr_N <- o_q_iy_intr$nobs

o_q_iy_intr_R <- round(r2(o_q_iy_intr, type = "ar2"), 3) #adjusted R2

o_q_iy_intr_Wald <- c(round(compute_wald(o_q_iy_intr, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_intr, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_intr, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_intr_out <- c(`Num. Obs.` = o_q_iy_intr_N,
                     `Adjusted R-Squared` =  o_q_iy_intr_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = ' ',
                     `Climate Measure` = 'Exposure',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_intr_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_intr_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_intr_Wald[3]))

## Occ: Lagged DV ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_laggeddv_MODELS_REVISION.RData")

o_q_iy_ldv <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_ldv_ready <- c(o_q_iy_ldv$coefficients["op_expo_ew"], o_q_iy_ldv$se["op_expo_ew"],
                       o_q_iy_ldv$coefficients["rg_expo_ew"], o_q_iy_ldv$se["rg_expo_ew"],
                       o_q_iy_ldv$coefficients["ph_expo_ew"], o_q_iy_ldv$se["ph_expo_ew"],
                       o_q_iy_ldv$coefficients["CLI_l1"], o_q_iy_ldv$se["CLI_l1"])

o_q_iy_ldv_N <- o_q_iy_ldv$nobs

o_q_iy_ldv_R <- round(r2(o_q_iy_ldv, type = "ar2"), 3) #adjusted R2

o_q_iy_ldv_Wald <- c(round(compute_wald(o_q_iy_ldv, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_ldv, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_ldv, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_ldv_out <- c(`Num. Obs.` = o_q_iy_ldv_N,
                     `Adjusted R-Squared` =  o_q_iy_ldv_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = '\\checkmark',
                     `Climate Measure` = 'Exposure',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_ldv_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_ldv_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_ldv_Wald[3]))

## Occ: Congress Target OLS ------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_target_MODELS_REVISION.RData")

o_q_iy_cong <- models[["CONG"]]


o_q_iy_cong_ready <- c(o_q_iy_cong$coefficients["op_expo_ew"], o_q_iy_cong$se["op_expo_ew"],
                       o_q_iy_cong$coefficients["rg_expo_ew"], o_q_iy_cong$se["rg_expo_ew"],
                       o_q_iy_cong$coefficients["ph_expo_ew"], o_q_iy_cong$se["ph_expo_ew"])

o_q_iy_cong_N <- o_q_iy_cong$nobs

o_q_iy_cong_R <- round(r2(o_q_iy_cong, type = "ar2"), 3) #adjusted R2

o_q_iy_cong_Wald <- c(round(compute_wald(o_q_iy_cong, "op_expo_ew", "rg_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_cong, "op_expo_ew", "ph_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_cong, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_cong_out <- c(`Num. Obs.` = o_q_iy_cong_N,
                    `Adjusted R-Squared` =  o_q_iy_cong_R,
                    `Industry x Year FE` = '\\checkmark',
                    `Firm FE` = ' ',
                    `Firm Controls` = '\\checkmark',
                    `Lagged DV` = ' ',
                    `Climate Measure` = 'Exposure',
                    `Estimation` = 'OLS',
                    `Panel` = 'Firm-Qtr.',
                    `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_cong_Wald[1]),
                    `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_cong_Wald[2]),
                    `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_cong_Wald[3]))

## Occ: EPA Target OLS ------------------------------------------------

o_q_iy_epa <- models[["EPA"]]


o_q_iy_epa_ready <- c(o_q_iy_epa$coefficients["op_expo_ew"], o_q_iy_epa$se["op_expo_ew"],
                      o_q_iy_epa$coefficients["rg_expo_ew"], o_q_iy_epa$se["rg_expo_ew"],
                      o_q_iy_epa$coefficients["ph_expo_ew"], o_q_iy_epa$se["ph_expo_ew"])

o_q_iy_epa_N <- o_q_iy_epa$nobs

o_q_iy_epa_R <- round(r2(o_q_iy_epa, type = "ar2"), 3) #adjusted R2

o_q_iy_epa_Wald <- c(round(compute_wald(o_q_iy_epa, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_epa, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_epa, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_epa_out <- c(`Num. Obs.` = o_q_iy_epa_N,
                     `Adjusted R-Squared` =  o_q_iy_epa_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = ' ',
                     `Climate Measure` = 'Exposure',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_epa_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_epa_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_epa_Wald[3]))

## Occ: DOE Target OLS ------------------------------------------------

o_q_iy_doe <- models[["DOE"]]


o_q_iy_doe_ready <- c(o_q_iy_doe$coefficients["op_expo_ew"], o_q_iy_doe$se["op_expo_ew"],
                      o_q_iy_doe$coefficients["rg_expo_ew"], o_q_iy_doe$se["rg_expo_ew"],
                      o_q_iy_doe$coefficients["ph_expo_ew"], o_q_iy_doe$se["ph_expo_ew"])

o_q_iy_doe_N <- o_q_iy_doe$nobs

o_q_iy_doe_R <- round(r2(o_q_iy_doe, type = "ar2"), 3) #adjusted R2

o_q_iy_doe_Wald <- c(round(compute_wald(o_q_iy_doe, "op_expo_ew", "rg_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_doe, "op_expo_ew", "ph_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_doe, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_doe_out <- c(`Num. Obs.` = o_q_iy_doe_N,
                    `Adjusted R-Squared` =  o_q_iy_doe_R,
                    `Industry x Year FE` = '\\checkmark',
                    `Firm FE` = ' ',
                    `Firm Controls` = '\\checkmark',
                    `Lagged DV` = ' ',
                    `Climate Measure` = 'Exposure',
                    `Estimation` = 'OLS',
                    `Panel` = 'Firm-Qtr.',
                    `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_doe_Wald[1]),
                    `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_doe_Wald[2]),
                    `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_doe_Wald[3]))



## Amount --------------------------------------------------




## Amt: Main OLS ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION.RData")
o_q_iy_amt <- models[[5]] #Column 5 - main result for firm-quarter panel, industry-by-year FE

o_q_iy_amt_ready <- c(o_q_iy_amt$coefficients["op_expo_ew"], o_q_iy_amt$se["op_expo_ew"],
                  o_q_iy_amt$coefficients["rg_expo_ew"], o_q_iy_amt$se["rg_expo_ew"],
                  o_q_iy_amt$coefficients["ph_expo_ew"], o_q_iy_amt$se["ph_expo_ew"])

o_q_iy_amt_N <- o_q_iy_amt$nobs

o_q_iy_amt_R <- round(r2(o_q_iy_amt, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_Wald <- c(round(compute_wald(o_q_iy_amt, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(o_q_iy_amt, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(o_q_iy_amt, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

o_q_iy_amt_out <- c(`Num. Obs.` = o_q_iy_amt_N,
                `Adjusted R-Squared` =  o_q_iy_amt_R,
                `Industry x Year FE` = '\\checkmark',
                `Firm FE` = ' ',
                `Firm Controls` = '\\checkmark',
                `Lagged DV` = ' ',
                `Climate Measure` = 'Exposure',
                `Estimation` = 'OLS',
                `Panel` = 'Firm-Qtr.',
                `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_Wald[1]),
                `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_Wald[2]),
                `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_Wald[3]))

## Amt: Year FE ---------------------------------------------------

o_q_iyf_amt <- models[[7]] #Column 7 - add year FE

o_q_iyf_amt_ready <- c(o_q_iyf_amt$coefficients["op_expo_ew"], o_q_iyf_amt$se["op_expo_ew"],
                  o_q_iyf_amt$coefficients["rg_expo_ew"], o_q_iyf_amt$se["rg_expo_ew"],
                  o_q_iyf_amt$coefficients["ph_expo_ew"], o_q_iyf_amt$se["ph_expo_ew"])

o_q_iyf_amt_N <- o_q_iyf_amt$nobs

o_q_iyf_amt_R <- round(r2(o_q_iyf_amt, type = "ar2"), 3) #adjusted R2

o_q_iyf_amt_Wald <- c(round(compute_wald(o_q_iyf_amt, "op_expo_ew", "rg_expo_ew"), 3), 
                  round(compute_wald(o_q_iyf_amt, "op_expo_ew", "ph_expo_ew"), 3), 
                  round(compute_wald(o_q_iyf_amt, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

o_q_iyf_amt_out <- c(`Num. Obs.` = o_q_iyf_amt_N,
                 `Adjusted R-Squared` =  o_q_iyf_amt_R,
                 `Industry x Year FE` = '\\checkmark',
                 `Firm FE` = '\\checkmark',
                 `Firm Controls` = '\\checkmark',
                 `Lagged DV` = ' ',
                 `Climate Measure` = 'Exposure',
                 `Estimation` = 'OLS',
                 `Panel` = 'Firm-Qtr.',
                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iyf_amt_Wald[1]),
                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iyf_amt_Wald[2]),
                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iyf_amt_Wald[3]))

## Amt: Sentiment ---------------------------------------------------

o_q_sent_iy_amt <- models[[9]] #Column 9 - industry-by-year FE with sentiment coefs

o_q_sent_iy_amt_ready <- c(o_q_sent_iy_amt$coefficients["op_sent_ew"], o_q_sent_iy_amt$se["op_sent_ew"],
                           o_q_sent_iy_amt$coefficients["rg_sent_ew"], o_q_sent_iy_amt$se["rg_sent_ew"],
                           o_q_sent_iy_amt$coefficients["ph_sent_ew"], o_q_sent_iy_amt$se["ph_sent_ew"])

o_q_sent_iy_amt_N <- o_q_sent_iy_amt$nobs

o_q_sent_iy_amt_R <- round(r2(o_q_sent_iy_amt, type = "ar2"), 3) #adjusted R2

o_q_sent_iy_amt_Wald <- c(round(compute_wald(o_q_sent_iy_amt, "op_sent_ew", "rg_sent_ew"), 3), 
                      round(compute_wald(o_q_sent_iy_amt, "op_sent_ew", "ph_sent_ew"), 3), 
                      round(compute_wald(o_q_sent_iy_amt, "rg_sent_ew", "ph_sent_ew"), 3)) #Wald stats

o_q_sent_iy_amt_out <- c(`Num. Obs.` = o_q_sent_iy_amt_N,
                     `Adjusted R-Squared` =  o_q_sent_iy_amt_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = ' ',
                     `Climate Measure` = 'Sentiment',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_sent_iy_amt_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_sent_iy_amt_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_sent_iy_amt_Wald[3]))

## Amt: Risk ---------------------------------------------------

o_q_risk_iy_amt <- models[[10]] #Column 10 - industry-by-year FE with risk coefs

o_q_risk_iy_amt_ready <- c(o_q_risk_iy_amt$coefficients["op_risk_ew"], o_q_risk_iy_amt$se["op_risk_ew"],
                           o_q_risk_iy_amt$coefficients["rg_risk_ew"], o_q_risk_iy_amt$se["rg_risk_ew"],
                           o_q_risk_iy_amt$coefficients["ph_risk_ew"], o_q_risk_iy_amt$se["ph_risk_ew"])

o_q_risk_iy_amt_N <- o_q_risk_iy_amt$nobs

o_q_risk_iy_amt_R <- round(r2(o_q_risk_iy_amt, type = "ar2"), 3) #adjusted R2

o_q_risk_iy_amt_Wald <- c(round(compute_wald(o_q_risk_iy_amt, "op_risk_ew", "rg_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy_amt, "op_risk_ew", "ph_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy_amt, "rg_risk_ew", "ph_risk_ew"), 3)) #Wald stats

o_q_risk_iy_amt_out <- c(`Num. Obs.` = o_q_risk_iy_amt_N,
                     `Adjusted R-Squared` =  o_q_risk_iy_amt_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = ' ',
                     `Climate Measure` = 'Risk',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_risk_iy_amt_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_risk_iy_amt_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_risk_iy_amt_Wald[3]))

## Amt: Interact ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_interaction_amount_MODELS_REVISION.RData")

o_q_iy_amt_intr <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_intr_ready <- c(o_q_iy_amt_intr$coefficients["op_expo_ew"], o_q_iy_amt_intr$se["op_expo_ew"],
                           o_q_iy_amt_intr$coefficients["rg_expo_ew"], o_q_iy_amt_intr$se["rg_expo_ew"],
                           o_q_iy_amt_intr$coefficients["ph_expo_ew"], o_q_iy_amt_intr$se["ph_expo_ew"],
                           o_q_iy_amt_intr$coefficients["op_expo_ew:rg_expo_ew"], o_q_iy_amt_intr$se["op_expo_ew:rg_expo_ew"],
                           o_q_iy_amt_intr$coefficients["op_expo_ew:ph_expo_ew"], o_q_iy_amt_intr$se["op_expo_ew:ph_expo_ew"],
                           o_q_iy_amt_intr$coefficients["rg_expo_ew:ph_expo_ew"], o_q_iy_amt_intr$se["rg_expo_ew:ph_expo_ew"])

o_q_iy_amt_intr_N <- o_q_iy_amt_intr$nobs

o_q_iy_amt_intr_R <- round(r2(o_q_iy_amt_intr, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_intr_Wald <- c(round(compute_wald(o_q_iy_amt_intr, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_amt_intr, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_amt_intr, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_intr_out <- c(`Num. Obs.` = o_q_iy_amt_intr_N,
                     `Adjusted R-Squared` =  o_q_iy_amt_intr_R,
                     `Industry x Year FE` = '\\checkmark',
                     `Firm FE` = ' ',
                     `Firm Controls` = '\\checkmark',
                     `Lagged DV` = ' ',
                     `Climate Measure` = 'Exposure',
                     `Estimation` = 'OLS',
                     `Panel` = 'Firm-Qtr.',
                     `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_intr_Wald[1]),
                     `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_intr_Wald[2]),
                     `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_intr_Wald[3]))

## Amt: Lagged DV ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_laggeddv_amount_MODELS_REVISION.RData")

o_q_iy_amt_ldv <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_ldv_ready <- c(o_q_iy_amt_ldv$coefficients["op_expo_ew"], o_q_iy_amt_ldv$se["op_expo_ew"],
                          o_q_iy_amt_ldv$coefficients["rg_expo_ew"], o_q_iy_amt_ldv$se["rg_expo_ew"],
                          o_q_iy_amt_ldv$coefficients["ph_expo_ew"], o_q_iy_amt_ldv$se["ph_expo_ew"],
                          o_q_iy_amt_ldv$coefficients["log_CLI_amount_l1"], o_q_iy_amt_ldv$se["log_CLI_amount_l1"])

o_q_iy_amt_ldv_N <- o_q_iy_amt_ldv$nobs

o_q_iy_amt_ldv_R <- round(r2(o_q_iy_amt_ldv, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_ldv_Wald <- c(round(compute_wald(o_q_iy_amt_ldv, "op_expo_ew", "rg_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_amt_ldv, "op_expo_ew", "ph_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_amt_ldv, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_ldv_out <- c(`Num. Obs.` = o_q_iy_amt_ldv_N,
                    `Adjusted R-Squared` =  o_q_iy_amt_ldv_R,
                    `Industry x Year FE` = '\\checkmark',
                    `Firm FE` = ' ',
                    `Firm Controls` = '\\checkmark',
                    `Lagged DV` = '\\checkmark',
                    `Climate Measure` = 'Exposure',
                    `Estimation` = 'OLS',
                    `Panel` = 'Firm-Qtr.',
                    `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_ldv_Wald[1]),
                    `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_ldv_Wald[2]),
                    `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_ldv_Wald[3]))

## Amt: Congress Target ---------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_target_amount_MODELS_REVISION.RData")

o_q_iy_amt_cong <- models[["CONG"]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_cong_ready <- c(o_q_iy_amt_cong$coefficients["op_expo_ew"], o_q_iy_amt_cong$se["op_expo_ew"],
                          o_q_iy_amt_cong$coefficients["rg_expo_ew"], o_q_iy_amt_cong$se["rg_expo_ew"],
                          o_q_iy_amt_cong$coefficients["ph_expo_ew"], o_q_iy_amt_cong$se["ph_expo_ew"])

o_q_iy_amt_cong_N <- o_q_iy_amt_cong$nobs

o_q_iy_amt_cong_R <- round(r2(o_q_iy_amt_cong, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_cong_Wald <- c(round(compute_wald(o_q_iy_amt_cong, "op_expo_ew", "rg_expo_ew"), 3), 
                         round(compute_wald(o_q_iy_amt_cong, "op_expo_ew", "ph_expo_ew"), 3), 
                         round(compute_wald(o_q_iy_amt_cong, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_cong_out <- c(`Num. Obs.` = o_q_iy_amt_cong_N,
                        `Adjusted R-Squared` =  o_q_iy_amt_cong_R,
                        `Industry x Year FE` = '\\checkmark',
                        `Firm FE` = ' ',
                        `Firm Controls` = '\\checkmark',
                        `Lagged DV` = ' ',
                        `Climate Measure` = 'Exposure',
                        `Estimation` = 'OLS',
                        `Panel` = 'Firm-Qtr.',
                        `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_cong_Wald[1]),
                        `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_cong_Wald[2]),
                        `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_cong_Wald[3]))

## Amt: EPA Target ---------------------------------------------

o_q_iy_amt_epa <- models[["EPA"]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_epa_ready <- c(o_q_iy_amt_epa$coefficients["op_expo_ew"], o_q_iy_amt_epa$se["op_expo_ew"],
                          o_q_iy_amt_epa$coefficients["rg_expo_ew"], o_q_iy_amt_epa$se["rg_expo_ew"],
                          o_q_iy_amt_epa$coefficients["ph_expo_ew"], o_q_iy_amt_epa$se["ph_expo_ew"])

o_q_iy_amt_epa_N <- o_q_iy_amt_epa$nobs

o_q_iy_amt_epa_R <- round(r2(o_q_iy_amt_epa, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_epa_Wald <- c(round(compute_wald(o_q_iy_amt_epa, "op_expo_ew", "rg_expo_ew"), 3), 
                          round(compute_wald(o_q_iy_amt_epa, "op_expo_ew", "ph_expo_ew"), 3), 
                          round(compute_wald(o_q_iy_amt_epa, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_epa_out <- c(`Num. Obs.` = o_q_iy_amt_epa_N,
                         `Adjusted R-Squared` =  o_q_iy_amt_epa_R,
                         `Industry x Year FE` = '\\checkmark',
                         `Firm FE` = ' ',
                         `Firm Controls` = '\\checkmark',
                         `Lagged DV` = ' ',
                         `Climate Measure` = 'Exposure',
                         `Estimation` = 'OLS',
                         `Panel` = 'Firm-Qtr.',
                         `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_epa_Wald[1]),
                         `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_epa_Wald[2]),
                         `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_epa_Wald[3]))

## Amt: DOE Target ---------------------------------------------

o_q_iy_amt_doe <- models[["DOE"]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_doe_ready <- c(o_q_iy_amt_doe$coefficients["op_expo_ew"], o_q_iy_amt_doe$se["op_expo_ew"],
                          o_q_iy_amt_doe$coefficients["rg_expo_ew"], o_q_iy_amt_doe$se["rg_expo_ew"],
                          o_q_iy_amt_doe$coefficients["ph_expo_ew"], o_q_iy_amt_doe$se["ph_expo_ew"])

o_q_iy_amt_doe_N <- o_q_iy_amt_doe$nobs

o_q_iy_amt_doe_R <- round(r2(o_q_iy_amt_doe, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_doe_Wald <- c(round(compute_wald(o_q_iy_amt_doe, "op_expo_ew", "rg_expo_ew"), 3), 
                         round(compute_wald(o_q_iy_amt_doe, "op_expo_ew", "ph_expo_ew"), 3), 
                         round(compute_wald(o_q_iy_amt_doe, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_doe_out <- c(`Num. Obs.` = o_q_iy_amt_doe_N,
                        `Adjusted R-Squared` =  o_q_iy_amt_doe_R,
                        `Industry x Year FE` = '\\checkmark',
                        `Firm FE` = ' ',
                        `Firm Controls` = '\\checkmark',
                        `Lagged DV` = ' ',
                        `Climate Measure` = 'Exposure',
                        `Estimation` = 'OLS',
                        `Panel` = 'Firm-Qtr.',
                        `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_doe_Wald[1]),
                        `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_doe_Wald[2]),
                        `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_doe_Wald[3]))

## Combine and export --------------------------------------------------

o_q_iy_ready_stars <- stars(o_q_iy_ready)
o_q_iyf_ready_stars <- stars(o_q_iyf_ready)
o_q_iy_intr_ready_stars <- stars(o_q_iy_intr_ready)
o_q_iy_ldv_ready_stars <- stars(o_q_iy_ldv_ready, ldv=TRUE)
o_q_sent_iy_ready_stars <- stars(o_q_sent_iy_ready)
o_q_risk_iy_ready_stars <- stars(o_q_risk_iy_ready)
o_q_iy_cong_ready_stars <- stars(o_q_iy_cong_ready)
o_q_iy_epa_ready_stars <- stars(o_q_iy_epa_ready)
o_q_iy_doe_ready_stars <- stars(o_q_iy_doe_ready)

o_q_iy_amt_ready_stars <- stars(o_q_iy_amt_ready)
o_q_iyf_amt_ready_stars <- stars(o_q_iyf_amt_ready)
o_q_iy_amt_intr_ready_stars <- stars(o_q_iy_amt_intr_ready)
o_q_iy_amt_ldv_ready_stars <- stars(o_q_iy_amt_ldv_ready, ldv=T)
o_q_sent_iy_amt_ready_stars <- stars(o_q_sent_iy_amt_ready)
o_q_risk_iy_amt_ready_stars <- stars(o_q_risk_iy_amt_ready)
o_q_iy_amt_cong_ready_stars <- stars(o_q_iy_amt_cong_ready)
o_q_iy_amt_epa_ready_stars <- stars(o_q_iy_amt_epa_ready)
o_q_iy_amt_doe_ready_stars <- stars(o_q_iy_amt_doe_ready)


m1 <- list(tidy=o_q_iy_ready_stars); class(m1) <- "modelsummary_list"
m2 <- list(tidy=o_q_iyf_ready_stars); class(m2) <- "modelsummary_list"
m3 <- list(tidy=o_q_iy_intr_ready_stars); class(m3) <- "modelsummary_list"
m4 <- list(tidy=o_q_iy_ldv_ready_stars); class(m4) <- "modelsummary_list"
m5 <- list(tidy=o_q_sent_iy_ready_stars); class(m5) <- "modelsummary_list"
m6 <- list(tidy=o_q_risk_iy_ready_stars); class(m6) <- "modelsummary_list"
m13 <- list(tidy=o_q_iy_cong_ready_stars); class(m13) <- "modelsummary_list"
m14 <- list(tidy=o_q_iy_epa_ready_stars); class(m14) <- "modelsummary_list"
m15 <- list(tidy=o_q_iy_doe_ready_stars); class(m15) <- "modelsummary_list"

m7 <- list(tidy=o_q_iy_amt_ready_stars); class(m7) <- "modelsummary_list"
m8 <- list(tidy=o_q_iyf_amt_ready_stars); class(m8) <- "modelsummary_list"
m9 <- list(tidy=o_q_iy_amt_intr_ready_stars); class(m9) <- "modelsummary_list"
m10 <- list(tidy=o_q_iy_amt_ldv_ready_stars); class(m10) <- "modelsummary_list"
m11 <- list(tidy=o_q_sent_iy_amt_ready_stars); class(m11) <- "modelsummary_list"
m12 <- list(tidy=o_q_risk_iy_amt_ready_stars); class(m12) <- "modelsummary_list"
m16 <- list(tidy=o_q_iy_amt_cong_ready_stars); class(m16) <- "modelsummary_list"
m17 <- list(tidy=o_q_iy_amt_epa_ready_stars); class(m17) <- "modelsummary_list"
m18 <- list(tidy=o_q_iy_amt_doe_ready_stars); class(m18) <- "modelsummary_list"

mod_list <- list("OLS 1"=m1, "OLS 2"=m2, "OLS 3"=m3, "OLS 4"=m4, "OLS 5"=m5, "OLS 6"=m6, "OLS 13"=m13, "OLS 14"=m14, "OLS 15"=m15,
                 "OLS 7"=m7, "OLS 8"=m8, "OLS 9"=m9, "OLS 10"=m10, "OLS 11"=m11, "OLS 12"=m12, "OLS 16"=m16, "OLS 17"=m17, "OLS 18"=m18)
# mod_list <- list("Occurrence" = list("OLS 1"=m1, "OLS 2"=m2, "OLS 3"=m3, "OLS 4"=m4, "OLS 5"=m5, "OLS 6"=m6),
#                  "Amount" = list("OLS 7"=m7, "OLS 8"=m8, "OLS 9"=m9, "OLS 10"=m10, "OLS 11"=m11, "OLS 12"=m12))

auxiliary <- data.frame(o_q_iy_out, 
                        o_q_iyf_out,
                        o_q_iy_intr_out,
                        o_q_iy_ldv_out,
                        o_q_sent_iy_out,
                        o_q_risk_iy_out,
                        o_q_iy_cong_out,
                        o_q_iy_epa_out,
                        o_q_iy_doe_out,
                        o_q_iy_amt_out,
                        o_q_iyf_amt_out,
                        o_q_iy_amt_intr_out,
                        o_q_iy_amt_ldv_out,
                        o_q_sent_iy_amt_out,
                        o_q_risk_iy_amt_out,
                        o_q_iy_amt_cong_out,
                        o_q_iy_amt_epa_out,
                        o_q_iy_amt_doe_out)
model_names <- names(mod_list)
auxiliary <- rbind(auxiliary, "Model"=model_names)

auxiliary_out <- data.frame(t(auxiliary)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      Test == "Firm.Controls" ~ "Firm Controls",
      Test == "Estimation" ~ "Estimation",
      Test == "Climate.Measure" ~ "Climate Measure",
      Test == "Wald.Stat..Opp...Reg...0." ~ "Wald Stat (Op-Rg=0)",
      Test == "Wald.Stat..Opp...Phy...0." ~ "Wald Stat (Op-Ph=0)",
      Test == "Wald.Stat..Reg...Phy...0." ~ "Wald Stat (Rg-Ph=0)",
      Test == "Num..Obs." ~ "Num. Obs.",
      Test == "Panel" ~ "Panel",
      Test == "Lagged.DV" ~ "Lagged DV",
      #Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      Test == "Adjusted.R.Squared.apr2" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

names(mod_list) <- c("Occur.", "Occur.", "Occur.", "Occur.", "Occur.", "Occur.", "Occ. (Cong.)", "Occ. (EPA)", "Occ. (DOE)",
                     "Amount", "Amount", "Amount", "Amount", "Amount", "Amount", "Amt. (Cong.)", "Amt. (EPA)", "Amt. (DOE)")

modelsummary(mod_list
             ,add_rows=auxiliary_out
             ,output="results/tables/appendix_table_test_ols.tex"
)










