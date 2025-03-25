
rm(list = ls())
options("modelsummary_format_numeric_latex" = "plain")
#devtools::install_version("modelsummary", version = "1.2", repos = "http://cran.us.r-project.org")
pacman::p_load(tidyverse, modelsummary, fixest)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

stars <- function(x, ldv=F) {
  x <- x * 100
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
  out_df <- list(tidy=out_df)
  class(out_df) <- "modelsummary_list"
  return(out_df)
}

compute_wald <- function(fixest_mod, var1, var2) {
  a <- fixest_mod$coefficients[var1] #var1 coef
  b <- fixest_mod$coefficients[var2] #var2 coef
  a_var <- vcov(fixest_mod)[var1, var1] #var1 variance
  b_var <- vcov(fixest_mod)[var2, var2] #var2 variance
  ab_cov <- vcov(fixest_mod)[var1, var2] #var1-2 covariance
  wald <- (a-b) / sqrt(a_var + b_var - 2 * ab_cov) #wald stat
  return(abs(wald))
}

process_model <- function(model, 
                          Year_FE = ' ', 
                          IndbyYear_FE = '\\checkmark', 
                          IndbyQtr_FE = ' ',
                          Firm_FE = ' ', 
                          Firm_Controls = '\\checkmark',
                          Lagged_DV = ' ', 
                          Climate_Measure = 'Exposure', 
                          Estimation = 'OLS', 
                          Panel = 'Firm-Qtr') {
  
  estimates <- c(model$coefficients["op_expo_ew"], model$se["op_expo_ew"],
                 model$coefficients["rg_expo_ew"], model$se["rg_expo_ew"],
                 model$coefficients["ph_expo_ew"], model$se["ph_expo_ew"])
  
  N <- model$nobs
  R <- round(r2(model, type="ar2"), 3)
  
  Wald <- c(round(compute_wald(model, "op_expo_ew", "rg_expo_ew"), 3),
            round(compute_wald(model, "op_expo_ew", "ph_expo_ew"), 3), 
            round(compute_wald(model, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats
  
  diagnostics <- c(`Num. Obs.` = N,
                   `Adjusted R-Squared` =  unname(R),
                   `Year FE` = Year_FE,
                   `Industry x Year FE` = IndbyYear_FE,
                   `Industry x Qtr FE` = IndbyQtr_FE,
                   `Firm FE` = Firm_FE,
                   `Firm Controls` = Firm_Controls,
                   `Lagged DV` = Lagged_DV,
                   `Climate Measure` = Climate_Measure,
                   `Estimation` = Estimation,
                   `Panel` = Panel,
                   `Wald (Opp-Reg=0)` = as.character(Wald[1]),
                   `Wald (Opp-Phy=0)` = as.character(Wald[2]),
                   `Wald (Reg-Phy=0)` = as.character(Wald[3]))
  
  out <- list("Estimates" = estimates,
              "Diagnostics" = diagnostics)
  
  return(out)
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


## Occ: Year FE Only ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")
o_q_y <- models[[3]] #Column 3 - firm-quarter panel, only year FE

o_q_y_out <- process_model(o_q_y,
                           Year_FE = '\\checkmark',
                           IndbyYear_FE = ' ')

## Occ: Industry-by-Year FE ---------------------------------------------------

o_q_iy <- models[[5]] #Column 5 - main result for firm-quarter panel, industry-by-year FE

o_q_iy_out <- process_model(o_q_iy)

## Occ: Firm FE ---------------------------------------------------

o_q_iyf <- models[[7]] #Column 7 - add Firm FE

o_q_iyf_out <- process_model(o_q_iyf,
                             Firm_FE = '\\checkmark')

## Occ: Sentiment ---------------------------------------------------

o_q_sent_iy <- models[[9]] #Column 9 - industry-by-year FE with sentiment coefs

o_q_sent_iy_estimates <- c(o_q_sent_iy$coefficients["op_sent_ew"], o_q_sent_iy$se["op_sent_ew"],
                           o_q_sent_iy$coefficients["rg_sent_ew"], o_q_sent_iy$se["rg_sent_ew"],
                           o_q_sent_iy$coefficients["ph_sent_ew"], o_q_sent_iy$se["ph_sent_ew"])

o_q_sent_iy_N <- o_q_sent_iy$nobs

o_q_sent_iy_R <- unname(round(r2(o_q_sent_iy, type = "ar2"), 3)) #adjusted R2

o_q_sent_iy_Wald <- c(round(compute_wald(o_q_sent_iy, "op_sent_ew", "rg_sent_ew"), 3), 
                      round(compute_wald(o_q_sent_iy, "op_sent_ew", "ph_sent_ew"), 3), 
                      round(compute_wald(o_q_sent_iy, "rg_sent_ew", "ph_sent_ew"), 3)) #Wald stats

o_q_sent_iy_diagnostics <- c(`Num. Obs.` = o_q_sent_iy_N,
                 `Adjusted R-Squared` =  o_q_sent_iy_R,
                 `Year FE` = ' ',
                 `Industry x Year FE` = '\\checkmark',
                 `Industry x Qtr FE` = ' ',
                 `Firm FE` = ' ',
                 `Firm Controls` = '\\checkmark',
                 `Lagged DV` = ' ',
                 `Climate Measure` = 'Sentiment',
                 `Estimation` = 'OLS',
                 `Panel` = 'Firm-Qtr',
                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_sent_iy_Wald[1]),
                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_sent_iy_Wald[2]),
                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_sent_iy_Wald[3]))

o_q_iy_sent_out <- list("Estimates" = o_q_sent_iy_estimates,
                        "Diagnostics" = o_q_sent_iy_diagnostics)

## Occ: Risk ---------------------------------------------------

o_q_risk_iy <- models[[10]] #Column 10 - industry-by-year FE with risk coefs

o_q_risk_iy_estimates <- c(o_q_risk_iy$coefficients["op_risk_ew"], o_q_risk_iy$se["op_risk_ew"],
                           o_q_risk_iy$coefficients["rg_risk_ew"], o_q_risk_iy$se["rg_risk_ew"],
                           o_q_risk_iy$coefficients["ph_risk_ew"], o_q_risk_iy$se["ph_risk_ew"])

o_q_risk_iy_N <- o_q_risk_iy$nobs

o_q_risk_iy_R <- unname(round(r2(o_q_risk_iy, type = "ar2"), 3)) #adjusted R2

o_q_risk_iy_Wald <- c(round(compute_wald(o_q_risk_iy, "op_risk_ew", "rg_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy, "op_risk_ew", "ph_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy, "rg_risk_ew", "ph_risk_ew"), 3)) #Wald stats

o_q_risk_iy_diagnostics <- c(`Num. Obs.` = o_q_risk_iy_N,
                             `Adjusted R-Squared` =  o_q_risk_iy_R,
                             `Year FE` = ' ',
                             `Industry x Year FE` = '\\checkmark',
                             `Industry x Qtr FE` = ' ',
                             `Firm FE` = ' ',
                             `Firm Controls` = '\\checkmark',
                             `Lagged DV` = ' ',
                             `Climate Measure` = 'Risk',
                             `Estimation` = 'OLS',
                             `Panel` = 'Firm-Qtr',
                             `Wald Stat (Opp - Reg = 0)` = as.character(o_q_risk_iy_Wald[1]),
                             `Wald Stat (Opp - Phy = 0)` = as.character(o_q_risk_iy_Wald[2]),
                             `Wald Stat (Reg - Phy = 0)` = as.character(o_q_risk_iy_Wald[3]))

o_q_iy_risk_out <- list("Estimates" = o_q_risk_iy_estimates,
                        "Diagnostics" = o_q_risk_iy_diagnostics)

## Occ: Interact ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_interaction_MODELS_REVISION_NEW.RData")

o_q_iy_intr <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_intr_estimates <- c(o_q_iy_intr$coefficients["op_expo_ew"], o_q_iy_intr$se["op_expo_ew"],
                           o_q_iy_intr$coefficients["rg_expo_ew"], o_q_iy_intr$se["rg_expo_ew"],
                           o_q_iy_intr$coefficients["ph_expo_ew"], o_q_iy_intr$se["ph_expo_ew"],
                           o_q_iy_intr$coefficients["op_expo_ew:rg_expo_ew"], o_q_iy_intr$se["op_expo_ew:rg_expo_ew"],
                           o_q_iy_intr$coefficients["op_expo_ew:ph_expo_ew"], o_q_iy_intr$se["op_expo_ew:ph_expo_ew"],
                           o_q_iy_intr$coefficients["rg_expo_ew:ph_expo_ew"], o_q_iy_intr$se["rg_expo_ew:ph_expo_ew"])

o_q_iy_intr_N <- o_q_iy_intr$nobs

o_q_iy_intr_R <- unname(round(r2(o_q_iy_intr, type = "ar2"), 3)) #adjusted R2

o_q_iy_intr_Wald <- c(round(compute_wald(o_q_iy_intr, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_intr, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_intr, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_intr_diagnostics <- c(`Num. Obs.` = o_q_iy_intr_N,
                             `Adjusted R-Squared` =  o_q_iy_intr_R,
                             `Year FE` = ' ',
                             `Industry x Year FE` = '\\checkmark',
                             `Industry x Qtr FE` = ' ',
                             `Firm FE` = ' ',
                             `Firm Controls` = '\\checkmark',
                             `Lagged DV` = ' ',
                             `Climate Measure` = 'Exposure',
                             `Estimation` = 'OLS',
                             `Panel` = 'Firm-Qtr',
                             `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_intr_Wald[1]),
                             `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_intr_Wald[2]),
                             `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_intr_Wald[3]))

o_q_iy_intr_out <- list("Estimates" = o_q_iy_intr_estimates,
                        "Diagnostics" = o_q_iy_intr_diagnostics)

## Occ: Lagged DV ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_laggeddv_MODELS_REVISION_NEW.RData")

o_q_iy_ldv <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_ldv_estimates <- c(o_q_iy_ldv$coefficients["op_expo_ew"], o_q_iy_ldv$se["op_expo_ew"],
                          o_q_iy_ldv$coefficients["rg_expo_ew"], o_q_iy_ldv$se["rg_expo_ew"],
                          o_q_iy_ldv$coefficients["ph_expo_ew"], o_q_iy_ldv$se["ph_expo_ew"],
                          o_q_iy_ldv$coefficients["CLI_l1"], o_q_iy_ldv$se["CLI_l1"])

o_q_iy_ldv_N <- o_q_iy_ldv$nobs

o_q_iy_ldv_R <- unname(round(r2(o_q_iy_ldv, type = "ar2"), 3)) #adjusted R2

o_q_iy_ldv_Wald <- c(round(compute_wald(o_q_iy_ldv, "op_expo_ew", "rg_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_ldv, "op_expo_ew", "ph_expo_ew"), 3),
                     round(compute_wald(o_q_iy_ldv, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_ldv_diagnostics <- c(`Num. Obs.` = o_q_iy_ldv_N,
                            `Adjusted R-Squared` =  o_q_iy_ldv_R,
                            `Year FE` = ' ',
                            `Industry x Year FE` = '\\checkmark',
                            `Industry x Qtr FE` = ' ',
                            `Firm FE` = ' ',
                            `Firm Controls` = '\\checkmark',
                            `Lagged DV` = '\\checkmark',
                            `Climate Measure` = 'Exposure',
                            `Estimation` = 'OLS',
                            `Panel` = 'Firm-Qtr',
                            `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_ldv_Wald[1]),
                            `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_ldv_Wald[2]),
                            `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_ldv_Wald[3]))

o_q_iy_ldv_out <- list("Estimates" = o_q_iy_ldv_estimates,
                       "Diagnostics" = o_q_iy_ldv_diagnostics)

## Occ: Congress Target OLS ------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_target_MODELS_REVISION_NEW.RData")

o_q_iy_cong <- models[["CONG"]]

o_q_iy_cong_out <- process_model(o_q_iy_cong)

## Occ: EPA Target OLS ------------------------------------------------

o_q_iy_epa <- models[["EPA"]]

o_q_iy_epa_out <- process_model(o_q_iy_epa)

## Occ: DOE Target OLS ------------------------------------------------

o_q_iy_doe <- models[["DOE"]]

o_q_iy_doe_out <- process_model(o_q_iy_doe)

## Occ: Main Analysis Logit ------------------------------------------------

load("data/03_final/climate_logit_qrt_bycomponent_MODELS_REVISION_NEW.RData")

l_q_iy <- models[[1]]

l_q_iy_out <- process_model(l_q_iy, Estimation = "Logit")

l_q_iy_out$Diagnostics["Adjusted R-Squared"] <- round(r2(l_q_iy, type = "apr2"), 3) #adjusted "Pseudo" R2

## Occ: Annual Model with Overall Expo ------------------------------------------------

load("data/03_final/climate_logit_yr_compare10K_MODELS_REVISION_NEW.RData")

o_a_iy_ovrl <- out[[1]]

o_a_iy_ovrl_estimates <- c(o_a_iy_ovrl$coefficients["cc_expo_ew"], o_a_iy_ovrl$se["cc_expo_ew"])
o_a_iy_ovrl_estimates <- round(o_a_iy_ovrl_estimates * 100, 3)

t <- o_a_iy_ovrl$coefficients["cc_expo_ew"] / o_a_iy_ovrl$se["cc_expo_ew"]
strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))
o_a_iy_ovrl_stars <- data.frame(term="Exposure", estimate=paste0(o_a_iy_ovrl_estimates[1], strs), std.error=o_a_iy_ovrl_estimates[2])

o_a_iy_ovrl_N <- o_a_iy_ovrl$nobs

o_a_iy_ovrl_R <- unname(round(r2(o_a_iy_ovrl, type = "ar2"), 3)) #adjusted R2

o_a_iy_ovrl_diagnostics <- c(`Num. Obs.` = o_a_iy_ovrl_N,
                             `Adjusted R-Squared` =  o_a_iy_ovrl_R,
                             `Year FE` = ' ',
                             `Industry x Year FE` = '\\checkmark',
                             `Industry x Qtr FE` = ' ',
                             `Firm FE` = ' ',
                             `Firm Controls` = '\\checkmark',
                             `Lagged DV` = ' ',
                             `Climate Measure` = 'Ovrl. Expo.',
                             `Estimation` = 'OLS',
                             `Panel` = 'Firm-Yr',
                             `Wald Stat (Opp - Reg = 0)` = ' ',
                             `Wald Stat (Opp - Phy = 0)` = ' ',
                             `Wald Stat (Reg - Phy = 0)` = ' ')

o_a_iy_ovrl_out <- list("Estimates" = o_a_iy_ovrl_estimates,
                        "Diagnostics" = o_a_iy_ovrl_diagnostics)

## Occ: Annual Model with 10-K Exposure Measure ------------------------------------------------

o_a_iy_tenk <- out[[2]]

o_a_iy_tenk_estimates <- c(o_a_iy_tenk$coefficients["tenk_exposure"], o_a_iy_tenk$se["tenk_exposure"])
o_a_iy_tenk_estimates <- round(o_a_iy_tenk_estimates * 100, 3)
t <- o_a_iy_tenk$coefficients["tenk_exposure"] / o_a_iy_tenk$se["tenk_exposure"]
strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))
o_a_iy_tenk_stars <- data.frame(term="Exposure", estimate=paste0(o_a_iy_tenk_estimates[1], strs), std.error=o_a_iy_tenk_estimates[2])

o_a_iy_tenk_N <- o_a_iy_tenk$nobs

o_a_iy_tenk_R <- round(r2(o_a_iy_tenk, type = "ar2"), 3) #adjusted R2

o_a_iy_tenk_diagnostics <- c(`Num. Obs.` = o_a_iy_tenk_N,
                             `Adjusted R-Squared` =  o_a_iy_tenk_R,
                             `Year FE` = ' ',
                             `Industry x Year FE` = '\\checkmark',
                             `Industry x Qtr FE` = ' ',
                             `Firm FE` = ' ',
                             `Firm Controls` = '\\checkmark',
                             `Lagged DV` = ' ',
                             `Climate Measure` = '10-K Expo.',
                             `Estimation` = 'OLS',
                             `Panel` = 'Firm-Yr',
                             `Wald Stat (Opp - Reg = 0)` = ' ',
                             `Wald Stat (Opp - Phy = 0)` = ' ',
                             `Wald Stat (Reg - Phy = 0)` = ' ')

o_a_iy_tenk_out <- list("Estimates" = o_a_iy_tenk_estimates,
                        "Diagnostics" = o_a_iy_tenk_diagnostics)

## Occ: Main Model with Annual Panel ------------------------------------------

load("data/03_final/climate_ols_annual_bycomponent_MODELS_REVISION_NEW.RData")

o_a_iy <- models[[1]]

o_a_iy_out <- process_model(o_a_iy, Panel = 'Firm-Yr')

## Occ: Bills-Based Measure of Climate Lobbying ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_altclimatebills.RData")

o_q_iy_bills <- models[[1]]

o_q_iy_bills_out <- process_model(o_q_iy_bills)


## Occ: Keyword-Based Measure of Climate Lobbying ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_altkeywords.RData")

o_q_iy_kywd <- models[[1]]

o_q_iy_kywd_out <- process_model(o_q_iy_kywd)


## Occ: Keyword-Based Measure of Climate MITIGATION Lobbying ------------------------------------------

o_q_iy_kywd_miti <- models[[2]]

o_q_iy_kywd_miti_out <- process_model(o_q_iy_kywd_miti)


## Occ: Keyword-Based Measure of Climate ADAPTATION Lobbying ------------------------------------------

o_q_iy_kywd_adpt <- models[[3]]

o_q_iy_kywd_adpt_out <- process_model(o_q_iy_kywd_adpt)

## Occ: Main Models with Augmented Controls ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_augmented.RData")

o_q_iy_aug <- models[[1]]

o_q_iy_aug_out <- process_model(o_q_iy_aug, Firm_Controls = "Augm.")

## Occ: Coalition Based Directionality Models ------------------------------------------

## Pro ---

load("data/03_final/climate_ols_yr_coalition_MODELS_REVISION_NEW.RData")

o_y_iy_coal_pro <- models[[1]]

o_y_iy_coal_pro_out <- process_model(o_y_iy_coal_pro, Panel = "Firm-Yr")

## Anti ---

o_y_iy_coal_anti <- models[[2]]

o_y_iy_coal_anti_out <- process_model(o_y_iy_coal_anti, Panel = "Firm-Yr")


## Occ: Imputation Model ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_imputed.RData")

o_q_iy_impt <- models[1,,drop=F]

o_q_iy_impt_estimates <- c(o_q_iy_impt$op_expo_ew_coef, o_q_iy_impt$op_expo_ew_se,
                           o_q_iy_impt$rg_expo_ew_coef, o_q_iy_impt$rg_expo_ew_se,
                           o_q_iy_impt$ph_expo_ew_coef, o_q_iy_impt$ph_expo_ew_se)

o_q_iy_impt_N <- o_q_iy_impt$n

o_q_iy_impt_R <- round(o_q_iy_impt$r, 3)

o_q_iy_impt_Wald <- unname(abs(round(c(o_q_iy_impt$wald1, o_q_iy_impt$wald2, o_q_iy_impt$wald3), 3))) #Wald stats


o_q_iy_impt_diagnostics <- c(`Num. Obs.` = o_q_iy_impt_N,
                             `Adjusted R-Squared` =  o_q_iy_impt_R,
                             `Year FE` = ' ',
                             `Industry x Year FE` = '\\checkmark',
                             `Industry x Qtr FE` = ' ',
                             `Firm FE` = ' ',
                             `Firm Controls` = '\\checkmark',
                             `Lagged DV` = ' ',
                             `Climate Measure` = 'Exposure',
                             `Estimation` = 'OLS',
                             `Panel` = 'Imputed FQ',
                             `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_impt_Wald[1]),
                             `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_impt_Wald[2]),
                             `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_impt_Wald[3]))

o_q_iy_impt_out <- list("Estimates" = o_q_iy_impt_estimates,
                        "Diagnostics" = o_q_iy_impt_diagnostics)

## Occ: Issue-Specific Models ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")

## CAW
o_q_iy_caw <- models[[12]]

o_q_iy_caw_out <- process_model(o_q_iy_caw)

## ENG
o_q_iy_eng <- models[[13]]

o_q_iy_eng_out <- process_model(o_q_iy_eng)


## ENV
o_q_iy_env <- models[[14]]

o_q_iy_env_out <- process_model(o_q_iy_env)

## FUE
o_q_iy_fue <- models[[15]]
o_q_iy_fue_out <- process_model(o_q_iy_fue)

## Occ: Quarter FE ---------------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")

o_q_iq <- models[[11]]

o_q_iq_out <- process_model(o_q_iq, IndbyQtr_FE = '\\checkmark', IndbyYear_FE = ' ')

## Occ: Error Correction ---------------------------------------------------------

load("data/03_final/climate_ols_qrt_errorcorrect_MODELS_REVISION_NEW.RData")

o_q_iy_ec <- models$`(1`

o_q_iy_ec_estimates <- c(o_q_iy_ec$coefficients["op_expo_ew_l1"], o_q_iy_ec$se["op_expo_ew_l1"],
                               o_q_iy_ec$coefficients["op_expo_ew_chg"], o_q_iy_ec$se["op_expo_ew_chg"],
                               o_q_iy_ec$coefficients["rg_expo_ew_l1"], o_q_iy_ec$se["rg_expo_ew_l1"],
                               o_q_iy_ec$coefficients["rg_expo_ew_chg"], o_q_iy_ec$se["rg_expo_ew_chg"],
                               o_q_iy_ec$coefficients["ph_expo_ew_l1"], o_q_iy_ec$se["ph_expo_ew_l1"],
                               o_q_iy_ec$coefficients["ph_expo_ew_chg"], o_q_iy_ec$se["ph_expo_ew_chg"],
                               o_q_iy_ec$coefficients["CLI_l1"], o_q_iy_ec$se["CLI_l1"])
o_q_iy_ec_estimates <- round(o_q_iy_ec_estimates * 100, 3)

t <- c(o_q_iy_ec$coefficients["op_expo_ew_l1"] / o_q_iy_ec$se["op_expo_ew_l1"],
       o_q_iy_ec$coefficients["op_expo_ew_chg"] / o_q_iy_ec$se["op_expo_ew_chg"],
       o_q_iy_ec$coefficients["rg_expo_ew_l1"] / o_q_iy_ec$se["rg_expo_ew_l1"],
       o_q_iy_ec$coefficients["rg_expo_ew_chg"] / o_q_iy_ec$se["rg_expo_ew_chg"],
       o_q_iy_ec$coefficients["ph_expo_ew_l1"] / o_q_iy_ec$se["ph_expo_ew_l1"],
       o_q_iy_ec$coefficients["ph_expo_ew_chg"] / o_q_iy_ec$se["ph_expo_ew_chg"],
       o_q_iy_ec$coefficients["CLI_l1"] / o_q_iy_ec$se["CLI_l1"])

strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))

o_q_iy_ec_stars <- data.frame(term=c("op_expo_ew_l1"="Opp_{t-1}", "rg_expo_ew_l1"="Reg_{t-1}", "ph_expo_ew_l1"="Phy_{t-1}",
                                     "op_expo_ew_chg"="Opp \\Delta", "rg_expo_ew_chg"="Reg \\Delta", "ph_expo_ew_chg"="Phy \\Delta",
                                     "CLI_l1" = "Lagged DV"), 
                              estimate=paste0(o_q_iy_ec_estimates[seq(1, length(o_q_iy_ec_estimates), 2)], strs), 
                              std.error=o_q_iy_ec_estimates[seq(2, length(o_q_iy_ec_estimates), 2)])

o_q_iy_ec_N <- o_q_iy_ec$nobs

o_q_iy_ec_R <- round(r2(o_q_iy_ec, type = "ar2"), 3) #adjusted R2


o_q_iy_ec_diagnostics <- c(`Num. Obs.` = o_q_iy_ec_N,
                           `Adjusted R-Squared` =  o_q_iy_ec_R,
                           `Year FE` = ' ',
                           `Industry x Year FE` = '\\checkmark',
                           `Industry x Qtr FE` = ' ',
                           `Firm FE` = ' ',
                           `Firm Controls` = '\\checkmark',
                           `Lagged DV` = '\\checkmark',
                           `Climate Measure` = 'Exposure',
                           `Estimation` = 'OLS',
                           `Panel` = 'Firm-Qtr',
                           `Wald Stat (Opp - Reg = 0)` = ' ',
                           `Wald Stat (Opp - Phy = 0)` = ' ',
                           `Wald Stat (Reg - Phy = 0)` = ' ')

o_q_iy_ec_out <- list("Estimates" = o_q_iy_ec_estimates,
                      "Diagnostics" = o_q_iy_ec_diagnostics)


## Occ: Transition vs. Physical Risk

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_LiEtAl.RData")

o_q_iy_li <- models[[1]]

o_q_iy_li_estimates <- c(o_q_iy_li$coefficients["tran_risk_w_std"], o_q_iy_li$se["tran_risk_w_std"],
                         o_q_iy_li$coefficients["phy_risk"], o_q_iy_li$se["phy_risk"])
o_q_iy_li_estimates <- round(o_q_iy_li_estimates * 100, 3)
t <- c(o_q_iy_li$coefficients["tran_risk_w_std"] / o_q_iy_li$se["tran_risk_w_std"],
       o_q_iy_li$coefficients["phy_risk"] / o_q_iy_li$se["phy_risk"])
strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))
o_q_iy_li_stars <- data.frame(term=c("Transition", "Physical"), 
                              estimate=c(paste0(o_q_iy_li_estimates[1], strs[1]),
                                         paste0(o_q_iy_li_estimates[3], strs[2])), 
                              std.error=c(o_q_iy_li_estimates[2],
                                          o_q_iy_li_estimates[4]))

o_q_iy_li_N <- o_q_iy_li$nobs

o_q_iy_li_R <- round(r2(o_q_iy_li, type = "ar2"), 3) #adjusted R2

o_q_iy_li_diagnostics <- c(`Num. Obs.` = o_q_iy_li_N,
                             `Adjusted R-Squared` =  o_q_iy_li_R,
                             `Year FE` = ' ',
                             `Industry x Year FE` = '\\checkmark',
                             `Industry x Qtr FE` = ' ',
                             `Firm FE` = ' ',
                             `Firm Controls` = '\\checkmark',
                             `Lagged DV` = ' ',
                             `Climate Measure` = 'Li et al.',
                             `Estimation` = 'OLS',
                             `Panel` = 'Firm-Qtr',
                             `Wald Stat (Opp - Reg = 0)` = ' ',
                             `Wald Stat (Opp - Phy = 0)` = ' ',
                             `Wald Stat (Reg - Phy = 0)` = ' ')

o_q_iy_li_out <- list("Estimates" = o_q_iy_li_estimates,
                        "Diagnostics" = o_q_iy_li_diagnostics)


## Amount --------------------------------------------------


## Amt: Year FE Only ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")
o_q_y_amt <- models[[3]] #Column 3 - firm-quarter panel, year FE

o_q_y_amt_out <- process_model(o_q_y_amt, Year_FE = '\\checkmark', IndbyYear_FE = ' ')

## Amt: Main OLS ---------------------------------------------------

o_q_iy_amt <- models[[5]] #Column 5 - main result for firm-quarter panel, industry-by-year FE

o_q_iy_amt_out <- process_model(o_q_iy_amt)


## Amt: Firm FE ---------------------------------------------------

o_q_iyf_amt <- models[[7]] #Column 7 - add firm FE

o_q_iyf_amt_out <- process_model(o_q_iyf_amt, Firm_FE = '\\checkmark')

## Amt: Sentiment ---------------------------------------------------

o_q_sent_iy_amt <- models[[9]] #Column 9 - industry-by-year FE with sentiment coefs

o_q_sent_iy_amt_estimates <- c(o_q_sent_iy_amt$coefficients["op_sent_ew"], o_q_sent_iy_amt$se["op_sent_ew"],
                               o_q_sent_iy_amt$coefficients["rg_sent_ew"], o_q_sent_iy_amt$se["rg_sent_ew"],
                               o_q_sent_iy_amt$coefficients["ph_sent_ew"], o_q_sent_iy_amt$se["ph_sent_ew"])

o_q_sent_iy_amt_N <- o_q_sent_iy_amt$nobs

o_q_sent_iy_amt_R <- unname(round(r2(o_q_sent_iy_amt, type = "ar2"), 3)) #adjusted R2

o_q_sent_iy_amt_Wald <- c(round(compute_wald(o_q_sent_iy_amt, "op_sent_ew", "rg_sent_ew"), 3), 
                      round(compute_wald(o_q_sent_iy_amt, "op_sent_ew", "ph_sent_ew"), 3), 
                      round(compute_wald(o_q_sent_iy_amt, "rg_sent_ew", "ph_sent_ew"), 3)) #Wald stats

o_q_sent_iy_amt_diagnostics <- c(`Num. Obs.` = o_q_sent_iy_amt_N,
                                 `Adjusted R-Squared` =  o_q_sent_iy_amt_R,
                                 `Year FE` = ' ',
                                 `Industry x Year FE` = '\\checkmark',
                                 `Industry x Qtr FE` = ' ',
                                 `Firm FE` = ' ',
                                 `Firm Controls` = '\\checkmark',
                                 `Lagged DV` = ' ',
                                 `Climate Measure` = 'Sentiment',
                                 `Estimation` = 'OLS',
                                 `Panel` = 'Firm-Qtr',
                                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_sent_iy_amt_Wald[1]),
                                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_sent_iy_amt_Wald[2]),
                                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_sent_iy_amt_Wald[3]))

o_q_iy_amt_sent_out <- list("Estimates" = o_q_sent_iy_amt_estimates,
                            "Diagnostics" = o_q_sent_iy_amt_diagnostics)

## Amt: Risk ---------------------------------------------------

o_q_risk_iy_amt <- models[[10]] #Column 10 - industry-by-year FE with risk coefs

o_q_risk_iy_amt_estimates <- c(o_q_risk_iy_amt$coefficients["op_risk_ew"], o_q_risk_iy_amt$se["op_risk_ew"],
                               o_q_risk_iy_amt$coefficients["rg_risk_ew"], o_q_risk_iy_amt$se["rg_risk_ew"],
                               o_q_risk_iy_amt$coefficients["ph_risk_ew"], o_q_risk_iy_amt$se["ph_risk_ew"])

o_q_risk_iy_amt_N <- o_q_risk_iy_amt$nobs

o_q_risk_iy_amt_R <- unname(round(r2(o_q_risk_iy_amt, type = "ar2"), 3)) #adjusted R2

o_q_risk_iy_amt_Wald <- c(round(compute_wald(o_q_risk_iy_amt, "op_risk_ew", "rg_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy_amt, "op_risk_ew", "ph_risk_ew"), 3), 
                      round(compute_wald(o_q_risk_iy_amt, "rg_risk_ew", "ph_risk_ew"), 3)) #Wald stats

o_q_risk_iy_amt_diagnostics <- c(`Num. Obs.` = o_q_risk_iy_amt_N,
                                 `Adjusted R-Squared` =  o_q_risk_iy_amt_R,
                                 `Year FE` = ' ',
                                 `Industry x Year FE` = '\\checkmark',
                                 `Industry x Qtr FE` = ' ',
                                 `Firm FE` = ' ',
                                 `Firm Controls` = '\\checkmark',
                                 `Lagged DV` = ' ',
                                 `Climate Measure` = 'Risk',
                                 `Estimation` = 'OLS',
                                 `Panel` = 'Firm-Qtr',
                                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_risk_iy_amt_Wald[1]),
                                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_risk_iy_amt_Wald[2]),
                                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_risk_iy_amt_Wald[3]))

o_q_iy_amt_risk_out <- list("Estimates" = o_q_risk_iy_amt_estimates,
                            "Diagnostics" = o_q_risk_iy_amt_diagnostics)

## Amt: Interact ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_interaction_amount_MODELS_REVISION_NEW.RData")

o_q_iy_amt_intr <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_intr_estimates <- c(o_q_iy_amt_intr$coefficients["op_expo_ew"], o_q_iy_amt_intr$se["op_expo_ew"],
                               o_q_iy_amt_intr$coefficients["rg_expo_ew"], o_q_iy_amt_intr$se["rg_expo_ew"],
                               o_q_iy_amt_intr$coefficients["ph_expo_ew"], o_q_iy_amt_intr$se["ph_expo_ew"],
                               o_q_iy_amt_intr$coefficients["op_expo_ew:rg_expo_ew"], o_q_iy_amt_intr$se["op_expo_ew:rg_expo_ew"],
                               o_q_iy_amt_intr$coefficients["op_expo_ew:ph_expo_ew"], o_q_iy_amt_intr$se["op_expo_ew:ph_expo_ew"],
                               o_q_iy_amt_intr$coefficients["rg_expo_ew:ph_expo_ew"], o_q_iy_amt_intr$se["rg_expo_ew:ph_expo_ew"])

o_q_iy_amt_intr_N <- o_q_iy_amt_intr$nobs

o_q_iy_amt_intr_R <- unname(round(r2(o_q_iy_amt_intr, type = "ar2"), 3)) #adjusted R2

o_q_iy_amt_intr_Wald <- c(round(compute_wald(o_q_iy_amt_intr, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_amt_intr, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_amt_intr, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_intr_diagnostics <- c(`Num. Obs.` = o_q_iy_amt_intr_N,
                                 `Adjusted R-Squared` =  o_q_iy_amt_intr_R,
                                 `Year FE` = ' ',
                                 `Industry x Year FE` = '\\checkmark',
                                 `Industry x Qtr FE` = ' ',
                                 `Firm FE` = ' ',
                                 `Firm Controls` = '\\checkmark',
                                 `Lagged DV` = ' ',
                                 `Climate Measure` = 'Exposure',
                                 `Estimation` = 'OLS',
                                 `Panel` = 'Firm-Qtr',
                                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_intr_Wald[1]),
                                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_intr_Wald[2]),
                                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_intr_Wald[3]))

o_q_iy_amt_intr_out <- list("Estimates" = o_q_iy_amt_intr_estimates,
                            "Diagnostics" = o_q_iy_amt_intr_diagnostics)

## Amt: Lagged DV ---------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_laggeddv_amount_MODELS_REVISION_NEW.RData")

o_q_iy_amt_ldv <- models[[5]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_ldv_estimates <- c(o_q_iy_amt_ldv$coefficients["op_expo_ew"], o_q_iy_amt_ldv$se["op_expo_ew"],
                              o_q_iy_amt_ldv$coefficients["rg_expo_ew"], o_q_iy_amt_ldv$se["rg_expo_ew"],
                              o_q_iy_amt_ldv$coefficients["ph_expo_ew"], o_q_iy_amt_ldv$se["ph_expo_ew"],
                              o_q_iy_amt_ldv$coefficients["log_CLI_amount_l1"], o_q_iy_amt_ldv$se["log_CLI_amount_l1"])

o_q_iy_amt_ldv_N <- o_q_iy_amt_ldv$nobs

o_q_iy_amt_ldv_R <- unname(round(r2(o_q_iy_amt_ldv, type = "ar2"), 3)) #adjusted R2

o_q_iy_amt_ldv_Wald <- c(round(compute_wald(o_q_iy_amt_ldv, "op_expo_ew", "rg_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_amt_ldv, "op_expo_ew", "ph_expo_ew"), 3), 
                     round(compute_wald(o_q_iy_amt_ldv, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


o_q_iy_amt_ldv_diagnostics <- c(`Num. Obs.` = o_q_iy_amt_ldv_N,
                                `Adjusted R-Squared` =  o_q_iy_amt_ldv_R,
                                `Year FE` = ' ',
                                `Industry x Year FE` = '\\checkmark',
                                `Industry x Qtr FE` = ' ',
                                `Firm FE` = ' ',
                                `Firm Controls` = '\\checkmark',
                                `Lagged DV` = '\\checkmark',
                                `Climate Measure` = 'Exposure',
                                `Estimation` = 'OLS',
                                `Panel` = 'Firm-Qtr',
                                `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_amt_ldv_Wald[1]),
                                `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_amt_ldv_Wald[2]),
                                `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_amt_ldv_Wald[3]))

o_q_iy_amt_ldv_out <- list("Estimates" = o_q_iy_amt_ldv_estimates,
                           "Diagnostics" = o_q_iy_amt_ldv_diagnostics)

## Amt: Congress Target ---------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_target_amount_MODELS_REVISION_NEW.RData")

o_q_iy_amt_cong <- models[["CONG"]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_cong_out <- process_model(o_q_iy_amt_cong)

## Amt: EPA Target ---------------------------------------------

o_q_iy_amt_epa <- models[["EPA"]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_epa_out <- process_model(o_q_iy_amt_epa)


## Amt: DOE Target ---------------------------------------------

o_q_iy_amt_doe <- models[["DOE"]] #Column 5 - industry-by-year FE with interactions

o_q_iy_amt_doe_out <- process_model(o_q_iy_amt_doe)


## Amount: Annual Model with Overall Expo ------------------------------------------------

load("data/03_final/climate_logit_yr_compare10K_MODELS_REVISION_NEW.RData")

o_a_iy_ovrl_amt <- out[[3]]

o_a_iy_ovrl_amt_estimates <- c(o_a_iy_ovrl_amt$coefficients["cc_expo_ew"], o_a_iy_ovrl_amt$se["cc_expo_ew"])
o_a_iy_ovrl_amt_estimates <- round(o_a_iy_ovrl_amt_estimates * 100, 3)

t <- o_a_iy_ovrl_amt$coefficients["cc_expo_ew"] / o_a_iy_ovrl_amt$se["cc_expo_ew"]
strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))
o_a_iy_amt_ovrl_stars <- data.frame(term="Exposure", estimate=paste0(o_a_iy_ovrl_amt_estimates[1], strs), std.error=o_a_iy_ovrl_amt_estimates[2])

o_a_iy_ovrl_amt_N <- o_a_iy_ovrl_amt$nobs

o_a_iy_ovrl_amt_R <- unname(round(r2(o_a_iy_ovrl_amt, type = "ar2"), 3)) #adjusted R2

o_a_iy_ovrl_amt_diagnostics <- c(`Num. Obs.` = o_a_iy_ovrl_amt_N,
                                 `Adjusted R-Squared` =  o_a_iy_ovrl_amt_R,
                                 `Year FE` = ' ',
                                 `Industry x Year FE` = '\\checkmark',
                                 `Industry x Qtr FE` = ' ',
                                 `Firm FE` = ' ',
                                 `Firm Controls` = '\\checkmark',
                                 `Lagged DV` = ' ',
                                 `Climate Measure` = 'Ovrl Expo.',
                                 `Estimation` = 'OLS',
                                 `Panel` = 'Firm-Yr',
                                 `Wald Stat (Opp - Reg = 0)` = ' ',
                                 `Wald Stat (Opp - Phy = 0)` = ' ',
                                 `Wald Stat (Reg - Phy = 0)` = ' ')

o_a_iy_amt_ovrl_out <- list("Estimates" = o_a_iy_ovrl_amt_estimates,
                            "Diagnostics" = o_a_iy_ovrl_amt_diagnostics)

## Occ: Annual Model with 10-K Exposure Measure ------------------------------------------------

o_a_iy_tenk_amt <- out[[4]]

o_a_iy_tenk_amt_estimates <- c(o_a_iy_tenk_amt$coefficients["tenk_exposure"], o_a_iy_tenk_amt$se["tenk_exposure"])
o_a_iy_tenk_amt_estimates <- round(o_a_iy_tenk_amt_estimates * 100, 3)
t <- o_a_iy_tenk_amt$coefficients["tenk_exposure"] / o_a_iy_tenk_amt$se["tenk_exposure"]
strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))
o_a_iy_amt_tenk_stars <- data.frame(term="Exposure", estimate=paste0(o_a_iy_tenk_amt_estimates[1], strs), std.error=o_a_iy_tenk_amt_estimates[2])

o_a_iy_tenk_amt_N <- o_a_iy_tenk_amt$nobs

o_a_iy_tenk_amt_R <- round(r2(o_a_iy_tenk_amt, type = "ar2"), 3) #adjusted R2

o_a_iy_tenk_amt_diagnostics <- c(`Num. Obs.` = o_a_iy_tenk_amt_N,
                                 `Adjusted R-Squared` =  o_a_iy_tenk_amt_R,
                                 `Year FE` = ' ',
                                 `Industry x Year FE` = '\\checkmark',
                                 `Industry x Qtr FE` = ' ',
                                 `Firm FE` = ' ',
                                 `Firm Controls` = '\\checkmark',
                                 `Lagged DV` = ' ',
                                 `Climate Measure` = '10-K Expo.',
                                 `Estimation` = 'OLS',
                                 `Panel` = 'Firm-Yr',
                                 `Wald Stat (Opp - Reg = 0)` = ' ',
                                 `Wald Stat (Opp - Phy = 0)` = ' ',
                                 `Wald Stat (Reg - Phy = 0)` = ' ')

o_a_iy_amt_tenk_out <- list("Estimates" = o_a_iy_tenk_amt_estimates,
                            "Diagnostics" = o_a_iy_tenk_amt_diagnostics)

## Amt: Main Model with Annual Panel ------------------------------------------

load("data/03_final/climate_ols_annual_bycomponent_MODELS_REVISION_NEW.RData")

o_a_iy_amt <- models[[2]]

o_a_iy_amt_out <- process_model(o_a_iy_amt, Panel = "Firm-Yr")


## Amount: Bills-Based Measure of Climate Lobbying ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW_altclimatebills.RData")

o_q_iy_amt_bills <- models[[1]]

o_q_iy_amt_bills_out <- process_model(o_q_iy_amt_bills)

## Amt: Keyword-Based Measure of Climate Lobbying ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_altkeywords.RData")

o_q_iy_kywd_amt <- models[[4]]

o_q_iy_amt_kywd_out <- process_model(o_q_iy_kywd_amt)


## Amt: Keyword-Based Measure of Climate MITIGATION Lobbying ------------------------------------------

o_q_iy_kywd_miti_amt <- models[[5]]

o_q_iy_amt_kywd_miti_out <- process_model(o_q_iy_kywd_miti_amt)

## Occ: Keyword-Based Measure of Climate ADAPTATION Lobbying ------------------------------------------

o_q_iy_kywd_adpt_amt <- models[[6]]

o_q_iy_amt_kywd_adpt_out <- process_model(o_q_iy_kywd_adpt_amt)


## Amt: Main Models with Augmented Controls ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_augmented.RData")

o_q_iy_aug_amt <- models[[2]]

o_q_iy_amt_aug_out <- process_model(o_q_iy_aug_amt, Firm_Controls = 'Augm.')

## Occ: Coalition Based Directionality Models ------------------------------------------

## Pro ---

load("data/03_final/climate_ols_yr_coalition_MODELS_REVISION_NEW.RData")

o_y_iy_coal_pro_amt <- models[[5]]

o_y_iy_amt_coal_pro_out <- process_model(o_y_iy_coal_pro_amt, Panel = "Firm-Yr")

## Anti ---

o_y_iy_coal_anti_amt <- models[[6]]

o_y_iy_amt_coal_anti_out <- process_model(o_y_iy_coal_anti_amt, Panel = "Firm-Yr")

## Join Pro Coal ---

o_y_iy_coal_joinpro <- models[[3]]

o_y_iy_coal_joinpro_out <- process_model(o_y_iy_coal_joinpro, Panel = "Firm-Yr")

## Join Anti Coal ---
o_y_iy_coal_joinanti <- models[[4]]

o_y_iy_coal_joinanti_out <- process_model(o_y_iy_coal_joinanti, Panel = "Firm-Yr")



## Amt: Imputation Model ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_imputed.RData")

o_q_iy_impt_amt <- models[2,,drop=F]

o_q_iy_impt_amt_estimates <- c(o_q_iy_impt_amt$op_expo_ew_coef, o_q_iy_impt_amt$op_expo_ew_se,
                               o_q_iy_impt_amt$rg_expo_ew_coef, o_q_iy_impt_amt$rg_expo_ew_se,
                               o_q_iy_impt_amt$ph_expo_ew_coef, o_q_iy_impt_amt$ph_expo_ew_se)

o_q_iy_impt_amt_N <- o_q_iy_impt_amt$n

o_q_iy_impt_amt_R <- unname(round(o_q_iy_impt_amt$r, 3))

o_q_iy_impt_amt_Wald <- abs(round(c(o_q_iy_impt_amt$wald1, o_q_iy_impt_amt$wald2, o_q_iy_impt_amt$wald3), 3)) #Wald stats


o_q_iy_impt_amt_diagnostics <- c(`Num. Obs.` = o_q_iy_impt_amt_N,
                                 `Adjusted R-Squared` =  o_q_iy_impt_amt_R,
                                 `Year FE` = ' ',
                                 `Industry x Year FE` = '\\checkmark',
                                 `Industry x Qtr FE` = ' ',
                                 `Firm FE` = ' ',
                                 `Firm Controls` = '\\checkmark',
                                 `Lagged DV` = ' ',
                                 `Climate Measure` = 'Exposure',
                                 `Estimation` = 'OLS',
                                 `Panel` = 'Imputed FQ',
                                 `Wald Stat (Opp - Reg = 0)` = as.character(o_q_iy_impt_amt_Wald[1]),
                                 `Wald Stat (Opp - Phy = 0)` = as.character(o_q_iy_impt_amt_Wald[2]),
                                 `Wald Stat (Reg - Phy = 0)` = as.character(o_q_iy_impt_amt_Wald[3]))

o_q_iy_amt_impt_out <- list("Estimates" = o_q_iy_impt_amt_estimates,
                            "Diagnostics" = o_q_iy_impt_amt_diagnostics)

## Amt: Tobit Model --------------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_tobit.RData")

t_q_iy_amt <- tobit[1,,drop=F]

t_q_iy_amt_estimates <- c(t_q_iy_amt$op_expo_ew_coef, t_q_iy_amt$op_expo_ew_se,
                          t_q_iy_amt$rg_expo_ew_coef, t_q_iy_amt$rg_expo_ew_se,
                          t_q_iy_amt$ph_expo_ew_coef, t_q_iy_amt$ph_expo_ew_se)

t_q_iy_amt_N <- t_q_iy_amt$n

t_q_iy_amt_R <- round(t_q_iy_amt$r, 3)

t_q_iy_amt_Wald <- abs(round(c(t_q_iy_amt$wald1, t_q_iy_amt$wald2, t_q_iy_amt$wald3), 3)) #Wald stats


t_q_iy_amt_diagnostics <- c(`Num. Obs.` = t_q_iy_amt_N,
                            `Adjusted R-Squared` =  t_q_iy_amt_R,
                            `Year FE` = ' ',
                            `Industry x Year FE` = '\\checkmark',
                            `Industry x Qtr FE` = ' ',
                            `Firm FE` = ' ',
                            `Firm Controls` = '\\checkmark',
                            `Lagged DV` = ' ',
                            `Climate Measure` = 'Exposure',
                            `Estimation` = 'Tobit',
                            `Panel` = 'Firm-Qtr',
                            `Wald Stat (Opp - Reg = 0)` = as.character(t_q_iy_amt_Wald[1]),
                            `Wald Stat (Opp - Phy = 0)` = as.character(t_q_iy_amt_Wald[2]),
                            `Wald Stat (Reg - Phy = 0)` = as.character(t_q_iy_amt_Wald[3]))

t_q_iy_amt_out <- list("Estimates" = t_q_iy_amt_estimates,
                       "Diagnostics" = t_q_iy_amt_diagnostics)

## Occ: Issue-Specific Models ------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")

## CAW
o_q_iy_caw_amt <- models[[12]]
o_q_iy_amt_caw_out <- process_model(o_q_iy_caw_amt)

## ENG
o_q_iy_eng_amt <- models[[13]]

o_q_iy_amt_eng_out <- process_model(o_q_iy_eng_amt)

## ENV
o_q_iy_env_amt <- models[[14]]

o_q_iy_amt_env_out <- process_model(o_q_iy_env_amt)

## FUE
o_q_iy_fue_amt <- models[[15]]

o_q_iy_amt_fue_out <- process_model(o_q_iy_fue_amt)

## Amt: Quarter FE ---------------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")

o_q_iq_amt <- models[[11]]

o_q_iq_amt_out <- process_model(o_q_iq_amt, IndbyQtr_FE = '\\checkmark', IndbyYear_FE = ' ')

## Amt: Error Correction -------------------------------------------------------

load("data/03_final/climate_ols_qrt_errorcorrect_MODELS_REVISION_NEW.RData")

o_q_iy_amt_ec <- models$`(2`

o_q_iy_amt_ec_estimates <- c(o_q_iy_amt_ec$coefficients["op_expo_ew_l1"], o_q_iy_amt_ec$se["op_expo_ew_l1"],
                                   o_q_iy_amt_ec$coefficients["op_expo_ew_chg"], o_q_iy_amt_ec$se["op_expo_ew_chg"],
                                   o_q_iy_amt_ec$coefficients["rg_expo_ew_l1"], o_q_iy_amt_ec$se["rg_expo_ew_l1"],
                                   o_q_iy_amt_ec$coefficients["rg_expo_ew_chg"], o_q_iy_amt_ec$se["rg_expo_ew_chg"],
                                   o_q_iy_amt_ec$coefficients["ph_expo_ew_l1"], o_q_iy_amt_ec$se["ph_expo_ew_l1"],
                                   o_q_iy_amt_ec$coefficients["ph_expo_ew_chg"], o_q_iy_amt_ec$se["ph_expo_ew_chg"],
                                   o_q_iy_amt_ec$coefficients["log_CLI_amount_l1"], o_q_iy_amt_ec$se["log_CLI_amount_l1"])
o_q_iy_amt_ec_estimates <- round(o_q_iy_amt_ec_estimates * 100, 3)

t <- c(o_q_iy_amt_ec$coefficients["op_expo_ew_l1"] / o_q_iy_amt_ec$se["op_expo_ew_l1"],
       o_q_iy_amt_ec$coefficients["op_expo_ew_chg"] / o_q_iy_amt_ec$se["op_expo_ew_chg"],
       o_q_iy_amt_ec$coefficients["rg_expo_ew_l1"] / o_q_iy_amt_ec$se["rg_expo_ew_l1"],
       o_q_iy_amt_ec$coefficients["rg_expo_ew_chg"] / o_q_iy_amt_ec$se["rg_expo_ew_chg"],
       o_q_iy_amt_ec$coefficients["ph_expo_ew_l1"] / o_q_iy_amt_ec$se["ph_expo_ew_l1"],
       o_q_iy_amt_ec$coefficients["ph_expo_ew_chg"] / o_q_iy_amt_ec$se["ph_expo_ew_chg"],
       o_q_iy_amt_ec$coefficients["log_CLI_amount_l1"] / o_q_iy_amt_ec$se["log_CLI_amount_l1"])

strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))

o_q_iy_amt_ec_stars <- data.frame(term=c("op_expo_ew_l1"="Opp_{t-1}", "rg_expo_ew_l1"="Reg_{t-1}", "ph_expo_ew_l1"="Phy_{t-1}",
                                         "op_expo_ew_chg"="Opp \\Delta", "rg_expo_ew_chg"="Reg \\Delta", "ph_expo_ew_chg"="Phy \\Delta",
                                         "log_CLI_amount_l1" = "Lagged DV"), 
                              estimate=paste0(o_q_iy_amt_ec_estimates[seq(1, length(o_q_iy_amt_ec_estimates), 2)], strs), 
                              std.error=o_q_iy_amt_ec_estimates[seq(2, length(o_q_iy_amt_ec_estimates), 2)])

o_q_iy_amt_ec_N <- o_q_iy_amt_ec$nobs

o_q_iy_amt_ec_R <- round(r2(o_q_iy_amt_ec, type = "ar2"), 3) #adjusted R2

o_q_iy_amt_ec_diagnostics <- c(`Num. Obs.` = o_q_iy_amt_ec_N,
                               `Adjusted R-Squared` =  o_q_iy_amt_ec_R,
                               `Year FE` = ' ',
                               `Industry x Year FE` = '\\checkmark',
                               `Industry x Qtr FE` = ' ',
                               `Firm FE` = ' ',
                               `Firm Controls` = '\\checkmark',
                               `Lagged DV` = '\\checkmark',
                               `Climate Measure` = 'Exposure',
                               `Estimation` = 'OLS',
                               `Panel` = 'Firm-Qtr',
                               `Wald Stat (Opp - Reg = 0)` = ' ',
                               `Wald Stat (Opp - Phy = 0)` = ' ',
                               `Wald Stat (Reg - Phy = 0)` = ' ')

o_q_iy_amt_ec_out <- list("Estimates" = o_q_iy_amt_ec_estimates, 
                          "Diagnostics" = o_q_iy_amt_ec_diagnostics)

## Amt: Transition vs Physical Risk --------------------------------------------

## Occ: Transition vs. Physical Risk

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW_LiEtAl.RData")

o_q_iy_li_amt <- models[[2]]

o_q_iy_li_amt_estimates <- c(o_q_iy_li_amt$coefficients["tran_risk_w_std"], o_q_iy_li_amt$se["tran_risk_w_std"],
                             o_q_iy_li_amt$coefficients["phy_risk"], o_q_iy_li_amt$se["phy_risk"])
o_q_iy_li_amt_estimates <- round(o_q_iy_li_amt_estimates * 100, 3)
t <- c(o_q_iy_li_amt$coefficients["tran_risk_w_std"] / o_q_iy_li_amt$se["tran_risk_w_std"],
       o_q_iy_li_amt$coefficients["phy_risk"] / o_q_iy_li_amt$se["phy_risk"])
strs <- ifelse(abs(t)>=2.576, "***", ifelse(abs(t)>=1.96, "**", ifelse(abs(t)>=1.645, "*", "")))
o_q_iy_li_amt_stars <- data.frame(term=c("Transition", "Physical"), 
                              estimate=c(paste0(o_q_iy_li_amt_estimates[1], strs[1]),
                                         paste0(o_q_iy_li_amt_estimates[3], strs[2])), 
                              std.error=c(o_q_iy_li_amt_estimates[2],
                                          o_q_iy_li_amt_estimates[4]))

o_q_iy_li_amt_N <- o_q_iy_li_amt$nobs

o_q_iy_li_amt_R <- round(r2(o_q_iy_li_amt, type = "ar2"), 3) #adjusted R2

o_q_iy_li_amt_diagnostics <- c(`Num. Obs.` = o_q_iy_li_amt_N,
                           `Adjusted R-Squared` =  o_q_iy_li_amt_R,
                           `Year FE` = ' ',
                           `Industry x Year FE` = '\\checkmark',
                           `Industry x Qtr FE` = ' ',
                           `Firm FE` = ' ',
                           `Firm Controls` = '\\checkmark',
                           `Lagged DV` = ' ',
                           `Climate Measure` = 'Li et al.',
                           `Estimation` = 'OLS',
                           `Panel` = 'Firm-Qtr',
                           `Wald Stat (Opp - Reg = 0)` = ' ',
                           `Wald Stat (Opp - Phy = 0)` = ' ',
                           `Wald Stat (Reg - Phy = 0)` = ' ')

o_q_iy_li_amt_out <- list("Estimates" = o_q_iy_li_amt_estimates,
                      "Diagnostics" = o_q_iy_li_amt_diagnostics)

## Complete Results for Main Models --------------------------------------------

mod_list <- list(
  "Year FE Occ" = o_q_y,
  "Ind-Yr FE Occ" = o_q_iy,
  "EPA Occ" = o_q_iy_epa,
  "DOE Occ" = o_q_iy_doe,
  "Year FE Amt" = o_q_y_amt,
  "Ind-Yr FE Amt" = o_q_iy_amt,
  "EPA Amt" = o_q_iy_amt_epa,
  "DOE Amt" = o_q_iy_amt_doe
)

scaleby100 <- function(x) {
  #x$coefficients <- x$coefficients * 100
  x$coeftable[,1] <- x$coeftable[,1] * 100
  x$coeftable[,2] <- x$coeftable[,2] * 100
  #x$se <- x$se * 100
  return(x)
}

mod_list <- lapply(mod_list, FUN = function(x) scaleby100(x))

wald1 <- unlist(lapply(mod_list, FUN = function(x) round(compute_wald(x, "op_expo_ew", "rg_expo_ew"), 3)))
wald2 <- unlist(lapply(mod_list, FUN = function(x) round(compute_wald(x, "op_expo_ew", "ph_expo_ew"), 3)))
wald3 <- unlist(lapply(mod_list, FUN = function(x) round(compute_wald(x, "rg_expo_ew", "ph_expo_ew"), 3)))

n <- unlist(lapply(mod_list, FUN = function(x) x$nobs))
r2 <- unlist(lapply(mod_list, FUN = function(x) round(r2(x, type="ar2"), 3)))

auxiliary <- data.frame("Num. Obs." = unname(n),
                        "Adjusted R-Squared" = unname(r2),
                        "Year FE" = c('\\checkmark', ' ', ' ', ' ', '\\checkmark', ' ', ' ', ' '),
                        "Industry x Year FE" = c(' ', '\\checkmark', '\\checkmark', '\\checkmark', ' ', '\\checkmark', '\\checkmark', '\\checkmark'),
                        "Wald (Opp - Reg = 0)" = unname(wald1),
                        "Wald (Opp - Phy = 0)" = unname(wald2),
                        "Wald (Reg - Phy = 0)" = unname(wald3))

model_names <- names(mod_list)
auxiliary <- rbind(t(auxiliary), "Model"=model_names)

auxiliary_out <- data.frame(t(auxiliary)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  mutate(
    Test = case_when(
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      Test == "Wald..Opp...Reg...0." ~ "Wald (Op-Rg=0)",
      Test == "Wald..Opp...Phy...0." ~ "Wald (Op-Ph=0)",
      Test == "Wald..Reg...Phy...0." ~ "Wald (Rg-Ph=0)",
      Test == "Num..Obs." ~ "Num. Obs.",
      Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure",
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_quarter" = "Total Lobbying (\\$)"
)


modelsummary(mod_list
             ,add_rows=auxiliary_out
             ,gof_omit="."
             ,stars = c('*'=0.1, '**'=0.05, '***'=0.01)
             ,coef_map=cm
             ,output="results/tables/appendix_table_test_ols_NEW_mainmodels.tex"
             ,escape=F
)



## Build Occurrence Non-Alt DV Table -------------------------------------------

o_a_iy_ovrl_stars <- list(tidy=o_a_iy_ovrl_stars)
class(o_a_iy_ovrl_stars) <- "modelsummary_list"

o_a_iy_tenk_stars <- list(tidy=o_a_iy_tenk_stars)
class(o_a_iy_tenk_stars) <- "modelsummary_list"

o_q_iy_ec_stars <- list(tidy=o_q_iy_ec_stars)
class(o_q_iy_ec_stars) <- "modelsummary_list"

o_q_iy_li_stars <- list(tidy=o_q_iy_li_stars)
class(o_q_iy_li_stars) <- "modelsummary_list"

mod_list <- list(#"Main" = stars(o_q_iy_out$Estimates),
                 #"EPA" = stars(o_q_iy_epa_out$Estimates),
                 #"DOE" = stars(o_q_iy_doe_out$Estimates),
                 "CONG" = stars(o_q_iy_cong_out$Estimates),
                 #"Yr FE" = stars(o_q_y_out$Estimates),
                 "Ind-Qtr FE" = stars(o_q_iq_out$Estimates),
                 "Logit" = stars(l_q_iy_out$Estimates),
                 "Yr Panel" = stars(o_a_iy_out$Estimates),
                 "Firm FE" = stars(o_q_iyf_out$Estimates),
                 "Aug Ctrl" = stars(o_q_iy_aug_out$Estimates),
                 "Impute" = stars(o_q_iy_impt_out$Estimates),
                 "Lag DV" = stars(o_q_iy_ldv_out$Estimates, ldv=T),
                 "Err Cor" = o_q_iy_ec_stars,
                 "Intr." = stars(o_q_iy_intr_out$Estimates),
                 "Sent" = stars(o_q_iy_sent_out$Estimates),
                 "Risk" = stars(o_q_iy_risk_out$Estimates),
                 "Ovrl" = o_a_iy_ovrl_stars,
                 "10-K" = o_a_iy_tenk_stars,
                 "Li et al" = o_q_iy_li_stars)

auxiliary <- data.frame(#o_q_iy_out$Diagnostics,
                        #o_q_iy_epa_out$Diagnostics,
                        #o_q_iy_doe_out$Diagnostics,
                        o_q_iy_cong_out$Diagnostics,
                        #o_q_y_out$Diagnostics,
                        o_q_iq_out$Diagnostics,
                        l_q_iy_out$Diagnostics,
                        o_a_iy_out$Diagnostics,
                        o_q_iyf_out$Diagnostics,
                        o_q_iy_aug_out$Diagnostics,
                        o_q_iy_impt_out$Diagnostics,
                        o_q_iy_ldv_out$Diagnostics,
                        o_q_iy_ec_out$Diagnostics,
                        o_q_iy_intr_out$Diagnostics,
                        o_q_iy_sent_out$Diagnostics,
                        o_q_iy_risk_out$Diagnostics,
                        o_a_iy_ovrl_out$Diagnostics,
                        o_a_iy_tenk_out$Diagnostics,
                        o_q_iy_li_out$Diagnostics)

model_names <- names(mod_list)
auxiliary <- rbind(auxiliary, "Model"=model_names)

auxiliary_out <- data.frame(t(auxiliary)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      Test == "Industry.x.Qtr.FE" ~ "Industry x Qtr FE",
      Test == "Firm.Controls" ~ "Firm Controls",
      Test == "Estimation" ~ "Estimation",
      Test == "Climate.Measure" ~ "Climate Measure",
      Test == "Wald..Opp.Reg.0." ~ "Wald (Op-Rg=0)",
      Test == "Wald..Opp.Phy.0." ~ "Wald (Op-Ph=0)",
      Test == "Wald..Reg.Phy.0." ~ "Wald (Rg-Ph=0)",
      Test == "Num..Obs." ~ "Num. Obs.",
      Test == "Panel" ~ "Panel",
      Test == "Lagged.DV" ~ "Lagged DV",
      Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

modelsummary(mod_list
             ,add_rows=auxiliary_out
             ,output="results/tables/appendix_table_test_ols_NEW_occur_noaltdv.tex"
             ,escape=F
)

## Build Amount Non-Alt DV Table -------------------------------------------

o_a_iy_amt_ovrl_stars <- list(tidy=o_a_iy_amt_ovrl_stars)
class(o_a_iy_amt_ovrl_stars) <- "modelsummary_list"

o_a_iy_amt_tenk_stars <- list(tidy=o_a_iy_amt_tenk_stars)
class(o_a_iy_amt_tenk_stars) <- "modelsummary_list"

o_q_iy_amt_ec_stars <- list(tidy=o_q_iy_amt_ec_stars)
class(o_q_iy_amt_ec_stars) <- "modelsummary_list"

o_q_iy_li_amt_stars <- list(tidy=o_q_iy_li_amt_stars)
class(o_q_iy_li_amt_stars) <- "modelsummary_list"

mod_list <- list(#"Main" = stars(o_q_iy_amt_out$Estimates),
                 #"EPA" = stars(o_q_iy_amt_epa_out$Estimates),
                 #"DOE" = stars(o_q_iy_amt_doe_out$Estimates),
                 "CONG" = stars(o_q_iy_amt_cong_out$Estimates),
                 #"Yr FE" = stars(o_q_y_amt_out$Estimates),
                 "Ind-Qtr FE" = stars(o_q_iq_amt_out$Estimates),
                 "Tobit" = stars(t_q_iy_amt_out$Estimates),
                 "Yr Panel" = stars(o_a_iy_amt_out$Estimates),
                 "Firm FE" = stars(o_q_iyf_amt_out$Estimates),
                 "Aug Ctrl" = stars(o_q_iy_amt_aug_out$Estimates),
                 "Impute" = stars(o_q_iy_amt_impt_out$Estimates),
                 "Lag DV" = stars(o_q_iy_amt_ldv_out$Estimates, ldv=T),
                 "Err Cor" = o_q_iy_amt_ec_stars,
                 "Intr." = stars(o_q_iy_amt_intr_out$Estimates),
                 "Sent" = stars(o_q_iy_amt_sent_out$Estimates),
                 "Risk" = stars(o_q_iy_amt_risk_out$Estimates),
                 "Ovrl" = o_a_iy_amt_ovrl_stars,
                 "10-K" = o_a_iy_amt_tenk_stars,
                 "Li et al" = o_q_iy_li_amt_stars)

auxiliary <- data.frame(#o_q_iy_amt_out$Diagnostics,
                        #o_q_iy_amt_epa_out$Diagnostics,
                        #o_q_iy_amt_doe_out$Diagnostics,
                        o_q_iy_amt_cong_out$Diagnostics,
                        #o_q_y_amt_out$Diagnostics,
                        o_q_iq_amt_out$Diagnostics,
                        t_q_iy_amt_out$Diagnostics,
                        o_a_iy_amt_out$Diagnostics,
                        o_q_iyf_amt_out$Diagnostics,
                        o_q_iy_amt_aug_out$Diagnostics,
                        o_q_iy_amt_impt_out$Diagnostics,
                        o_q_iy_amt_ldv_out$Diagnostics,
                        o_q_iy_amt_ec_out$Diagnostics,
                        o_q_iy_amt_intr_out$Diagnostics,
                        o_q_iy_amt_sent_out$Diagnostics,
                        o_q_iy_amt_risk_out$Diagnostics,
                        o_a_iy_amt_ovrl_out$Diagnostics,
                        o_a_iy_amt_tenk_out$Diagnostics,
                        o_q_iy_li_amt_out$Diagnostics)

model_names <- names(mod_list)
auxiliary <- rbind(auxiliary, "Model"=model_names)

auxiliary_out <- data.frame(t(auxiliary)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      Test == "Industry.x.Qtr.FE" ~ "Industry x Qtr FE",
      Test == "Firm.Controls" ~ "Firm Controls",
      Test == "Estimation" ~ "Estimation",
      Test == "Climate.Measure" ~ "Climate Measure",
      Test == "Wald..Opp.Reg.0." ~ "Wald (Op-Rg=0)",
      Test == "Wald..Opp.Phy.0." ~ "Wald (Op-Ph=0)",
      Test == "Wald..Reg.Phy.0." ~ "Wald (Rg-Ph=0)",
      Test == "Num..Obs." ~ "Num. Obs.",
      Test == "Panel" ~ "Panel",
      Test == "Lagged.DV" ~ "Lagged DV",
      Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

modelsummary(mod_list
             ,add_rows=auxiliary_out
             ,output="results/tables/appendix_table_test_ols_NEW_amount_noaltdv.tex"
             ,escape=F
)



## Alternate Dependent Variable Specifications ---------------------------------

mod_list <- list("Bills (Occ)" = stars(o_q_iy_bills_out$Estimates),
                 "Kywd (Occ)" = stars(o_q_iy_kywd_out$Estimates),
                 "Miti (Occ)" = stars(o_q_iy_kywd_miti_out$Estimates),
                 "Adpt (Occ)" = stars(o_q_iy_kywd_adpt_out$Estimates),
                 "Pro Clim Oc" = stars(o_y_iy_coal_pro_out$Estimates),
                 "Anti Clim Oc" = stars(o_y_iy_coal_anti_out$Estimates),
                 "Bills (Amt)" = stars(o_q_iy_amt_bills_out$Estimates),
                 "Kywd (Amt)" = stars(o_q_iy_amt_kywd_out$Estimates),
                 "Miti (Amt)" = stars(o_q_iy_amt_kywd_miti_out$Estimates),
                 "Adpt (Amt)" = stars(o_q_iy_amt_kywd_adpt_out$Estimates),
                 "Pro Clim Am" = stars(o_y_iy_amt_coal_pro_out$Estimates),
                 "Anti Clim Am" = stars(o_y_iy_amt_coal_anti_out$Estimates),
                 "Join Pro Coal" = stars(o_y_iy_coal_joinpro_out$Estimates),
                 "Join Anti Coal" = stars(o_y_iy_coal_joinanti_out$Estimates))

auxiliary <- data.frame(o_q_iy_bills_out$Diagnostics,
                        o_q_iy_kywd_out$Diagnostics,
                        o_q_iy_kywd_miti_out$Diagnostics,
                        o_q_iy_kywd_adpt_out$Diagnostics,
                        o_y_iy_coal_pro_out$Diagnostics,
                        o_y_iy_coal_anti_out$Diagnostics,
                        o_q_iy_amt_bills_out$Diagnostics,
                        o_q_iy_amt_kywd_out$Diagnostics,
                        o_q_iy_amt_kywd_miti_out$Diagnostics,
                        o_q_iy_amt_kywd_adpt_out$Diagnostics,
                        o_y_iy_amt_coal_pro_out$Diagnostics,
                        o_y_iy_amt_coal_anti_out$Diagnostics,
                        o_y_iy_coal_joinpro_out$Diagnostics,
                        o_y_iy_coal_joinanti_out$Diagnostics)

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
      Test == "Industry.x.Qtr.FE" ~ "Industry x Qtr FE",
      Test == "Firm.Controls" ~ "Firm Controls",
      Test == "Estimation" ~ "Estimation",
      Test == "Climate.Measure" ~ "Climate Measure",
      Test == "Wald..Opp.Reg.0." ~ "Wald (Op-Rg=0)",
      Test == "Wald..Opp.Phy.0." ~ "Wald (Op-Ph=0)",
      Test == "Wald..Reg.Phy.0." ~ "Wald (Rg-Ph=0)",
      Test == "Num..Obs." ~ "Num. Obs.",
      Test == "Panel" ~ "Panel",
      Test == "Lagged.DV" ~ "Lagged DV",
      Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

modelsummary(mod_list
             ,add_rows=auxiliary_out
             ,output="results/tables/appendix_table_test_ols_NEW_altdv.tex"
)


## Issue Specific Table --------------------------------------------------------


mod_list <- list("CAW (Occ)" = stars(o_q_iy_caw_out$Estimates),
                 "ENG (Occ)" = stars(o_q_iy_eng_out$Estimates),
                 "ENV (Occ)" = stars(o_q_iy_env_out$Estimates),
                 "FUE (Occ)" = stars(o_q_iy_fue_out$Estimates),
                 "CAW (Amt)" = stars(o_q_iy_amt_caw_out$Estimates),
                 "ENG (Amt)" = stars(o_q_iy_amt_eng_out$Estimates),
                 "ENV (Amt)" = stars(o_q_iy_amt_env_out$Estimates),
                 "FUE (Amt)" = stars(o_q_iy_amt_fue_out$Estimates))

auxiliary <- data.frame(o_q_iy_caw_out$Diagnostics,
                        o_q_iy_eng_out$Diagnostics,
                        o_q_iy_env_out$Diagnostics,
                        o_q_iy_fue_out$Diagnostics,
                        o_q_iy_amt_caw_out$Diagnostics,
                        o_q_iy_amt_eng_out$Diagnostics,
                        o_q_iy_amt_env_out$Diagnostics,
                        o_q_iy_amt_fue_out$Diagnostics)


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
      Test == "Industry.x.Qtr.FE" ~ "Industry x Qtr FE",
      Test == "Firm.Controls" ~ "Firm Controls",
      Test == "Estimation" ~ "Estimation",
      Test == "Climate.Measure" ~ "Climate Measure",
      Test == "Wald..Opp.Reg.0." ~ "Wald (Op-Rg=0)",
      Test == "Wald..Opp.Phy.0." ~ "Wald (Op-Ph=0)",
      Test == "Wald..Reg.Phy.0." ~ "Wald (Rg-Ph=0)",
      Test == "Num..Obs." ~ "Num. Obs.",
      Test == "Panel" ~ "Panel",
      Test == "Lagged.DV" ~ "Lagged DV",
      Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

modelsummary(mod_list
             ,add_rows=auxiliary_out
             ,output="results/tables/appendix_table_test_ols_NEW_issues.tex"
)




## Wald Tests Across Models ----------------------------------------------------

# Assuming model1 and model2 are the models to compare
library(lmtest)

load("data/03_final/climate_ols_qrt_bycomponent_target_MODELS_REVISION_NEW.RData")

epa <- models[["EPA"]]
doe <- models[["DOE"]]

# num <- epa$coefficients["op_expo_ew"] - doe$coefficients["op_expo_ew"]
# denom <- sqrt(epa$se["op_expo_ew"]^2 + doe$se["op_expo_ew"]^2)

# Extract coefficients and standard errors
beta1 <- epa$coefficients["op_expo_ew"]
beta2 <- doe$coefficients["op_expo_ew"]
se1 <- sqrt(vcov(epa)["op_expo_ew", "op_expo_ew"])
se2 <- sqrt(vcov(doe)["op_expo_ew", "op_expo_ew"])
# Calculate the Wald statistic
wald_stat <- (beta1 - beta2)^2 / (se1^2 + se2^2)
p_value <- 1 - pchisq(wald_stat, df = 1)
cat("Wald Test Statistic:", wald_stat, "\nP-value:", p_value)

beta1 <- epa$coefficients["ph_expo_ew"]
beta2 <- doe$coefficients["ph_expo_ew"]
se1 <- sqrt(vcov(epa)["ph_expo_ew", "ph_expo_ew"])
se2 <- sqrt(vcov(doe)["ph_expo_ew", "ph_expo_ew"])
# Calculate the Wald statistic
wald_stat <- (beta1 - beta2)^2 / (se1^2 + se2^2)
p_value <- 1 - pchisq(wald_stat, df = 1)
cat("Wald Test Statistic:", wald_stat, "\nP-value:", p_value)



load("data/03_final/climate_ols_qrt_bycomponent_target_amount_MODELS_REVISION_NEW.RData")

epa <- models[["EPA"]]
doe <- models[["DOE"]]

# Extract coefficients and standard errors
beta1 <- epa$coefficients["op_expo_ew"]
beta2 <- doe$coefficients["op_expo_ew"]
se1 <- sqrt(vcov(epa)["op_expo_ew", "op_expo_ew"])
se2 <- sqrt(vcov(doe)["op_expo_ew", "op_expo_ew"])
# Calculate the Wald statistic
wald_stat <- (beta1 - beta2)^2 / (se1^2 + se2^2)
p_value <- 1 - pchisq(wald_stat, df = 1)
cat("Wald Test Statistic:", wald_stat, "\nP-value:", p_value)

# Extract coefficients and standard errors
beta1 <- epa$coefficients["ph_expo_ew"]
beta2 <- doe$coefficients["ph_expo_ew"]
se1 <- sqrt(vcov(epa)["ph_expo_ew", "ph_expo_ew"])
se2 <- sqrt(vcov(doe)["ph_expo_ew", "ph_expo_ew"])
# Calculate the Wald statistic
wald_stat <- (beta1 - beta2)^2 / (se1^2 + se2^2)
p_value <- 1 - pchisq(wald_stat, df = 1)
cat("Wald Test Statistic:", wald_stat, "\nP-value:", p_value)



