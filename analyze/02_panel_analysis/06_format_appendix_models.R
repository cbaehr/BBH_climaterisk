
rm(list = ls())

pacman::p_load(modelsummary)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

stars <- function(x) {
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

process_stata <- function(output, colnum) {
  op_coef <- as.numeric(gsub("=|\\*", "", output[which(tobit$X.=="=op_expo_ew"), colnum]))
  op_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(tobit$X.=="=op_expo_ew")+1, colnum]))
  op_se <- op_coef/op_tstat
  
  rg_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=rg_expo_ew"), colnum]))
  rg_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=rg_expo_ew")+1, colnum]))
  rg_se <- rg_coef/rg_tstat
  
  ph_coef <- as.numeric(gsub("=|\\*", "", output[which(output$X.=="=ph_expo_ew"), colnum]))
  ph_tstat <- as.numeric(gsub("=|\\(|\\)", "", output[which(output$X.=="=ph_expo_ew")+1, colnum]))
  ph_se <- ph_coef/ph_tstat
  
  out <- c(op_coef, op_se, rg_coef, rg_se, ph_coef, ph_se)
  return(out)
}

## --------------------------------------------------

load("data/03_final/climate_logit_qrt_bycomponent_MODELS_REVISION.RData")

l_q_iy <- models[[5]] #Column 4 - main result for firm-quarter panel, industry-by-year FE

l_q_iy_ready <- c(l_q_iy$coefficients["op_expo_ew"], l_q_iy$se["op_expo_ew"],
                  l_q_iy$coefficients["rg_expo_ew"], l_q_iy$se["rg_expo_ew"],
                  l_q_iy$coefficients["ph_expo_ew"], l_q_iy$se["ph_expo_ew"])

l_q_iy_N <- l_q_iy$nobs

l_q_iy_R <- round(r2(l_q_iy, type = "apr2"), 3) #adjusted R2

l_q_iy_Wald <- c(round(compute_wald(l_q_iy, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(l_q_iy, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(l_q_iy, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats
## --------------------------------------------------

l_q_iyf <- models[[7]] #Column 7 - main result for firm-quarter panel, industry-by-year AND FIRM FE

l_q_iyf_ready <- c(l_q_iyf$coefficients["op_expo_ew"], l_q_iyf$se["op_expo_ew"],
                   l_q_iyf$coefficients["rg_expo_ew"], l_q_iyf$se["rg_expo_ew"],
                   l_q_iyf$coefficients["ph_expo_ew"], l_q_iyf$se["ph_expo_ew"])

l_q_iyf_N <- l_q_iyf$nobs

l_q_iyf_R <- round(r2(l_q_iyf, type = "apr2"), 3) #adjusted R2

l_q_iyf_Wald <- c(round(compute_wald(l_q_iyf, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(l_q_iyf, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(l_q_iyf, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


## --------------------------------------------------

l_q_iy_sent <- models[[9]] #Column 9 - industry by year with sentiment for IV
l_q_iy_sent_ready <- c(l_q_iy_sent$coefficients["op_sent_ew"], l_q_iy_sent$se["op_sent_ew"],
                       l_q_iy_sent$coefficients["rg_sent_ew"], l_q_iy_sent$se["rg_sent_ew"],
                       l_q_iy_sent$coefficients["ph_sent_ew"], l_q_iy_sent$se["ph_sent_ew"])

l_q_iy_sent_N <- l_q_iy_sent$nobs

l_q_iy_sent_R <- round(r2(l_q_iy_sent, type = "apr2"), 3) #adjusted R2

l_q_iy_sent_Wald <- c(round(compute_wald(l_q_iy_sent, "op_sent_ew", "rg_sent_ew"), 3), 
                      round(compute_wald(l_q_iy_sent, "op_sent_ew", "ph_sent_ew"), 3),
                      round(compute_wald(l_q_iy_sent, "rg_sent_ew", "ph_sent_ew"), 3)) #Wald stats

## --------------------------------------------------

load("data/03_final/climate_logit_qrt_bycomponent_laggeddv_MODELS_REVISION.RData")

l_q_iy_lgdv <- models[[5]] #Column 5 - industry by year with lagged DV
l_q_iy_lgdv_ready <- c(l_q_iy_lgdv$coefficients["op_expo_ew"], l_q_iy_lgdv$se["op_expo_ew"],
                       l_q_iy_lgdv$coefficients["rg_expo_ew"], l_q_iy_lgdv$se["rg_expo_ew"],
                       l_q_iy_lgdv$coefficients["ph_expo_ew"], l_q_iy_lgdv$se["ph_expo_ew"])

l_q_iy_lgdv_N <- l_q_iy_lgdv$nobs

l_q_iy_lgdv_R <- round(r2(l_q_iy_lgdv, type = "apr2"), 3) #adjusted R2

l_q_iy_lgdv_Wald <- c(round(compute_wald(l_q_iy_lgdv, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(l_q_iy_lgdv, "op_expo_ew", "ph_expo_ew"), 3),
                      round(compute_wald(l_q_iy_lgdv, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats


## --------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION.RData")

o_q_iy <- models[[5]] #Column 5 - main result for firm-quarter OLS panel, industry-by-year FE

o_q_iy_ready <- c(o_q_iy$coefficients["op_expo_ew"], o_q_iy$se["op_expo_ew"],
                  o_q_iy$coefficients["rg_expo_ew"], o_q_iy$se["rg_expo_ew"],
                  o_q_iy$coefficients["ph_expo_ew"], o_q_iy$se["ph_expo_ew"])

o_q_iy_N <- o_q_iy$nobs

o_q_iy_R <- round(r2(o_q_iy, type = "ar2"), 3) #adjusted R2

o_q_iy_Wald <- c(round(compute_wald(o_q_iy, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(o_q_iy, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(o_q_iy, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

## --------------------------------------------------

load("data/03_final/climate_logit_qrt_bycomponent_interactions_MODELS_REVISION.RData")

l_q_iy_intr <- models[[5]] #Column 5 - interaction result for firm-quarter panel, industry-by-year FE

l_q_iy_intr_ready <- c(l_q_iy_intr$coefficients["op_expo_ew"], l_q_iy_intr$se["op_expo_ew"],
                       l_q_iy_intr$coefficients["rg_expo_ew"], l_q_iy_intr$se["rg_expo_ew"],
                       l_q_iy_intr$coefficients["ph_expo_ew"], l_q_iy_intr$se["ph_expo_ew"],
                       l_q_iy_intr$coefficients["op_expo_ew:rg_expo_ew"], l_q_iy_intr$se["op_expo_ew:rg_expo_ew"],
                       l_q_iy_intr$coefficients["op_expo_ew:ph_expo_ew"], l_q_iy_intr$se["op_expo_ew:ph_expo_ew"],
                       l_q_iy_intr$coefficients["rg_expo_ew:ph_expo_ew"], l_q_iy_intr$se["rg_expo_ew:ph_expo_ew"])

l_q_iy_intr_N <- l_q_iy_intr$nobs

l_q_iy_intr_R <- round(r2(l_q_iy_intr, type = "apr2"), 3) #adjusted R2

l_q_iy_intr_Wald <- c(round(compute_wald(l_q_iy_intr, "op_expo_ew", "rg_expo_ew"), 3), 
                 round(compute_wald(l_q_iy_intr, "op_expo_ew", "ph_expo_ew"), 3), 
                 round(compute_wald(l_q_iy_intr, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

## --------------------------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION.RData")

o_q_iy_spnd <- models[[5]] #Column 5 - interaction result for firm-quarter panel, industry-by-year FE

o_q_iy_spnd_ready <- c(o_q_iy_spnd$coefficients["op_expo_ew"], o_q_iy_spnd$se["op_expo_ew"],
                       o_q_iy_spnd$coefficients["rg_expo_ew"], o_q_iy_spnd$se["rg_expo_ew"],
                       o_q_iy_spnd$coefficients["ph_expo_ew"], o_q_iy_spnd$se["ph_expo_ew"])

o_q_iy_spnd_N <- o_q_iy_spnd$nobs

o_q_iy_spnd_R <- round(r2(o_q_iy_spnd, type = "ar2"), 3) #adjusted R2

o_q_iy_spnd_Wald <- c(round(compute_wald(o_q_iy_spnd, "op_expo_ew", "rg_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_spnd, "op_expo_ew", "ph_expo_ew"), 3), 
                      round(compute_wald(o_q_iy_spnd, "rg_expo_ew", "ph_expo_ew"), 3)) #Wald stats

## --------------------------------------------------

tobit <- read.csv("results/model_Data/tobit_results_annual_DATA_REVISION.csv", stringsAsFactors=F)

t_q_iy_ready <- process_stata(tobit, 3)

t_q_iy_N <- as.numeric(gsub("=", "", tobit$X..3.[which(tobit$X.=="=N")]))

t_q_iy_R <- round(as.numeric(gsub("=", "", tobit$X..3.[which(tobit$X.=="=r2a")])), 3)

t_q_iy_Wald <- c(round(as.numeric(gsub("=", "", tobit$X..3.[which(tobit$X.=="=wald1")])), 3),
                 round(as.numeric(gsub("=", "", tobit$X..3.[which(tobit$X.=="=wald2")])), 3),
                 round(as.numeric(gsub("=", "", tobit$X..3.[which(tobit$X.=="=wald3")])), 3))

## --------------------------------------------------

l_q_iy_ready_stars <- stars(l_q_iy_ready)
o_q_iy_ready_stars <- stars(o_q_iy_ready)
l_q_iyf_ready_stars <- stars(l_q_iyf_ready)
l_q_iy_intr_ready_stars <- stars(l_q_iy_intr_ready)
l_q_iy_lgdv_ready_stars <- stars(l_q_iy_lgdv_ready)
l_q_iy_sent_ready_stars <- stars(l_q_iy_sent_ready)
t_q_iy_ready_stars <- stars(t_q_iy_ready)
o_q_iy_spnd_ready_stars <- stars(o_q_iy_spnd_ready)

m1 <- list(tidy=l_q_iy_ready_stars); class(m1) <- "modelsummary_list"
m2 <- list(tidy=o_q_iy_ready_stars); class(m2) <- "modelsummary_list"
m8 <- list(tidy=l_q_iyf_ready_stars); class(m8) <- "modelsummary_list"
m3 <- list(tidy=l_q_iy_intr_ready_stars); class(m3) <- "modelsummary_list"
m4 <- list(tidy=l_q_iy_lgdv_ready_stars); class(m4) <- "modelsummary_list"
m7 <- list(tidy=l_q_iy_sent_ready_stars); class(m7) <- "modelsummary_list"
m5 <- list(tidy=t_q_iy_ready_stars); class(m5) <- "modelsummary_list"
m6 <- list(tidy=o_q_iy_spnd_ready_stars); class(m6) <- "modelsummary_list"

N <- c(l_q_iy_N, o_q_iy_N, l_q_iyf_N, l_q_iy_intr_N, l_q_iy_lgdv_N, l_q_iy_sent_N, t_q_iy_N, o_q_iy_spnd_N)
Wald <- data.frame(l_q_iy_Wald, o_q_iy_Wald, l_q_iyf_Wald, l_q_iy_intr_Wald, l_q_iy_lgdv_Wald, l_q_iy_sent_Wald, t_q_iy_Wald, o_q_iy_spnd_Wald)
R <- c(l_q_iy_R, o_q_iy_R, l_q_iyf_R, l_q_iy_intr_R, l_q_iy_lgdv_R, l_q_iy_sent_R, t_q_iy_R, o_q_iy_spnd_R)

mod_list <- list("Logit 1"=m1, "OLS 1"=m2, "Logit 5"=m8, "Logit 2"=m3, "Logit 3"=m4, "Logit 4"=m7,
                 "Tobit 1"=m5, "OLS 2"=m6)

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Num. Obs.` = as.character(N),
  `Adjusted R-Squared` = as.character(R),
  `Industry x Year FE` = c('\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  `Firm FE` = c(' ', ' ', '\\checkmark', ' ', ' ', ' ', ' ', ' '),
  `Firm Controls` = c('\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  `Lagged DV` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', ' ', ' '),
  `Climate Measure` = c('Exposure', 'Exposure', 'Exposure', 'Exposure', 'Exposure', 'Sentiment', 'Exposure', 'Exposure'),
  `Estimation` = c('Logit', 'OLS', 'Logit', 'Logit', 'Logit', 'Logit', 'Tobit', 'OLS'),
  `Wald Stat (Opp - Reg = 0)` = as.character(Wald[1,]),
  `Wald Stat (Opp - Phy = 0)` = as.character(Wald[2,]),
  `Wald Stat (Reg - Phy = 0)` = as.character(Wald[3,]),
  `Model` = names(mod_list)) %>%
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
      Test == "Lagged.DV" ~ "Lagged DV",
      Test == "Adjusted.R.Squared" ~ "Adj. R-Squared",
      TRUE ~ " "
    )
  )

#stats <- bind_rows(adjusted_r2_df, fes)
stats <- bind_rows(fes)

names(mod_list) <- c("Dummy", "Dummy", "Dummy", "Dummy", "Dummy", "Dummy", "Amount", "Amount")

modelsummary(mod_list
             ,add_rows=stats
             ,output="results/tables/appendix_table_test.tex"
             )







