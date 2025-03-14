### Firms & Lobbying
### Sensitivity Analysis

rm(list=ls())

# devtools::install_github("chadhazlett/sensemakr")

# Load packages
pacman::p_load(data.table, tidyverse, fixest, sensemakr)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")
df_old <- read_rds("data/03_final/lobbying_df_quarterly_REVISE.rds")

names(df)


# # Transform Variables -----------------------------------------------------

# # dummy variable: climate issues
# df <- df |>
#   mutate(CLI = ifelse(ENV == 1 | 
#                         CAW == 1 |
#                         ENG == 1 |
#                         FUE == 1,
#                       1,0))

# df$CLI_dollars <- apply(df[, c("amount_num_ENV", "amount_num_CAW", "amount_num_ENG", "amount_num_FUE")],
#                         1, function(x) sum(x, na.rm=T) / 1000000)



# Control variables -------------------------------------------------------

# #US dummy variable 
# df <- df |>
#   mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

# #Total annual lobbying (total dollars)
# df <- df |>
#   group_by(gvkey, year) |>
#   # mutate(total_lobby = n_distinct(report_uuid))
#   mutate(total_lobby = sum(c_across(grep("amount_num", names(df), value=T))))

df <- df |>
  # group by unit (in our case: firm)
  group_by(isin) |>
  # arrange by year
  arrange(yearqtr) |>
  # for one year
  mutate(#log_co2_l1 = lag(log_co2, 1),
    total_lobby_l1 = lag(total_lobby_quarter_scaled, 1)) |>
  #ungroup
  ungroup()


# df$industry <- df$bvd_sector
# df <- df[which(df$industry!=""), ]
# df$industry_year <- paste(df$industry, df$year)

sum(duplicated(df[, c("yearqtr", "isin")]))

# ## continuous variables in regression models
# df_cont_vars <- c("cc_expo_ew", "cc_expo_ew", "op_expo_ew", "rg_expo_ew", "ph_expo_ew",
#                   "ebit_scaled", "at_scaled", "total_lobby_quarter_scaled", "ebit_at_scaled")
# ## pull from main data
# df_cont <- df[, df_cont_vars]
# ## rescale to standard normal
# df_cont <- scale(df_cont)
# ## slot back into main df
# df[, df_cont_vars] <- df_cont


# # Transform variabls for regression
# df <- df |>
#   mutate(
#     ebit_asset = ebit/at,
#     CLI_dollars = log(CLI_dollars +1)
#   )


# Run Models --------------------------------------------------------------

# Occurrence --------------------------------------------------------------

df$`Total Lobbying`<- df$total_lobby_quarter_scaled

df$CLI <- as.numeric(df$CLI_quarter)

m_occurrence <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_at_scaled + us_dummy_scaled + `Total Lobbying` | industry_year, df, vcov = ~ yearqtr + isin)

# Opportunity
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_occurrence_op_qty.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_occurrence,
    treatment = "op_expo_ew",
    benchmark_covariates = "`Total Lobbying`",
    kd = 15
  ),
  cex.label.text = 1,
  label.bump.x = 0.05,
  cex.axis = 1,
  cex.lab = 1
)
dev.off()

# Regulatory
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_occurrence_rg_qty.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_occurrence,
    treatment = "rg_expo_ew",
    benchmark_covariates = "`Total Lobbying`",
    kd = 15
  ),
  cex.label.text = 1,
  label.bump.x = 0.05,
  cex.axis = 1,
  cex.lab = 1
)
dev.off()

# Physical
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_occurrence_ph_qty.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_occurrence,
    treatment = "ph_expo_ew",
    benchmark_covariates = "`Total Lobbying`",
    kd = 15
  ),
  cex.label.text = 1,
  label.bump.x = 0.05,
  cex.axis = 1,
  cex.lab = 1
)
dev.off()

# Amount --------------------------------------------------------------

m_amount <- feols(CLI_amount_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + `Total Lobbying` | year + industry + industry_year, df, vcov = ~ year + isin)

# Opportunity
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_amount_op_qrt.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_amount,
    treatment = "op_expo_ew",
    benchmark_covariates = "`Total Lobbying`",
    kd = 1
  ),
  cex.label.text = 1,
  label.bump.x = 0.05,
  cex.axis = 1,
  cex.lab = 1
)
dev.off()

# Regulatory
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_amount_rg_qrt.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_amount,
    treatment = "rg_expo_ew",
    benchmark_covariates = "`Total Lobbying`",
    kd = 1
  ),
  cex.label.text = 1,
  label.bump.x = 0.05,
  cex.axis = 1,
  cex.lab = 1
)
dev.off()

# Physical
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_amount_ph_qrt.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_amount,
    treatment = "ph_expo_ew",
    benchmark_covariates = "`Total Lobbying`",
    kd = 1
  ),
  cex.label.text = 1,
  label.bump.x = 0.05,
  cex.axis = 1,
  cex.lab = 1
)
dev.off()



# Enhanced Sensitivity Analysis ----------------------------------------------

glimpse(df)

# Create lagged DV
df <- df %>%
  group_by(isin) %>%
  arrange(year, report_quarter_code) %>%
  mutate(
    CLI_lag = lag(CLI),
    CLI_amount_lag = lag(CLI_amount_quarter)
  ) %>%
  ungroup()

# Models with lagged DV
m_occurrence_lag <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + CLI_lag + 
                         ebit + ebit_at + us_dummy + `Total Lobbying` | 
                         industry_year, df, vcov = ~ year + isin)

m_amount_lag <- feols(CLI_amount_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                     CLI_amount_lag + ebit + ebit_at + us_dummy + `Total Lobbying` | 
                     year + industry + industry_year, df, vcov = ~ year + isin)

# Minimal reporting for main results
cat("\nMinimal Sensitivity Reporting - Occurrence Model:\n")
ovb_minimal_reporting(m_occurrence, treatment = "op_expo_ew")
ovb_minimal_reporting(m_occurrence, treatment = "rg_expo_ew")
ovb_minimal_reporting(m_occurrence, treatment = "ph_expo_ew")

cat("\nMinimal Sensitivity Reporting - Amount Model:\n")
ovb_minimal_reporting(m_amount, treatment = "op_expo_ew")
ovb_minimal_reporting(m_amount, treatment = "rg_expo_ew")
ovb_minimal_reporting(m_amount, treatment = "ph_expo_ew")

# Compare relative strength of confounders
# For occurrence model
occurrence_sensitivity <- sensemakr(m_occurrence,
                                  treatment = "op_expo_ew",
                                  benchmark_covariates = c("`Total Lobbying`", "CLI_lag", "ebit"),
                                  kd = 1:3)

# For amount model
amount_sensitivity <- sensemakr(m_amount,
                              treatment = "op_expo_ew",
                              benchmark_covariates = c("`Total Lobbying`", "CLI_amount_lag", "ebit"),
                              kd = 1:3)

# Create plots comparing strength of observed confounders to robustness values
pdf("results/Figures/sensitivity/sensitivity_benchmark_occurrence.pdf", width = 8, height = 6)
plot(occurrence_sensitivity, type = "extreme", cex.lab = 1)
dev.off()

pdf("results/Figures/sensitivity/sensitivity_benchmark_amount.pdf", width = 8, height = 6)
plot(amount_sensitivity, type = "extreme", cex.lab = 1)
dev.off()

# Contour plots showing joint confounding
pdf("results/Figures/sensitivity/contour_occurrence.pdf", width = 8, height = 6)
ovb_contour_plot(m_occurrence, treatment = "op_expo_ew",
                 benchmark_covariates = c("`Total Lobbying`", "CLI_lag"),
                 r2dz.x = seq(0, 0.4, 0.05),
                 r2yz.dx = seq(0, 0.4, 0.05))
dev.off()

pdf("results/Figures/sensitivity/contour_amount.pdf", width = 8, height = 6)
ovb_contour_plot(m_amount, treatment = "op_expo_ew",
                 benchmark_covariates = c("`Total Lobbying`", "CLI_amount_lag"),
                 r2dz.x = seq(0, 0.4, 0.05),
                 r2yz.dx = seq(0, 0.4, 0.05))
dev.off()

# END