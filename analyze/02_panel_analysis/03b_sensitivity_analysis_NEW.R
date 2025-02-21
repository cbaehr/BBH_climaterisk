### Firms & Lobbying
### Sensitivity Analysis

rm(list=ls())

# no scientific notation
options(scipen = 999)

# devtools::install_github("chadhazlett/sensemakr")

# Load packages
pacman::p_load(data.table, tidyverse, fixest, sensemakr, kableExtra)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

names(df)

# create lag
df <- df |>
  # group by unit (in our case: firm)
  group_by(isin) |>
  # arrange by year
  arrange(yearqtr) |>
  # for one year
  mutate(#log_co2_l1 = lag(log_co2, 1),
    CLI_lag = lag(CLI_quarter, 1)) |>
  #ungroup
  ungroup()

sum(duplicated(df[, c("yearqtr", "isin")]))



# Run Models --------------------------------------------------------------


df$CLI <- as.numeric(df$CLI_quarter)

df$log_CLI_amount <- log(df$CLI_amount_quarter +1)

m_occurrence <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year, df, vcov = ~ year + isin)
#m_occurrence_iid <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year, df, vcov = "iid")

m_amount <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year, df, vcov = ~ year + isin)

# Get minimal statistics ---------------------------------------------------

# # Some inspections
# sense <- sensemakr(
#   estimate = coef(m_occurrence)[["op_expo_ew"]],
#   se = se(m_occurrence)[["op_expo_ew"]],
#   dof = df.residual(m_occurrence),
#   treatment = "op_expo_ew"
# )

# sense$sensitivity_stats

# se(m_occurrence)["op_expo_ew"]
# se(m_occurrence_iid)["op_expo_ew"]
# clustering makes huge difference for ses and t-stats

# Function to calculate sensitivity stats with clustered SEs
get_sensitivity_stats_clustered <- function(model, treatment_var) {
  # Create sensitivity analysis using manual inputs
  sense <- sensemakr(
    estimate = coef(model)[treatment_var],
    se = se(model)[treatment_var],
    dof = df.residual(model),
    treatment = treatment_var
  )
  
  # Extract stats and convert to data.frame
  stats_df <- as.data.frame(sense$sensitivity_stats)
  stats_df$model_type <- if(grepl("amount", deparse(substitute(model)))) "Expenditure" else "Occurrence"
  
  return(stats_df)
}

# Get stats for both models and all treatment variables
sensitivity_results <- rbind(
  get_sensitivity_stats_clustered(m_occurrence, "op_expo_ew"),
  get_sensitivity_stats_clustered(m_occurrence, "rg_expo_ew"),
  get_sensitivity_stats_clustered(m_occurrence, "ph_expo_ew"),
  get_sensitivity_stats_clustered(m_amount, "op_expo_ew"),
  get_sensitivity_stats_clustered(m_amount, "rg_expo_ew"),
  get_sensitivity_stats_clustered(m_amount, "ph_expo_ew")
) |>
  mutate(treatment = case_when(
    treatment == "op_expo_ew" ~ "Opportunity",
    treatment == "rg_expo_ew" ~ "Regulatory",
    treatment == "ph_expo_ew" ~ "Physical",
    TRUE ~ treatment
  ))
  
sensitivity_table <- sensitivity_results |>
  select(model_type, treatment, estimate, se, t_statistic, r2yd.x, rv_q, rv_qa) |>
  kbl(format = "latex", booktabs = TRUE, linesep = "",
      col.names = c("Model", "Treatment", "Estimate", "SE", "t-stat", "$R^2_{Y\\sim D|\\mathbf{X}}$", "$RV_{q=1}$", "$RV_{q=1,\\alpha=0.05}$"),
      digits = c(0, 0, 4, 4, 2, 5, 5, 5),
      escape = FALSE,
      caption = "Sensitivity Analysis Results") |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_header_above(c(" " = 2, "Statistics" = 3, "Robustness Values" = 3))

sensitivity_table

# Save to file
writeLines(sensitivity_table, "results/tables/sensitivity_stats.tex")








# Regress the treatments on the covariates
D_on_Z_op <- feols(op_expo_ew ~ rg_expo_ew + ph_expo_ew + total_lobby_quarter + ebit + ebit_at + us_dummy | industry_year, df, vcov = ~ year + isin)
D_on_Z_rg <- feols(rg_expo_ew ~ op_expo_ew + ph_expo_ew + total_lobby_quarter + ebit + ebit_at + us_dummy | industry_year, df, vcov = ~ year + isin)
D_on_Z_ph <- feols(ph_expo_ew ~ op_expo_ew + rg_expo_ew + total_lobby_quarter + ebit + ebit_at + us_dummy | industry_year, df, vcov = ~ year + isin)

# Get partial R2 values
r2dxj.x_op <- partial_r2(t_statistic = tstat(D_on_Z_op)[["ebit"]], dof = df.residual(D_on_Z_op))
r2dxj.x_rg <- partial_r2(t_statistic = tstat(D_on_Z_rg)[["ebit"]], dof = df.residual(D_on_Z_rg))
r2dxj.x_ph <- partial_r2(t_statistic = tstat(D_on_Z_ph)[["ebit"]], dof = df.residual(D_on_Z_ph))

# Get partial R2 values for outcome on treatment and covariates
r2yxj.dx_oc <- partial_r2(t_statistic = tstat(m_occurrence)[["ebit"]], dof = df.residual(m_occurrence))
r2yxj.dx_ex <- partial_r2(t_statistic = tstat(m_expenditure)[["ebit"]], dof = df.residual(m_expenditure))




# Create sensitivity plots ---------------------------------------------------


sense_op <- sensemakr(
  model                = m_occurrence,
  treatment            = "op_expo_ew",
  benchmark_covariates = c("total_lobby_quarter", "ebit", "ebit_at", "us_dummy"),
  kd                   = 10
)

sense_op$sensitivity_stats

plot(sense_op)


# Augment w/ covariates ---------------------------------------------------
df$subs_iso3c <- unlist(lapply(df$subs_iso3c, FUN = function(x) paste(gsub("n.a.", "", unique(unlist(strsplit(x, "\n")))), collapse="|")))
sum(df$subs_iso3c=="n.a.")
sum(df$subs_iso3c=="")

df$subs_iso3c <- gsub("NA", "", df$subs_iso3c)

df$subs_iso3c[which(nchar(df$subs_iso3c)==3)] <- gsub("\\|", "", df$subs_iso3c[which(nchar(df$subs_iso3c)==3)])
df$subs_iso3c[which(df$subs_iso3c=="")] <- NA

df$country_iso_code[which(df$Firm=="NL00150002Q7")] <- "DE"

df$multinational <- 0
df$multinational[which(df$subs_iso3c != df$country_iso_code)] <- 1

## If the subsidiary column isnt EXACTLY equal to the man country column, this
## implies there is a subsidiary in another country.

newdat <- read.csv("data/01_raw/boards_rep_v2/analysis_data_no_proprietary.csv", stringsAsFactors = F)
newdat <- newdat[ , c("gvkey", "year", "cso_exists", "cdp_report")]
df <- merge(df, newdat, by=c("gvkey", "year"), all.x=T)

df$cso_exists[which(is.na(df$cso_exists) & df$year %in% c(2000:2019))] <- 0
df$cdp_report[which(is.na(df$cdp_report) & df$year %in% c(2000:2019))] <- 0

m_occurrence2 <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + cso_exists + cdp_report + n_employees + multinational | industry_year, df, vcov = ~ year + isin)

summary(m_occurrence2)
ovb_minimal_reporting(m_occurrence2, verbose = F)

sense_op2 <- sensemakr(
  model                = m_occurrence2,
  treatment            = "op_expo_ew",
  benchmark_covariates = c("total_lobby_quarter", "ebit", "ebit_at", "us_dummy", "cso_exists", "cdp_report", "n_employees", "multinational"),
  kd                   = 10
)

plot(sense_op2)


# Opportunity
pdf("~/Dropbox (Princeton)/BBH/BBH1/results/Figures/sensitivity/sensitivity_plots_occurrence_op_qty.pdf", width = 6, height = 6)
plot(
  sensemakr(
    m_occurrence,
    treatment = "op_expo_ew",
    benchmark_covariates = "ebit",
    kd = c(5,10,15,20,25)
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

m_amount <- feols(CLI_amount_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_at_scaled + us_dummy_scaled + `Total Lobbying` | industry_year, df, vcov = ~ year + isin)

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


# 1. Start with minimal reporting as recommended by Cinelli & Hazlett
sensitivity_results <- sensemakr(model = m_occurrence,
                                treatment = "op_expo_ew",
                                benchmark_covariates = c("rg_expo_ew", "ph_expo_ew", "ebit_at_scaled", "us_dummy_scaled"),
                                kd = 1:5) # Test up to 5x stronger confounders

# Get standardized reporting table
ovb_minimal_reporting(sensitivity_results, format = "latex")

# 2. Add benchmark analysis using observed covariates
# This helps contextualize the robustness values by comparing to known confounders
benchmark_out <- ovb_bounds(model = m_occurrence,
                           treatment = "op_expo_ew",
                           benchmark_covariates = c("rg_expo_ew", "ph_expo_ew", "ebit_at_scaled", "us_dummy_scaled"),
                           kd = 1:5)

# 3. Create contour plots showing both point estimates and t-values
plot1 <- plot(sensitivity_results) # Point estimates
plot2 <- plot(sensitivity_results, sensitivity.of = "t-value") # Statistical significance

# 4. Examine extreme scenarios
plot3 <- plot(sensitivity_results, 
             type = "extreme",
             r2yz.dx = c(1, .75, .5, .25)) # Test multiple strength levels

# 5. Add verbal interpretation discussing:
# - How the robustness values compare to observed benchmarks
# - Why confounders of the required strength are unlikely given:
#   a) Theoretical mechanisms
#   b) Prior literature
#   c) Data collection process
#   d) Institutional context
#   e) Comparison to observed variable strengths

# END
