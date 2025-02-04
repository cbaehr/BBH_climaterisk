### Firms & Lobbying
### Target Analysis

rm(list=ls())

# install_github("insongkim/concordance", dependencies=TRUE)
# Load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary, readxl)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")

# Rename fixed effects variables
df <- df |>
  rename(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

# Run models --------------------------------------------------------------

names(df)

# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure", 
        "cc_expo_ew" = "Overall Exposure",
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_quarter" = "Total Lobbying (\\$)"
)

models <- list(
  "EPA" = feglm(CLI_EPA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOE" = feglm(CLI_DOE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)


### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Industry x Year FE` = c('\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
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
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  vcov = ~ Firm + Year,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/Tables/climate_logit_qrt_targets.tex"
  , escape = FALSE
)



# More targets ------------------------------------------------------------

# DOE="DEPARTMENT OF ENERGY",
# EPA="ENVIRONMENTAL PROTECTION AGENCY",
# FEMA="FEDERAL EMERGENCY MANAGEMENT AGENCY",
# COEQ="COUNCIL ON ENVIRONMENTAL QUALITY",
# DOT="DEPARTMENT OF TRANSPORTATION",
# DOTY="DEPARTMENT OF THE TREASURY",
# DOA="DEPARTMENT OF AGRICULTURE",
# NOAA="NATIONAL OCEANIC AND ATMOSPHERIC ADMINISTRATION",
# HOUS="HOUSE",
# SEN="SENATE",
# WTHS="WHITE HOUSE" 

df$CLI_CONG_quarter <- (df$CLI_HOUS_quarter + df$CLI_SEN_quarter > 0) * 1

models <- list(
  "EPA" = feglm(CLI_EPA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOE" = feglm(CLI_DOE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "FEMA" = feglm(CLI_FEMA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "COEQ" = feglm(CLI_COEQ_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOT" = feglm(CLI_DOT_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOTY" = feglm(CLI_DOTY_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOA" = feglm(CLI_DOA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "NOAA" = feglm(CLI_NOAA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "HOUS" = feglm(CLI_HOUS_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "SEN" = feglm(CLI_SEN_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "CONG" = feglm(CLI_CONG_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "WTHS" = feglm(CLI_WTHS_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Industry x Year FE` = '\\checkmark',
  Model = names(models)) %>%
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
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  vcov = ~ Firm + Year,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/Tables/climate_logit_qrt_targets_all.tex"
  , escape = FALSE
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  vcov = ~ Firm + Year,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "../../Apps/Overleaf/Climate Exposure and Political Activity_BBH/Tables/climate_logit_qrt_targets_all.tex"
  , escape = FALSE
)


## OLS Occurrence - Targets --------------------------------------------------------

df$CLI_CONG_quarter <- (df$CLI_HOUS_quarter + df$CLI_SEN_quarter > 0) * 1

models <- list(
  "EPA" = feols(CLI_EPA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOE" = feols(CLI_DOE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "FEMA" = feols(CLI_FEMA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "COEQ" = feols(CLI_COEQ_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOT" = feols(CLI_DOT_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOTY" = feols(CLI_DOTY_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOA" = feols(CLI_DOA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "NOAA" = feols(CLI_NOAA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "HOUS" = feols(CLI_HOUS_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "SEN" = feols(CLI_SEN_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "CONG" = feols(CLI_CONG_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "WTHS" = feols(CLI_WTHS_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)

save(models, file="data/03_final/climate_ols_qrt_bycomponent_target_MODELS_REVISION_NEW.RData")

## OLS Amount - Targets --------------------------------------------------------

df$CLI_CONG_amount_quarter <- df$CLI_HOUS_amount_quarter + df$CLI_SEN_amount_quarter

models <- list(
  "EPA" = feols(log(CLI_EPA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOE" = feols(log(CLI_DOE_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "FEMA" = feols(log(CLI_FEMA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "COEQ" = feols(log(CLI_COEQ_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOT" = feols(log(CLI_DOT_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOTY" = feols(log(CLI_DOTY_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "DOA" = feols(log(CLI_DOA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "NOAA" = feols(log(CLI_NOAA_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "HOUS" = feols(log(CLI_HOUS_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "SEN" = feols(log(CLI_SEN_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "CONG" = feols(log(CLI_CONG_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "WTHS" = feols(log(CLI_WTHS_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)

save(models, file="data/03_final/climate_ols_qrt_bycomponent_target_amount_MODELS_REVISION_NEW.RData")


### END