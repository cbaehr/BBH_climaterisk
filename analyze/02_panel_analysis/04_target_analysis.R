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
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

# Rename fixed effects variables
df <- df |>
  rename(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

# Run models --------------------------------------------------------------

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
  "EPA" = feglm(CLI_EPA_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry + `Industry x Year`, family = "binomial", df),
  "DOE" = feglm(CLI_DOE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry + `Industry x Year`, family = "binomial", df)
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
    sapply(wald_stats, function(stat)
      stat["F_stat"]),
    sapply(wald_stats, function(stat)
      stat["P_val"])
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c('\\checkmark', '\\checkmark'),
  `Industry FE` = c('\\checkmark', '\\checkmark'),
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

### END