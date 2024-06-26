### Firms & Lobbying
### Analysis

### Quarterly

rm(list=ls())

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

# Rename fixed effects variables
df <- df |>
  mutate(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

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

df$CLI <- as.numeric(df$CLI_quarter)

glimpse(df)


# Lobbying Occurrence ------------------------------------------------------


## Overall exposure  -------------------------------------------------

# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew_y, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
#   "(6)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
#   "(7)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
# )
# 
# 
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_year_NEW.tex"
# )


# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + gvkey, family = "binomial", df)
# )
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_qrt_FIRM.tex"
# )



## Exposure components -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Year + Firm),
  "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, family = "binomial", df, vcov = ~ Year + Firm),
  "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, family = "binomial", df, vcov = ~ Year + Firm),
  "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, family = "binomial", df, vcov = ~ Year + Firm),
  "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_logit_qrt_bycomponent_MODELS.RData")

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
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' '),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' '),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark'),
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
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_logit_qrt_bycomponent.tex"
  , escape = FALSE
)



# Inspect model 6
m6 <- models$`(6)`
m6[["fixef_id"]][["Year"]] # year fixed effects: 2001-2020
models$`(7)`[["fixef_id"]][["Firm"]] #  firm fixed effects: 614


# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|RMSE'
# )

### calculate F statistics for each model
# fitstat

# Calculate additional fit statistics
# fitstat(models$`(1)`, ~ f + wf + wald + my)
### 



### w/ firm fixed effects ---------------------------------------------------
# 
# models <- list(
#   "(1)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year, family = "binomial", df),
#   "(5)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + gvkey, family = "binomial", df)
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   #title = 'Effect of Climate Change Attention (components) on Lobbying on Climate Issues',
#   coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_qrt_FIRM.tex"
# )



## Quarter FEs -------------------------------------------------------------

df <- df |>
  mutate(
    Quarter = yearqtr,
    `Industry x Quarter` = paste(Industry, Quarter)
    )

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Quarter, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Quarter + Industry, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Quarter`, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Quarter + Firm, family = "binomial", df, vcov = ~ Quarter + Firm)
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
  `Quarter FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' '),
  `Industry x Quarter FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' '),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark'),
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
      Test == "Quarter.FE" ~ "Quarter FE",
      Test == "Industry.x.Quarter.FE" ~ "Industry x Quarter FE",
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
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_logit_qrt_bycomponent_qrtFEs.tex"
  , escape = FALSE
)



# ## All standardized --------------------------------------------------------
# 
# ## Effect of climate exposure on lobbying occurrence
# models <- list(
#   "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Year + Firm),
#   "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew | Year, family = "binomial", df, vcov = ~ Year + Firm),
#   "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled | Year, family = "binomial", df, vcov = ~ Year + Firm),
#   "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | Year, family = "binomial", df, vcov = ~ Year + Firm),
#   "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | Year + Industry, family = "binomial", df, vcov = ~ Year + Firm),
#   "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | Year + Industry + `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
#   "(7)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + total_lobby_quarter_scaled | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm)
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01)
#   #, coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   # ,output = "results/tables/climate_logit_qrt_bycomponent.tex"
# )



# Lobbying Expenditure ----------------------------------------------------

## Overall climate lobbying (DOLLARS), overall exposure for quarter
models <- list(
  "(1)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew, df, vcov = ~ Year + Firm),
  "(2)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, df, vcov = ~ Year + Firm),
  "(3)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, df, vcov = ~ Year + Firm),
  "(4)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, df, vcov = ~ Year + Firm),
  "(5)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(6)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, df, vcov = ~ Year + Firm)
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
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' '),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' '),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark'),
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
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_amount_qrt_bycomponent.tex"
  , escape = FALSE
)



# ## Plot --------------------------------------------------------------------
# 
# # Base R Plot
# plotmods <- list(models[[4]], models[[5]])
# pdf("results/figures/regressions/coefplot_byexposure_qrt.pdf")
# coefplot(plotmods,
#          dict = c(op_expo_ew="Opportunity", rg_expo_ew="Regulatory", ph_expo_ew="Physical"),
#          keep = c("Opportunity", "Regulatory", "Physical"), horiz=T, ylim.add = c(-0.5, 1), ci.lty=c(1),
#          main = " ")
# legend("topright", col = 1:2, pch = c(16, 17), lwd = 1, lty = 1,
#        legend = c("Year", "+ Year*Industry"), title = "Fixed Effects")
# dev.off()




# Occurence by Issue Area -------------------------------------------------


# ## Disaggregated lobby issues, overall climate exposure, quarter
# models2 <- list(
#   "(1)" = feglm(CLI_CAW_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df),
#   "(2)" = feglm(CLI_ENG_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df),
#   "(3)" = feglm(CLI_ENV_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df),
#   "(4)" = feglm(CLI_FUE_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df)
# )
# 
# x <- modelsummary(
#   models2
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   ,coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output="latex"
#   ,vcov = ~ year + industry
# ) |>
#   # column labels
#   add_header_above(c(
#     " " = 1,
#     "Clean Air and Water" = 1,
#     "Energy" = 1, 
#     "Environment" = 1,
#     "Fuel, Gas, and Oil" = 1))
# save_kable(x, file="results/tables/climate_logit_qrt_separate_issues.tex", keep_tex = T)


# Aggreate and disaggregated lobby issues, disaggregated exposure types, quarter
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(2)" = feglm(CLI_CAW_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(3)" = feglm(CLI_ENG_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(4)" = feglm(CLI_ENV_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(5)" = feglm(CLI_FUE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
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
  `Industry x Year FE` = c('\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
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
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
y <- modelsummary(
  models
  , stars = c('*' = .1, '**' = .05, '***' = .01)
  , coef_map = cm
  , gof_map = "nobs"
  , add_rows = stats
  , output = "latex"
  , escape = FALSE
  , vcov = ~ Year + Firm
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Climate" = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))
save_kable(y, file="results/tables/climate_logit_qrt_bycomponent_separate_issues.tex", keep_tex = T)


# Region level analysis --------------------------------------------------

# us <- c("United States")
# eur <- c("France", "Germany", "Ireland", "Netherlands", "Switzerland", "United Kingdom", "Sweden", "Finland", "Norway", "Italy", "Denmark", 
#          "Belgium", "Luxembourg", "Spain", "Czechia", "Russia", "Austria")
# asia <- c("Japan", "China", "South Korea", "India", "Singapore", "Philippines", "Taiwan")
# 
# df$hqloc <- ifelse(df$country_name %in% us, "usa",
#                    ifelse(df$country_name %in% eur, "europe",
#                           ifelse(df$country_name %in% asia, "asia", NA)))


# ## Overall climate lobbying, overall exposure for quarter by specific attention component
# models <- list(
#   "USA" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df[which(df$hqcountrycode=="usa"), ]),
#   "Europe" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df[which(df$hqcountrycode=="europe"), ]),
#   "Asia" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df[which(df$hqcountrycode=="asia"), ])
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   #title = 'Effect of Climate Change Attention on Lobbying on Climate Issues, by Region',
#   coef_map = cm
#   ,vcov = ~ year + industry
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   output = "latex_tabular"
#   #,output = "climate_logit_qrt.tex"
# )|>
#   # column labels
#   add_header_above(c(
#     " " = 1,
#     "USA" = 1,
#     "Europe" = 1,
#     "Asia" = 1))


# # Plots -------------------------------------------------------------------
# 
# ###Lobbying compared across time for top 10 industries by total attention 
# #filter industries 
# top10ind_total <- df |> 
#   filter(industry %in% c("Automotive Dealers and Gasoline Service Stations", "Coal Mining", "Construction - General Contractors & Operative Builders", "Electric, Gas, and Sanitary Services", "Electronic & Other Electrical Equipment and Components", "Heavy Construction, Except Building Construction and Contractors", "Local & Suburban Transit and Interurban Highway Transportation", "Petroleum Refining and Related Industries", "Primary Metal Industries", "Transportation Equipment"))
# 
# #plot total lobbying reports on climate for each industry year 
# top10ind_lobby <- top10ind_total |>
#   group_by(year, industry) |>
#   summarise(total_climate_reports = sum(CLI), na.rm=TRUE)
# 
# ggplot(data = top10ind_lobby, aes(x = year, y = total_climate_reports, group = industry)) +
#   geom_line(aes(color = industry)) +
#   scale_color_viridis(discrete=TRUE) + 
#   labs(title = " ", x = "Year", y = "Total Climate Lobbying (# Reports)", color = "Industry") +
#   theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))
# 
# ggsave("results/Figures/lobbing_timeseries.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

#plot total spending in reports tagged to climate for each industry year - needs to be updated for correct amount variable
#top10ind_spend <- top10ind_total |>
#  filter(CLI == 1)|>
# group_by(year, industry) |> 
#summarise(total_climate_spend = sum(amount))

#ggplot(data = top10ind_spend, aes(x = year, y = total_climate_spend, group = industry)) +
#geom_line(aes(color = industry)) +
#scale_color_viridis(discrete=TRUE) + 
#labs(title = " ", x = "Year", y = "Total Climate Lobbying (\\$)", color = "Industry") +
#theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

#ggsave("../results/Figures/lobbingspend_timeseries_industry.pdf", width=unit(8, units="in"), height=unit(6, units="in"))


###Scatterplot of climate attention and total lobbying 

