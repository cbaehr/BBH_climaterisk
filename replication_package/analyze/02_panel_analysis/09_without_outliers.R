### Firms & Climate Lobbying
### Analysis without outliers

# load packages
pacman::p_load(data.table, tidyverse, fixest, modelsummary, arrow, dplyr)

# prevent scientific notation
options(scipen = 999)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/Dropbox (Princeton)/BBH/BBH1/")}

# Load data
#df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")
df <- read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")

# Rename fixed effects variables for consistency
df <- df |>
  rename(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

# Function to remove outliers based on z-score
remove_outliers <- function(data, cols, threshold = 4) {
  for(col in cols) {
    if (str_detect(col, "expo")) {
        z_scores <- data[[col]]
        data <- data[abs(z_scores) <= threshold,]
    }
    else {
      z_scores <- scale(data[[col]])
      data <- data[abs(z_scores) <= threshold,]
    }
  }
  return(data)
}


# Define model variables
iv_cols <- c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")
dv_cols <- "CLI_amount_quarter"

# 1. Baseline models (no outlier removal)
m1_base <- feols(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                 ebit + ebit_at + us_dummy + total_lobby_quarter | 
                 `Industry x Year`, 
                 vcov = ~ Year + Firm,
                 data = df)

m2_base <- feols(log(CLI_amount_quarter + 1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                 ebit + ebit_at + us_dummy + total_lobby_quarter | 
                 `Industry x Year`, 
                 vcov = ~ Year + Firm,
                 data = df)

# 2. Remove IV outliers
df_no_iv <- remove_outliers(df, iv_cols)
nrow(df_no_iv)
nrow(df)

# inspect outliers
abs_z_op_expo_ew <- abs(df$op_expo_ew)
abs_z_op_expo_ew > 4


inspect_outliers <- df |>
  # calculate z-scores and absolute z-scores
  filter(!(Firm %in% df_no_iv$Firm) & (abs(op_expo_ew) > 4 | abs(rg_expo_ew) > 4 | abs(ph_expo_ew) > 4)) |>
  dplyr::select(gvkey, yearqtr, op_expo_ew, rg_expo_ew, ph_expo_ew)

View(inspect_outliers)


m1_no_iv <- feols(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                  ebit + ebit_at + us_dummy + total_lobby_quarter | 
                  `Industry x Year`, 
                  vcov = ~ Year + Firm,
                  data = df_no_iv)

m2_no_iv <- feols(log(CLI_amount_quarter + 1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                  ebit + ebit_at + us_dummy + total_lobby_quarter | 
                  `Industry x Year`, 
                  vcov = ~ Year + Firm,
                  data = df_no_iv)

# 3. Remove DV outliers
df_no_dv <- remove_outliers(df, dv_cols)
nrow(df_no_dv)
nrow(df)

inspect_outliers <- df |>
  mutate(
    scale_CLI_amount_quarter = scale(CLI_amount_quarter),
    abs_z_CLI_amount_quarter = abs(scale_CLI_amount_quarter)) |>
  filter(abs_z_CLI_amount_quarter > 4) |>
  mutate(log_CLI_amount_quarter = log(CLI_amount_quarter + 1)) |>
  dplyr::select(gvkey, yearqtr, CLI_amount_quarter, log_CLI_amount_quarter, scale_CLI_amount_quarter, abs_z_CLI_amount_quarter)

View(inspect_outliers)

m2_no_dv <- feols(log(CLI_amount_quarter + 1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                  ebit + ebit_at + us_dummy + total_lobby_quarter | 
                  `Industry x Year`, 
                  vcov = ~ Year + Firm,
                  data = df_no_dv)

# 4. Remove both IV and DV outliers
df_no_both <- remove_outliers(df_no_iv, dv_cols)
m1_no_both <- feols(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                    ebit + ebit_at + us_dummy + total_lobby_quarter | 
                    `Industry x Year`, 
                    vcov = ~ Year + Firm,
                    data = df_no_both)

m2_no_both <- feols(log(CLI_amount_quarter + 1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + 
                    ebit + ebit_at + us_dummy + total_lobby_quarter | 
                    `Industry x Year`, 
                    vcov = ~ Year + Firm,
                    data = df_no_both)


# Create model lists for summary
models <- list(
  "Main (Occ)" = m1_base,
  "No IV Outliers (Occ)" = m1_no_iv,
  "No Outliers (Occ)" = m1_no_both,
  "Main (Amt)" = m2_base,
  "No IV Outliers (Amt)" = m2_no_iv,
  "No DV Outliers (Amt)" = m2_no_dv,
  "No Outliers (Amt)" = m2_no_both
)


# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity",
        "rg_expo_ew" = "Regulatory",
        "ph_expo_ew" = "Physical")

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "ar2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


stats <- adjusted_r2_df |>
  bind_rows(
    tribble(
      ~Test, ~"Main (Occ)", ~"No IV Outliers (Occ)", ~"No Outliers (Occ)", ~"Main (Amt)", ~"No IV Outliers (Amt)", ~"No DV Outliers (Amt)", ~"No Outliers (Amt)",
      "Industry x Year FE", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark",
      "Firm Controls", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"
    )
  )

scaleby100 <- function(x) {
  x$coefficients <- x$coefficients * 100
  x$coeftable[,1] <- x$coeftable[,1] * 100
  x$coeftable[,2] <- x$coeftable[,2] * 100
  x$se <- x$se * 100
  return(x)
}

models <- lapply(models, FUN = function(x) scaleby100(x))


# Generate summary tables
modelsummary(
    models,
    estimate = "{estimate}{stars}",
    gof_omit = ".*",
    coef_map = cm,
    vcov = ~ Firm + Year,
    gof_map = "nobs",
    add_rows = stats,
    output = "results/Tables/climate_ols_qrt_no_outliers.tex",
    escape = FALSE
)


