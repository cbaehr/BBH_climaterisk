# Use PanelMatch to analyze effect of climate exposure on firm lobbying

# Load packages
# install.packages("PanelMatch")
library(PanelMatch)
library(tidyverse)
library(data.table)

# Source custom PanelMatch function
source("code/PanelMatch_function.r")

# load data
df <- read_rds("data/lobbying_df_quarterly_REVISE_normal.rds")

## Preprocessing -----------------------------------------------------------

### Remove duplicates -------------------------------------------------------
names(df)

# Duplicates
dupl <- df |>
  count(isin, yearqtr) |>
  filter(n>1)
nrow(dupl)
# 0 duplicates

# dupldf <- df |>
#   filter(id %in% dupl$id)
# 
# df <- df |>
#   filter(!(id_t %in% dupl$id_t) & !is.na(wave)) |>
#   arrange(nomem_encr,wave) |>
#   data.frame()
# rm(dupl)


## Some transformations -----------------------------------------------------

# Create consecutive new unit id
df_pm <- df |>
  group_by(yearqtr) |>
  mutate(t = cur_group_id()) |>
  ungroup() |>
  arrange(isin) |>
  mutate(id = dense_rank(isin))

# # inspect
# df_pm |>
#   select(isin, id, yearqtr, t)
# 
# df_pm |>
#   select(isin, id, yearqtr, t) |>
#   arrange(yearqtr)
# # worked

## Code treatment variable -------------------------------------------------

# # summary statistics for the change in exposure across all exposure variables for observations in which a change happens
# inspect <- df_pm |>
#   group_by(isin) |>
#   arrange(t) |>
#   mutate(
#     change_op_expo_ew = op_expo_ew - lag(op_expo_ew),
#     lag_op_expo_ew = lag(op_expo_ew),
#     perc_change_op_expo_ew = change_op_expo_ew / abs(lag_op_expo_ew)
#     ) |>
#   select(isin, t, op_expo_ew, lag_op_expo_ew, change_op_expo_ew, perc_change_op_expo_ew) |>
#   filter(!is.na(op_expo_ew) & !is.na(lag_op_expo_ew))
# 
# # among those which increase, what are median, mean, max, mode of change in exposure?
# inspect |>
#   ungroup() |>
#   filter(perc_change_op_expo_ew > 0) |>
#   select(perc_change_op_expo_ew) |>
#   summary()
# # median 112%, mean 658%


# # summary statistics for the change in exposure across all exposure variables for observations in which a change happens
# inspect <- df_pm |>
#   group_by(isin) |>
#   arrange(t) |>
#   mutate(
#     change_ph_expo_ew = ph_expo_ew - lag(ph_expo_ew),
#     lag_ph_expo_ew = lag(ph_expo_ew),
#     perc_change_ph_expo_ew = change_ph_expo_ew / abs(lag_ph_expo_ew)
#   ) |>
#   select(isin, t, ph_expo_ew, lag_ph_expo_ew, change_ph_expo_ew, perc_change_ph_expo_ew) |>
#   filter(!is.na(ph_expo_ew) & !is.na(lag_ph_expo_ew))

# # among those which increase, what are median, mean, max, mode of change in exposure?
# inspect |>
#   ungroup() |>
#   filter(perc_change_ph_expo_ew > 0) |>
#   select(perc_change_ph_expo_ew) |>
#   summary()
# # median 112%, mean 658%

# 1. Firms switch from below median climate exposure to above median climate exposure within their industry in a given quarter
# 2. Firms increase climate exposure in a given quarter
# 3. Firms increase climate exposure by more than 100% in a given quarter
# 4. Firms increase climate exposure by more than 200% in a given quarter

## Calculate treatment variables 
df_pm <- df_pm |>
  # Calculate industry medians for each time period
  group_by(industry, t) |>
  mutate(
    op_industry_median = median(op_expo_ew, na.rm = TRUE),
    rg_industry_median = median(rg_expo_ew, na.rm = TRUE),
    ph_industry_median = median(ph_expo_ew, na.rm = TRUE)
  ) |>
  ungroup() |>
  # Calculate treatments within each firm over time
  group_by(isin) |>
  arrange(t) |>
  mutate(
    # Treatment 1: Switch from below to above industry median
    treat_op_median = ifelse(lag(op_expo_ew) <= lag(op_industry_median) & 
                               op_expo_ew > op_industry_median, 1, 0),
    treat_rg_median = ifelse(lag(rg_expo_ew) <= lag(rg_industry_median) & 
                               rg_expo_ew > rg_industry_median, 1, 0), 
    treat_ph_median = ifelse(lag(ph_expo_ew) <= lag(ph_industry_median) & 
                               ph_expo_ew > ph_industry_median, 1, 0),
    
    # Treatment 2: Any increase in exposure
    treat_op_increase = ifelse(op_expo_ew > lag(op_expo_ew), 1, 0),
    treat_rg_increase = ifelse(rg_expo_ew > lag(rg_expo_ew), 1, 0),
    treat_ph_increase = ifelse(ph_expo_ew > lag(ph_expo_ew), 1, 0),
    
    # Treatment 3: 100% increase in exposure (calculating explicit change)
    treat_op_100per = ifelse(
      (op_expo_ew - lag(op_expo_ew))/abs(lag(op_expo_ew)) > 1 &
        op_expo_ew > lag(op_expo_ew), 1, 0
    ),
    treat_rg_100per = ifelse(
      (rg_expo_ew - lag(rg_expo_ew))/abs(lag(rg_expo_ew)) > 1 &
        rg_expo_ew > lag(rg_expo_ew), 1, 0
    ),
    treat_ph_100per = ifelse(
      (ph_expo_ew - lag(ph_expo_ew))/abs(lag(ph_expo_ew)) > 1 &
        ph_expo_ew > lag(ph_expo_ew), 1, 0
    ),
    
    # Treatment 4: 200% increase in exposure (calculating explicit change)
    treat_op_200per = ifelse(
      (op_expo_ew - lag(op_expo_ew))/abs(lag(op_expo_ew)) > 2 &
        op_expo_ew > lag(op_expo_ew), 1, 0
    ),
    treat_rg_200per = ifelse(
      (rg_expo_ew - lag(rg_expo_ew))/abs(lag(rg_expo_ew)) > 2 &
        rg_expo_ew > lag(rg_expo_ew), 1, 0
    ),
    treat_ph_200per = ifelse(
      (ph_expo_ew - lag(ph_expo_ew))/abs(lag(ph_expo_ew)) > 2 &
        ph_expo_ew > lag(ph_expo_ew), 1, 0
    )
  ) |>
  ungroup()


# inspect
inspect <- df_pm |>
  filter(!is.na(op_expo_ew)) |>
  select(isin, t, op_expo_ew, op_industry_median, treat_op_median, treat_op_increase, treat_op_100per, treat_op_200per) |>
  arrange(isin, t)

inspect <- df_pm |>
  filter(!is.na(ph_expo_ew)) |>
  select(isin, t, ph_expo_ew, ph_industry_median, treat_ph_median, treat_ph_increase, treat_ph_100per, treat_ph_200per) |>
  arrange(isin, t)

# View(inspect)
table(df_pm$treat_op_median, useNA = "ifany")
table(df_pm$treat_op_increase, useNA = "ifany")
table(df_pm$treat_op_100per, useNA = "ifany")
table(df_pm$treat_op_200per, useNA = "ifany")
table(df_pm$treat_rg_median, useNA = "ifany")
table(df_pm$treat_rg_increase, useNA = "ifany")
table(df_pm$treat_rg_100per, useNA = "ifany")
table(df_pm$treat_rg_200per, useNA = "ifany")
table(df_pm$treat_ph_median, useNA = "ifany")
table(df_pm$treat_ph_increase, useNA = "ifany")
table(df_pm$treat_ph_100per, useNA = "ifany")
table(df_pm$treat_ph_200per, useNA = "ifany")

# if treat NA, then 0
df_pm <- df_pm |>
  mutate(
    treat_op_median = ifelse(is.na(treat_op_median), 0, treat_op_median),
    treat_op_increase = ifelse(is.na(treat_op_increase), 0, treat_op_increase),
    treat_op_100per = ifelse(is.na(treat_op_100per), 0, treat_op_100per),
    treat_op_200per = ifelse(is.na(treat_op_200per), 0, treat_op_200per),
    treat_rg_median = ifelse(is.na(treat_rg_median), 0, treat_rg_median),
    treat_rg_increase = ifelse(is.na(treat_rg_increase), 0, treat_rg_increase),
    treat_rg_100per = ifelse(is.na(treat_rg_100per), 0, treat_rg_100per),
    treat_rg_200per = ifelse(is.na(treat_rg_200per), 0, treat_rg_200per),
    treat_ph_median = ifelse(is.na(treat_ph_median), 0, treat_ph_median),
    treat_ph_increase = ifelse(is.na(treat_ph_increase), 0, treat_ph_increase),
    treat_ph_100per = ifelse(is.na(treat_ph_100per), 0, treat_ph_100per),
    treat_ph_200per = ifelse(is.na(treat_ph_200per), 0, treat_ph_200per)
  )


## Code outcome variable ---------------------------------------------------

df_pm$CLI <- as.numeric(df_pm$CLI_quarter)

df_pm$log_CLI_amount <- log(df_pm$CLI_amount_quarter + 1)


## Code covariates --------------------------------------------------------

# Discretize continuous variables
df_pm <- df_pm %>%
  mutate(
    # Bin continuous variables into quartiles
    ebit_cat = ntile(ebit, 4),
    ebit_at_cat = ntile(ebit_at, 4),
    total_lobby_quarter_cat = ntile(total_lobby_quarter, 4),
    industry = as.factor(industry)
  )

## Create empty dataframes for results -------------------------------------

# Create an empty dataframe to store the results
results_df <- data.frame(
  treatment = character(),
  outcome = character(),
  t = numeric(),
  estimate = numeric(),
  conf.low = numeric(),
  conf.high = numeric(),
  conf.low90 = numeric(),
  conf.high90 = numeric(),
  stringsAsFactors = FALSE
)

# Create an empty dataframe to store covariate balances
covariate_balance_df <- data.frame(
  treatment = character(),
  outcome = character(),
  t = numeric(),
  covariate = character(),
  covbal = numeric(),
  model = character(),
  stringsAsFactors = FALSE
)


# Define treatments, outcomes, covariates ---------------------------------

table(df_pm$treat_ph_median)

# Define treatments, outcomes, and covariates
treatments <- c(
  "treat_op_median",
  "treat_op_increase",
  "treat_op_100per",
  "treat_op_200per" ,
  "treat_rg_median",
  "treat_rg_increase",
  "treat_rg_100per",
  "treat_rg_200per",
  "treat_ph_median",
  "treat_ph_increase",
  "treat_ph_100per",
  "treat_ph_200per"
)

outcomes <- c(
  "CLI",
  "log_CLI_amount"
)

covariates <- c(
  "ebit_cat", "ebit_at_cat", "us_dummy", "total_lobby_quarter_cat", "industry"
)

# number of missingness in the covariates
missing <- df_pm |>
  filter(!is.na(treat_op_median)) |>
  select(all_of(covariates)) |>
  summarise_all(~sum(is.na(.)))

## Specify paths -----------------------------------------------------------

figure_path <- "figures/panelmatch/"
dataframes_path <- "figures/panelmatch/dataframes/"

# Create folders if they don't exist
dir.create(figure_path, showWarnings = FALSE)
dir.create(dataframes_path, showWarnings = FALSE)

## Load already existing results dataframes --------------------------------

# Check if panelmatch_results.csv exists
if (file.exists(paste0(dataframes_path, "panelmatch_results.csv"))) {
  results_df <- fread(paste0(dataframes_path, "panelmatch_results.csv"))
  message("Loaded existing panelmatch_results.csv.")
} else {
  message("panelmatch_results.csv does not exist. Initializing as NULL.")
}

# Check if panelmatch_covariate_balance_df.csv exists
if (file.exists(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))) {
  covariate_balance_df <- fread(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))
  message("Loaded existing panelmatch_covariate_balance_df.csv.")
} else {
  message("panelmatch_covariate_balance_df.csv does not exist. Initializing as NULL.")
}


## Specify other inputs ----------------------------------------------------


# # create small sample of df_pm
# df_pm_s <- sample_n(df_pm, 200000)

data <- data.frame(df_pm)
unit_id <- "id"
time_id <- "t"
lag <- 4
lead <- c(0:7)

## Optimize memory usage before running loops ------------------------------

# Clear unnecessary objects from memory
rm(list=setdiff(ls(), c("df_pm", "treatments", "outcomes", "covariates", "figure_path", "dataframes_path")))
gc()

# Convert df_pm to data.table for better memory efficiency
data <- as.data.table(df_pm)
rm(df_pm)
gc()

## Modified loop structure ------------------------------------------------

# Run the function for all combinations of treatments and outcomes
for (treatment in treatments) {
  # Clear memory at start of each iteration
  gc()
  
  # Visualize Treatment Distribution
  treatment_hist <- DisplayTreatment(
    unit.id = unit_id,
    time.id = time_id,
    legend.position = "bottom",
    xlab = "Wave",
    ylab = "ID",
    title = "",
    treatment = treatment,
    data = data,
    hide.y.tick.label = TRUE,
    dense.plot = TRUE
  )
  
  # Save and clear plot from memory
  ggsave(
    filename = paste0(treatment, "_hist.pdf"),
    plot = treatment_hist,
    path = figure_path,
    height = 8, width = 8
  )
  rm(treatment_hist)
  gc()
  
  for (outcome in outcomes) {
    message(paste("Running analysis for treatment:", treatment, " and outcome:", outcome))
    
    # Save results incrementally after each iteration
    tryCatch({
      results <- run_panelmatch(
        data, 
        treatment,
        outcome, 
        covariates, 
        lag = lag, 
        lead = lead, 
        figure_path = figure_path, 
        dataframes_path = dataframes_path
      )
      
      # Immediately write results to disk
      fwrite(results$results_df, 
             file = paste0(dataframes_path, "panelmatch_results_", treatment, "_", outcome, ".csv"))
      fwrite(results$covariate_balance_df, 
             file = paste0(dataframes_path, "panelmatch_covbal_", treatment, "_", outcome, ".csv"))
      
      # Clear results from memory
      rm(results)
      gc()
      
    }, error = function(e) {
      message(paste("Error in iteration:", treatment, outcome))
      message(e)
    })
  }
}

# After all iterations, combine the individual files
results_files <- list.files(dataframes_path, pattern = "panelmatch_results_.*\\.csv$", full.names = TRUE)
covbal_files <- list.files(dataframes_path, pattern = "panelmatch_covbal_.*\\.csv$", full.names = TRUE)

results_df <- rbindlist(lapply(results_files, fread))
covariate_balance_df <- rbindlist(lapply(covbal_files, fread))

# Write final combined results
fwrite(results_df, file = paste0(dataframes_path,"panelmatch_results.csv"))
fwrite(covariate_balance_df, file = paste0(dataframes_path,"panelmatch_covariate_balance_df.csv"))



# # inspect -----------------------------------------------------------------
# match_formula <-
#   as.formula(paste("~", paste(covariates, collapse = " + ")))

# match <- PanelMatch(
#   lag = lag,
#   time.id = time_id,
#   unit.id = unit_id,
#   covs.formula = match_formula,
#   treatment = "financial_units_1yr_h_units_abv_med1",
#   refinement.method = "mahalanobis",
#   data = data,
#   match.missing = TRUE,
#   qoi = "att",
#   outcome.var = "fin_speech_share",
#   lead = lead,
#   forbid.treatment.reversal = FALSE,
#   placebo.test = TRUE
# )

# match

# balance_scatter(
#   matched_set_list = list(match$att),
#   data = data,
#   covariates = c(covariates, outcome)
# )


### END