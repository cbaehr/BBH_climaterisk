# Use PanelMatch to analyze effect of climate exposure on firm lobbying

# Load packages
# install.packages("PanelMatch")
library(PanelMatch)
library(tidyverse)
library(data.table)
library(stringr)

setwd("/scratch/gpfs/vh4264/bbh1")

# Source custom PanelMatch function
source("code/PanelMatch_function.r")

# load data
df <- read_rds("data/lobbying_df_quarterly_REVISE_normal.rds")

## Preprocessing -----------------------------------------------------------

### Remove duplicates -------------------------------------------------------
# names(df)

# # Duplicates
# dupl <- df |>
#   count(isin, yearqtr) |>
#   filter(n>1)
# nrow(dupl)
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


# # inspect
# inspect <- df_pm |>
#   filter(!is.na(op_expo_ew)) |>
#   select(isin, t, op_expo_ew, op_industry_median, treat_op_median, treat_op_increase, treat_op_100per, treat_op_200per) |>
#   arrange(isin, t)

# inspect <- df_pm |>
#   filter(!is.na(ph_expo_ew)) |>
#   select(isin, t, ph_expo_ew, ph_industry_median, treat_ph_median, treat_ph_increase, treat_ph_100per, treat_ph_200per) |>
#   arrange(isin, t)

# # View(inspect)
# table(df_pm$treat_op_median, useNA = "ifany")
# table(df_pm$treat_op_increase, useNA = "ifany")
# table(df_pm$treat_op_100per, useNA = "ifany")
# table(df_pm$treat_op_200per, useNA = "ifany")
# table(df_pm$treat_rg_median, useNA = "ifany")
# table(df_pm$treat_rg_increase, useNA = "ifany")
# table(df_pm$treat_rg_100per, useNA = "ifany")
# table(df_pm$treat_rg_200per, useNA = "ifany")
# table(df_pm$treat_ph_median, useNA = "ifany")
# table(df_pm$treat_ph_increase, useNA = "ifany")
# table(df_pm$treat_ph_100per, useNA = "ifany")
# table(df_pm$treat_ph_200per, useNA = "ifany")

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

# Initialize empty dataframes if they don't exist
if (!exists("results_df")) {
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
}

if (!exists("covariate_balance_df")) {
  covariate_balance_df <- data.frame(
    treatment = character(),
    outcome = character(),
    t = numeric(),
    covariate = character(),
    covbal = numeric(),
    stringsAsFactors = FALSE
  )
}


# Define treatments, outcomes, covariates ---------------------------------

# table(df_pm$treat_ph_median)

# Define treatments, outcomes, and covariates
treatments <- c(
  "treat_op_median" # ,
  # "treat_rg_median",
  # "treat_ph_median",
  # "treat_op_increase",
  # "treat_op_100per",
  # "treat_op_200per" ,
  # "treat_rg_increase",
  # "treat_rg_100per",
  # "treat_rg_200per",
  # "treat_ph_increase",
  # "treat_ph_100per",
  # "treat_ph_200per"
)

outcomes <- c(
  # "CLI",
  "log_CLI_amount"
)

covariates <- c(
#"ebit_cat", "ebit_at_cat", "us_dummy", "total_lobby_quarter_cat", "industry"
)

# # number of missingness in the covariates
# missing <- df_pm |>
#   filter(!is.na(treat_op_median)) |>
#   select(all_of(covariates)) |>
#   summarise_all(~sum(is.na(.)))

## Specify paths -----------------------------------------------------------

figure_path <- "figures/panelmatch/reduced/"
dataframes_path <- "figures/panelmatch/reduced/dataframes/"

# # Create folders if they don't exist
# dir.create(figure_path, showWarnings = FALSE)
# dir.create(dataframes_path, showWarnings = FALSE)

## Load already existing results dataframes --------------------------------

# # Check if panelmatch_results.csv exists
# if (file.exists(paste0(dataframes_path, "panelmatch_results.csv"))) {
#   results_df <- fread(paste0(dataframes_path, "panelmatch_results.csv"))
#   message("Loaded existing panelmatch_results.csv.")
# } else {
#   message("panelmatch_results.csv does not exist. Initializing as NULL.")
# }

# # Check if panelmatch_covariate_balance_df.csv exists
# if (file.exists(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))) {
#   covariate_balance_df <- fread(paste0(dataframes_path, "panelmatch_covariate_balance_df.csv"))
#   message("Loaded existing panelmatch_covariate_balance_df.csv.")
# } else {
#   message("panelmatch_covariate_balance_df.csv does not exist. Initializing as NULL.")
# }


## Specify other inputs ----------------------------------------------------

# Define key identifiers
unit_id <- "id"
time_id <- "t"
lag <- 2
lead <- c(0:3)

## Optimize memory usage before running loops ------------------------------

# Clear unnecessary objects from memory
rm(list=setdiff(ls(), c("df_pm", "treatments", "outcomes", "covariates", "figure_path", "dataframes_path", 
                        "unit_id", "time_id", "lag", "lead", "results_df", "covariate_balance_df", 
                        "run_panelmatch", "theme_vincent")))
gc()

# Keep as data.frame but remove unnecessary columns
data <- as.data.frame(df_pm[, c(unit_id, time_id, treatments, outcomes #, covariates
)])
rm(df_pm)
gc()

# show class of all columns
# sapply(data, class)

# Transform all numeric columns to integer
data <- data |>
  mutate(across(where(is.numeric), as.integer))


# Theme
theme_vincent <- function (fontsize = 15, facet_alt = F) {
  th <- ggplot2::theme_bw() + ggplot2::theme(panel.grid.major = element_blank(), 
                                             panel.grid.minor = element_blank(), text = element_text(size = fontsize))
  if (facet_alt) {
    th <- th + theme(strip.background = element_blank()) + 
      theme(strip. = element_text(colour = "black"))
  }
  th
}
    
# Create matched set for ATT & placebo
message(paste("Create matched set for ATT & placebo"))
    
# Create formula only if covariates exist
covs_formula <- if (!is.null(covariates) && length(covariates) > 0) {
  as.formula(paste("~", paste(covariates, collapse = " + ")))
} else {
  NULL
}

refinement.method <- "CBPS.weight"

if (!is.null(covariates) && length(covariates) > 0) {
  match <- PanelMatch(
    lag = lag,
    time.id = time_id,
    unit.id = unit_id,
    covs.formula = as.formula(paste("~", paste(covariates, collapse = " + "))),
    treatment = treatments,
    refinement.method = refinement.method,
    data = data,
    match.missing = TRUE,
    qoi = "att",
    outcome.var = outcomes,
    lead = lead,
    forbid.treatment.reversal = FALSE,
    placebo.test = TRUE
  )
} else {
  match <- PanelMatch(
    lag = 2,
    time.id = "t",
    unit.id = "id",
    treatment = "treat_op_median",
    refinement.method = "CBPS.weight",
    data = data,
    match.missing = TRUE,
    qoi = "att",
    outcome.var = "log_CLI_amount",
    lead = c(0:3),
    forbid.treatment.reversal = FALSE,
    placebo.test = TRUE
  )
}


# Estimate ATT
message(paste("Estimate ATT"))

est <-
  PanelEstimate(sets = match,
                data = data,
                se.method = se_method)

# Extract relevant portions of the summary data frames for each lead
summary_est <- summary(est)$summary

lead_rows <-
  lead + 1  # Assuming leads start from 0 and the summary is indexed starting from 1
tmp_results <- data.frame(
  treatment = rep(treatment, length(lead)),
  outcome = rep(outcome, length(lead)),
  t = rep(lead, each = 1),
  # Now dynamic
  estimate = summary_est[lead_rows, 1],
  conf.low = summary_est[lead_rows, 3],
  conf.high = summary_est[lead_rows, 4],
  # calculate 90% confidence intervals
  conf.low90 = summary_est[lead_rows, 1] - summary_est[lead_rows, 2] * qnorm(0.95),
  conf.high90 = summary_est[lead_rows, 1] + summary_est[lead_rows, 2] * qnorm(0.95),
  stringsAsFactors = FALSE
)

# Estimate placebo ATT
message(paste("Estimate placebo ATT"))

placebo_df <- placebo_test(
  pm.obj = match, data = data, 
  se.method = se_method,
  number.iterations = placebo_iterations, 
  plot = FALSE)

# # Bootstrap standard errors (function comes from https://github.com/insongkim/PanelMatch/blob/se_comparison/R/placebo_test.R)
# colnames(placebo_df$bootstrapped.estimates) <- names(placebo_df$estimates)
# ses <- apply(placebo_df$bootstrapped.estimates,  2,  sd, na.rm = TRUE)
# placebo_df <- list(
#   estimates = placebo_df$estimates,
#   bootstrapped.estimates = placebo_df$bootstrapped.estimates,
#   standard.errors = ses
# )

# Create data.frame for binding
placebo_df <- data.frame(
  t = c(-1, as.numeric(str_remove(names(placebo_df$estimates), "t"))),
  estimate = c(0, placebo_df$estimates),
  se = c(0, placebo_df$standard.errors),
  conf.low = c(0, placebo_df$estimates - placebo_df$standard.errors * qnorm(0.975)),
  conf.high = c(0, placebo_df$estimates + placebo_df$standard.errors * qnorm(0.975)),
  conf.low90 = c(0, placebo_df$estimates - placebo_df$standard.errors * qnorm(0.95)),
  conf.high90 = c(0, placebo_df$estimates + placebo_df$standard.errors * qnorm(0.95)),
  stringsAsFactors = FALSE
) |>
  select(-se)
rownames(placebo_df) <- NULL

# Bind with tmp_results
tmp_results <- rbind(tmp_results, placebo_df |> mutate(treatment = treatment, outcome = outcome))

# Add these results to the overall dataframe
results_df <- rbind(results_df, tmp_results)

# Plot results
message(paste("Plot results"))

plot_data <- data.frame(
  t = lead,
  estimate = summary_est[lead_rows, 1],
  conf.low = summary_est[lead_rows, 3],
  conf.high = summary_est[lead_rows, 4],
  conf.low90 = summary_est[lead_rows, 1] - summary_est[lead_rows, 2] * qnorm(0.95),
  conf.high90 = summary_est[lead_rows, 1] + summary_est[lead_rows, 2] * qnorm(0.95),
  stringsAsFactors = FALSE
) 

# Bind with placebo
plot_data <- rbind(plot_data, placebo_df)

# Plotting...
plot <- plot_data |>
  ggplot(aes(x = t, y = estimate)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "red",
    linewidth = .25,
    alpha = 0.75
  ) +
  geom_vline(
    xintercept = -0.5,
    linetype = "dashed",
    color = "red",
    linewidth = .25,
    alpha = 0.75
  ) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0,
                linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90),
                width = 0,
                linewidth = 1.25) +
  labs(y = "ATT", x = "Relative Time") +
  geom_point(aes(x = t, y = estimate), size = 2, shape = 21, fill = "white") +
  # scale_x_continuous(breaks = lead) +
  theme_vincent()

# Save
ggsave(
  filename = paste0(treatment, "_", outcome, "_att.pdf"),
  plot = plot,
  path = figure_path,
  width = 5,
  height = 5.5
)