### Firms & Lobbying
### Sensitivity Analysis

rm(list=ls())

# no scientific notation
options(scipen = 999)

# devtools::install_github("chadhazlett/sensemakr")

# Load packages
pacman::p_load(data.table, tidyverse, fixest, sensemakr, kableExtra, ggrepel, metR, haschaR)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df_orig <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.rds")
df <- df_orig
names(df)

# exclude obs where industry == NA
df <- df[!is.na(df$industry), ]

# Augment data w/ additional variables
df$subs_iso3c <- unlist(lapply(df$subs_iso3c, FUN = function(x) paste(gsub("n.a.", "", unique(unlist(strsplit(x, "\n")))), collapse="|")))
# sum(df$subs_iso3c=="n.a.")
# sum(df$subs_iso3c=="")

df$subs_iso3c <- gsub("NA", "", df$subs_iso3c)

df$subs_iso3c[which(nchar(df$subs_iso3c)==3)] <- gsub("\\|", "", df$subs_iso3c[which(nchar(df$subs_iso3c)==3)])
df$subs_iso3c[which(df$subs_iso3c=="")] <- NA

df$country_iso_code[which(df$isin=="NL00150002Q7")] <- "DE"

df$multinational <- 0
df$multinational[which(df$subs_iso3c != df$country_iso_code)] <- 1

## If the subsidiary column isnt EXACTLY equal to the man country column, this
## implies there is a subsidiary in another country.

newdat <- read.csv("data/01_raw/boards_rep_v2/analysis_data_no_proprietary.csv", stringsAsFactors = F)
newdat <- newdat[ , c("gvkey", "year", "cso_exists", "cdp_report")]
df <- merge(df, newdat, by=c("gvkey", "year"), all.x=T)

df$cso_exists[which(is.na(df$cso_exists) & df$year %in% c(2000:2019))] <- 0
df$cdp_report[which(is.na(df$cdp_report) & df$year %in% c(2000:2019))] <- 0


# Run Models --------------------------------------------------------------


df$CLI <- as.numeric(df$CLI_quarter)

df$log_CLI_amount <- log(df$CLI_amount_quarter +1)

m_occurrence <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year, df, vcov = ~ year + isin)
m_occurrence_iid <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year, df, vcov = "iid")
m_occurrence_scaled <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | industry_year, df, vcov = ~ year + isin)

m_amount <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year, df, vcov = ~ year + isin)
m_amount_scaled <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | industry_year, df, vcov = ~ year + isin)


# Get minimal statistics ---------------------------------------------------

# # Some inspections
# sense <- sensemakr(
#   estimate = coef(m_occurrence)[["op_expo_ew"]],
#   se = se(m_occurrence)[["op_expo_ew"]],
#   dof = df.residual(m_occurrence),
#   treatment = "op_expo_ew"
# )

# summary(sense)

# sense$sensitivity_stats

# ovb_minimal_reporting(sense)

# se(m_occurrence)["op_expo_ew"]
# se(m_occurrence_iid)["op_expo_ew"]
# # clustering makes huge difference for ses and t-stats

# Function to calculate sensitivity stats with clustered SEs
get_sensitivity_stats_clustered <- function(model, treatment_var, covar) {
  # get partial R2
  model_vars <- names(coef(model))
  model_vars <- model_vars[!grepl("^industry_year", model_vars)] # Remove fixed effects
  other_vars <- setdiff(model_vars, treatment_var)
  
  formula_str <- paste0(treatment_var, " ~ ", 
                        paste(other_vars, collapse = " + "), 
                        " | industry_year")
  
  # Fit model and handle potential errors
  D_on_Z <- feols(as.formula(formula_str), data = df, vcov = ~ year + isin)
  
  # Get t.stat from D_on_Y and D_on_Z
  t_stat_treatment <- tstat(D_on_Z)[[covar]]
  t_stat_outcome <- tstat(model)[[covar]]

  # Get df.residual from D_on_Z and model
  df_residual_D_on_Z <- df.residual(D_on_Z)
  df_residual_model <- df.residual(model)
  
  # Get partial R2 values
  r2dxj.x <- partial_r2(t_statistic = t_stat_treatment, 
                        dof = df_residual_D_on_Z)
  r2yxj.dx <- partial_r2(t_statistic = t_stat_outcome, 
                        dof = df_residual_model)


  # Create sensitivity analysis using manual inputs
  sense <- sensemakr(
    estimate = coef(model)[treatment_var],
    se = se(model)[treatment_var],
    dof = df.residual(model),
    r2dxj.x = r2dxj.x,
    r2yxj.dx = r2yxj.dx,
    treatment = treatment_var,
    benchmark_covariates = covar,
    kd = 10
  )
  
  # Extract stats and convert to data.frame
  stats_df <- as.data.frame(sense$sensitivity_stats)
  stats_df$model_type <- if(grepl("amount", deparse(substitute(model)))) "Amount" else "Occurrence"

  # Extract info on bounds
  stats_df$bound_label <- sense$bounds$bound_label
  stats_df$r2dz.x <- sense$bounds$r2dz.x
  stats_df$r2yz.dx <- sense$bounds$r2yz.dx
  
  return(stats_df)
}

# Get stats for both models and all treatment variables
sensitivity_results <- rbind(
  get_sensitivity_stats_clustered(m_occurrence, "op_expo_ew", "us_dummy"),
  get_sensitivity_stats_clustered(m_occurrence, "rg_expo_ew", "us_dummy"),
  get_sensitivity_stats_clustered(m_occurrence, "ph_expo_ew", "us_dummy"),
  get_sensitivity_stats_clustered(m_amount, "op_expo_ew", "us_dummy"),
  get_sensitivity_stats_clustered(m_amount, "rg_expo_ew", "us_dummy"),
  get_sensitivity_stats_clustered(m_amount, "ph_expo_ew", "us_dummy")
) |>
  mutate(treatment = case_when(
    treatment == "op_expo_ew" ~ "Opportunity",
    treatment == "rg_expo_ew" ~ "Regulatory",
    treatment == "ph_expo_ew" ~ "Physical",
    TRUE ~ treatment
  ))
  
sensitivity_table <- sensitivity_results |>
  select(model_type, treatment, estimate, se, t_statistic, r2yd.x, rv_q, rv_qa, r2dz.x, r2yz.dx) |>
  mutate(r2yd.x = paste0(format(r2yd.x * 100, digits = 1), "\\%"),
         rv_q = paste0(format(rv_q * 100, digits = 1), "\\%"), 
         rv_qa = paste0(format(rv_qa * 100, digits = 1), "\\%"),
         r2dz.x = paste0(format(r2dz.x * 100, digits = 1), "\\%"),
         r2yz.dx = paste0(format(r2yz.dx * 100, digits = 1), "\\%")) |>
  kbl(format = "latex", booktabs = TRUE, 
      linesep = c("", "", "\\addlinespace", "", "", ""),
      col.names = c("Model", "Treatment", "Estimate", "SE", "t-stat", "$R^2_{Y\\sim D|\\mathbf{X}}$", "$RV_{q=1}$", "$RV_{q=1,\\alpha=0.05}$", "$R^2_{D\\sim Z|\\mathbf{X}}$", "$R^2_{Y\\sim Z|\\mathbf{X},D}$"),
      digits = c(0, 0, 4, 4, 2, 2, 2, 2, 2, 2),
      escape = FALSE,
      caption = "Sensitivity Analysis to Unobserved Confounding \\label{tab:sensitivity_stats}") |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  add_header_above(c(" " = 2, "Statistics" = 3, "Robustness Values" = 3, "Bounds (50x Z)" = 2))

sensitivity_table


# Create sensitivity plot dataframes ---------------------------------------------------


# Augment models
m_occurrence_aug <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + cso_exists + cdp_report + n_employees + multinational | industry_year, df, vcov = ~ year + isin)

m_amount_aug <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + cso_exists + cdp_report + n_employees + multinational | industry_year, df, vcov = ~ year + isin)


model_vars <- names(coef(m_occurrence_aug))
model_vars <- model_vars[!grepl("^industry_year", model_vars)] # Remove fixed effects
other_vars <- setdiff(model_vars, "op_expo_ew")

formula_str <- paste0("op_expo_ew ~ ", paste(other_vars, collapse = " + "), " | industry_year")
formula_str

D_on_Z <- tryCatch({
      feols(as.formula(formula_str), data = df, vcov = ~ year + isin)
    }, error = function(e) {
      warning(paste("Error fitting model for covariate:", cov, "-", e$message))
      return(NULL)
    })

get_sensitivity_dfs <- function(model, treatment_var, covariates, model_type) {
  # Create empty list to store sensitivity analyses
  sense_list <- list()
  
  # Create sensitivity analysis for each covariate
  for(cov in covariates) {
    # Create formula for treatment regression
    # Include all variables from original model except the current covariate
    model_vars <- names(coef(model))
    model_vars <- model_vars[!grepl("^industry_year", model_vars)] # Remove fixed effects
    other_vars <- setdiff(model_vars, treatment_var)
    
    formula_str <- paste0(treatment_var, " ~ ", 
                         paste(other_vars, collapse = " + "), 
                         " | industry_year")
    
    # Fit model and handle potential errors
    D_on_Z <- feols(as.formula(formula_str), data = df, vcov = ~ year + isin)
    
    # Get t.stat from D_on_Y and D_on_Z
    t_stat_treatment <- tstat(D_on_Z)[[cov]]
    t_stat_outcome <- tstat(model)[[cov]]

    # Get df.residual from D_on_Z and model
    df_residual_D_on_Z <- df.residual(D_on_Z)
    df_residual_model <- df.residual(model)
    
    # Get partial R2 values
    r2dxj.x <- partial_r2(t_statistic = t_stat_treatment, 
                         dof = df_residual_D_on_Z)
    r2yxj.dx <- partial_r2(t_statistic = t_stat_outcome, 
                         dof = df_residual_model)
    
    # Create sensitivity analysis
    sense_list[[cov]] <- sensemakr(
      estimate = coef(model)[[treatment_var]],
      se = se(model)[[treatment_var]],
      dof = df.residual(model),
      treatment = treatment_var,
      r2dxj.x = r2dxj.x,
      r2yxj.dx = r2yxj.dx,
      kd = c(50, 100),
      benchmark_covariates = cov
    )
  }
  return(sense_list)
}

covariates <- c("ebit", "ebit_at", "us_dummy", "total_lobby_quarter", "cso_exists", "cdp_report", "n_employees", "multinational")

# Create all plots with error handling
sensitivity_dfs <- list()
for (model_info in list(
  list(model = m_occurrence_aug, type = "Occurrence"),
  list(model = m_amount_aug, type = "Amount")
)) {
  message("\nProcessing model type:", model_info$type)
  for (treatment in c("op_expo_ew", "rg_expo_ew")) {
    message("Processing treatment:", treatment)
    sensitivity_dfs[[paste(model_info$type, treatment)]] <- 
        get_sensitivity_dfs(model_info$model, treatment, covariates, model_info$type)
  }
}

sensitivity_dfs

sensitivity_dfs[[1]]$ebit
sensitivity_dfs[[2]]$ebit

sensitivity_dfs[[1]]$ebit$bounds
sensitivity_dfs[[2]]$ebit$bounds

# Occurence - opportunity ---------------------------------------------------

## Create sensitivity plots ---------------------------------------------------


# Get contour data from first sensitivity analysis (they're all the same)
sensitivity_df_oc_op <- sensitivity_dfs[[1]]$ebit
sensitivity_df_oc_op$sensitivity_stats
sensitivity_df_oc_op$info
sensitivity_df_oc_op$bounds
dat_oc_op <- plot(sensitivity_df_oc_op)

# Contours are equivalent for each treatment-DV combination

# Prepare contour data
contour_oc_op <- dat_oc_op$value %>%
  as.data.frame() %>%
  `rownames<-`(dat_oc_op$r2dz.x) %>%
  `colnames<-`(dat_oc_op$r2yz.dx) %>%
  rownames_to_column("x") %>%
  pivot_longer(cols = -"x",
                names_to = "y",
                values_to = "estim") %>%
  mutate(x = as.numeric(x),
          y = as.numeric(y))

head(contour_oc_op)


# Initialize an empty list to store all bounds
all_bounds_oc_op <- list()

# Loop over each covariate
for (cov in covariates) {
  # Get contour data for the current covariate
  # Using get() to dynamically access the covariate name from the sensitivity_dfs object
  data <- sensitivity_dfs[[1]][[cov]]
  bounds <- data$bounds |>
  mutate(
    # remove all non-digits from bound_label
    bound_label = str_remove_all(bound_label, "[^0-9]")
  )
  
  # Add the bounds to our list with the covariate name
  all_bounds_oc_op[[cov]] <- bounds
}

# If you want to combine all bounds into a single dataframe with an identifier
# You can do this (optional):
combined_bounds_oc_op <- do.call(rbind, lapply(names(all_bounds_oc_op), function(cov) {
  data <- all_bounds_oc_op[[cov]]
  data$covariate <- cov  # Add column to identify the covariate
  return(data)
})) |>
  mutate(
    covariate = case_when(
      covariate == "ebit" ~ "EBIT",
      covariate == "ebit_at" ~ "EBIT/Assets",
      covariate == "us_dummy" ~ "US Headquarter",
      covariate == "total_lobby_quarter" ~ "Total Lobbying",
      covariate == "cso_exists" ~ "CSO Exists",
      covariate == "cdp_report" ~ "CDP Report",
      covariate == "n_employees" ~ "Number of Employees",
      covariate == "multinational" ~ "Multinational"
    )
  )

# Get unadjusted point
unadjusted_point_oc_op <- sensitivity_df_oc_op$sensitivity_stats

names(unadjusted_point_oc_op)

names(combined_bounds_oc_op)

points_oc_op <- combined_bounds_oc_op |>
    filter(bound_label == 100) |>
    select(bound_label, x = r2dz.x, y = r2yz.dx, adjusted_estimate, covariate) |>
    mutate(
      label = paste0(covariate, " ", bound_label, "x",
                     " (", 
                     sprintf("%.2f", adjusted_estimate), 
                     ")"),
      group = "Adjusted"
    ) |>
    select(-bound_label, -covariate, -adjusted_estimate) |>
    bind_rows(unadjusted_point_oc_op |>
    select(estimate, y = r2yd.x) |>
    mutate(
      label = paste0("Unadjusted (", sprintf("%.2f", estimate), ")"),
      x = 0,
      group = "Unadjusted"
    ) |>
    select(-estimate)
    )


# combine into list
plot_list_oc_op <- list(contour = contour_oc_op, points = points_oc_op)

# Define breaks for contour plot
breaks <- seq(-0.3, 0.05, by = 0.005)

# Create plot
ggplot() +
  # Add contours
  # Add contours for non-zero levels in grey
  geom_contour(
    data = plot_list_oc_op$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = breaks[breaks != 0],
    color = "grey70",
    linetype = "solid",
    show.legend = FALSE
  ) +
  # add little labels on the lines that tell us what each contour line is:
  geom_label_contour(data = plot_list_oc_op$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = breaks[breaks != 0], # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "grey70"
                     ) +
  # Add zero contour in red
  geom_contour(
    data = plot_list_oc_op$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = c(0),
    color = "red", 
    linetype = "longdash",
    show.legend = FALSE
  ) +
  geom_label_contour(data = plot_list_oc_op$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = c(0), # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "red"
                     ) +
  # add labels including the adjusted estimate:
  geom_label_repel(data = plot_list_oc_op$points,
                  mapping = aes(x = x, y = y,
                                label = label,
                                color = group
                                ),
                  show.legend = FALSE,
                  seed = 21,
                  size = 3,
                  #nudge_x = 0.002  # Added nudge to move labels right
                  ) +
  # Add benchmark points
  geom_point(
    data = plot_list_oc_op$points,
    mapping = aes(
      x = x, y = y,
      color = group,
      shape = group
      # shape = bound_label
    ),
    size = 3
  ) +
  scale_color_manual(values = c("black", "red")) +
  # scale_shape_manual(values = c(21, 24)) +
  # Formatting
  scale_linetype_manual(values = c("solid", "longdash")) +
  xlab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ treatment)) +
  ylab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ outcome)) +
  # color legend no title
  # guides(color = guide_legend(title = NULL)) +
  theme_hanno() +
  theme(
    legend.position = "none") +
  xlim(0, 0.0075) +
  ylim(0, 0.05) 

ggsave("results/figures/sensitivity/sensitivity_plot_occurrence_op.pdf", width = 7, height = 7)


# Occurence - regulatory ---------------------------------------------------

## Create sensitivity plots ---------------------------------------------------


# Get contour data from first sensitivity analysis (they're all the same)
sensitivity_df_oc_rg <- sensitivity_dfs[[2]]$ebit
sensitivity_df_oc_rg$sensitivity_stats
sensitivity_df_oc_rg$info
sensitivity_df_oc_rg$bounds
dat_oc_rg <- plot(sensitivity_df_oc_rg)

# Contours are equivalent for each treatment-DV combination

# Prepare contour data
contour_oc_rg <- dat_oc_rg$value %>%
  as.data.frame() %>%
  `rownames<-`(dat_oc_rg$r2dz.x) %>%
  `colnames<-`(dat_oc_rg$r2yz.dx) %>%
  rownames_to_column("x") %>%
  pivot_longer(cols = -"x",
                names_to = "y",
                values_to = "estim") %>%
  mutate(x = as.numeric(x),
          y = as.numeric(y))

head(contour_oc_rg)


# Initialize an empty list to store all bounds
all_bounds_oc_rg <- list()

# Loop over each covariate
for (cov in covariates) {
  # Get contour data for the current covariate
  # Using get() to dynamically access the covariate name from the sensitivity_dfs object
  data <- sensitivity_dfs[[2]][[cov]]
  bounds <- data$bounds |>
  mutate(
    # remove all non-digits from bound_label
    bound_label = str_remove_all(bound_label, "[^0-9]")
  )
  
  # Add the bounds to our list with the covariate name
  all_bounds_oc_rg[[cov]] <- bounds
}

# If you want to combine all bounds into a single dataframe with an identifier
# You can do this (optional):
combined_bounds_oc_rg <- do.call(rbind, lapply(names(all_bounds_oc_rg), function(cov) {
  data <- all_bounds_oc_rg[[cov]]
  data$covariate <- cov  # Add column to identify the covariate
  return(data)
})) |>
  mutate(
    covariate = case_when(
      covariate == "ebit" ~ "EBIT",
      covariate == "ebit_at" ~ "EBIT/Assets",
      covariate == "us_dummy" ~ "US Headquarter",
      covariate == "total_lobby_quarter" ~ "Total Lobbying",
      covariate == "cso_exists" ~ "CSO Exists",
      covariate == "cdp_report" ~ "CDP Report",
      covariate == "n_employees" ~ "Number of Employees",
      covariate == "multinational" ~ "Multinational"
    )
  )

# Get unadjusted point
unadjusted_point_oc_rg <- sensitivity_df_oc_rg$sensitivity_stats

names(unadjusted_point_oc_rg)

names(combined_bounds_oc_rg)

points_oc_rg <- combined_bounds_oc_rg |>
    filter(bound_label == 100) |>
    select(bound_label, x = r2dz.x, y = r2yz.dx, adjusted_estimate, covariate) |>
    mutate(
      label = paste0(covariate, " ", bound_label, "x",
                     " (", 
                     sprintf("%.2f", adjusted_estimate), 
                     ")"),
      group = "Adjusted"
    ) |>
    select(-bound_label, -covariate, -adjusted_estimate) |>
    bind_rows(unadjusted_point_oc_rg |>
    select(estimate, y = r2yd.x) |>
    mutate(
      label = paste0("Unadjusted (", sprintf("%.2f", estimate), ")"),
      x = 0,
      group = "Unadjusted"
    ) |>
    select(-estimate)
    )


# combine into list
plot_list_oc_rg <- list(contour = contour_oc_rg, points = points_oc_rg)

# Define breaks for contour plot
breaks <- seq(-0.3, 0.05, by = 0.005)

# Create plot
ggplot() +
  # Add contours
  # Add contours for non-zero levels in grey
  geom_contour(
    data = plot_list_oc_rg$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = breaks[breaks != 0],
    color = "grey70",
    linetype = "solid",
    show.legend = FALSE
  ) +
  # add little labels on the lines that tell us what each contour line is:
  geom_label_contour(data = plot_list_oc_rg$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = breaks[breaks != 0], # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "grey70"
                     ) +
  # Add zero contour in red
  geom_contour(
    data = plot_list_oc_rg$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = c(0),
    color = "red", 
    linetype = "longdash",
    show.legend = FALSE
  ) +
  geom_label_contour(data = plot_list_oc_rg$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = c(0), # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "red"
                     ) +
  # add labels including the adjusted estimate:
  geom_label_repel(data = plot_list_oc_rg$points,
                  mapping = aes(x = x, y = y,
                                label = label,
                                color = group
                                ),
                  show.legend = FALSE,
                  seed = 21,
                  size = 3,
                  #nudge_x = 0.002  # Added nudge to move labels right
                  ) +
  # Add benchmark points
  geom_point(
    data = plot_list_oc_rg$points,
    mapping = aes(
      x = x, y = y,
      color = group,
      shape = group
      # shape = bound_label
    ),
    size = 3
  ) +
  scale_color_manual(values = c("black", "red")) +
  # scale_shape_manual(values = c(21, 24)) +
  # Formatting
  scale_linetype_manual(values = c("solid", "longdash")) +
  xlab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ treatment)) +
  ylab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ outcome)) +
  # color legend no title
  # guides(color = guide_legend(title = NULL)) +
  theme_hanno() +
  theme(
    legend.position = "none") +
  xlim(0, 0.0075) +
  ylim(0, 0.05) 

ggsave("results/figures/sensitivity/sensitivity_plot_occurrence_rg.pdf", width = 7, height = 7)



# Expenditure - opportunity ---------------------------------------------------

## Create sensitivity plots ---------------------------------------------------


# Get contour data from first sensitivity analysis (they're all the same)
sensitivity_df_ex_op <- sensitivity_dfs[[3]]$ebit
sensitivity_df_ex_op$sensitivity_stats
sensitivity_df_ex_op$info
sensitivity_df_ex_op$bounds
dat_ex_op <- plot(sensitivity_df_ex_op)

# Contours are equivalent for each treatment-DV combination

# Prepare contour data
contour_ex_op <- dat_ex_op$value %>%
  as.data.frame() %>%
  `rownames<-`(dat_ex_op$r2dz.x) %>%
  `colnames<-`(dat_ex_op$r2yz.dx) %>%
  rownames_to_column("x") %>%
  pivot_longer(cols = -"x",
                names_to = "y",
                values_to = "estim") %>%
  mutate(x = as.numeric(x),
          y = as.numeric(y))

head(contour_ex_op)


# Initialize an empty list to store all bounds
all_bounds_ex_op <- list()

# Loop over each covariate
for (cov in covariates) {
  # Get contour data for the current covariate
  # Using get() to dynamically access the covariate name from the sensitivity_dfs object
  data <- sensitivity_dfs[[3]][[cov]]
  bounds <- data$bounds |>
  mutate(
    # remove all non-digits from bound_label
    bound_label = str_remove_all(bound_label, "[^0-9]")
  )
  
  # Add the bounds to our list with the covariate name
  all_bounds_ex_op[[cov]] <- bounds
}

# If you want to combine all bounds into a single dataframe with an identifier
# You can do this (optional):
combined_bounds_ex_op <- do.call(rbind, lapply(names(all_bounds_ex_op), function(cov) {
  data <- all_bounds_ex_op[[cov]]
  data$covariate <- cov  # Add column to identify the covariate
  return(data)
})) |>
  mutate(
    covariate = case_when(
      covariate == "ebit" ~ "EBIT",
      covariate == "ebit_at" ~ "EBIT/Assets",
      covariate == "us_dummy" ~ "US Headquarter",
      covariate == "total_lobby_quarter" ~ "Total Lobbying",
      covariate == "cso_exists" ~ "CSO Exists",
      covariate == "cdp_report" ~ "CDP Report",
      covariate == "n_employees" ~ "Number of Employees",
      covariate == "multinational" ~ "Multinational"
    )
  )

# Get unadjusted point
unadjusted_point_ex_op <- sensitivity_df_ex_op$sensitivity_stats

names(unadjusted_point_ex_op)

names(combined_bounds_ex_op)

points_ex_op <- combined_bounds_ex_op |>
    filter(bound_label == 100) |>
    select(bound_label, x = r2dz.x, y = r2yz.dx, adjusted_estimate, covariate) |>
    mutate(
      label = paste0(covariate, " ", bound_label, "x",
                     " (", 
                     sprintf("%.2f", adjusted_estimate), 
                     ")"),
      group = "Adjusted"
    ) |>
    select(-bound_label, -covariate, -adjusted_estimate) |>
    bind_rows(unadjusted_point_ex_op |>
    select(estimate, y = r2yd.x) |>
    mutate(
      label = paste0("Unadjusted (", sprintf("%.2f", estimate), ")"),
      x = 0,
      group = "Unadjusted"
    ) |>
    select(-estimate)
    )


# combine into list
plot_list_ex_op <- list(contour = contour_ex_op, points = points_ex_op)

# Define breaks for contour plot
breaks <- seq(-0.3, 0.3, by = 0.05) |> round(2)

# Create plot
ggplot() +
  # Add contours
  # Add contours for non-zero levels in grey
  geom_contour(
    data = plot_list_ex_op$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = breaks[breaks != 0],
    color = "grey70",
    linetype = "solid",
    show.legend = FALSE
  ) +
  # add little labels on the lines that tell us what each contour line is:
  geom_label_contour(data = plot_list_ex_op$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = breaks[breaks != 0], # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "grey70"
                     ) +
  # Add zero contour in red
  geom_contour(
    data = plot_list_ex_op$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = c(0),
    color = "red", 
    linetype = "longdash",
    show.legend = FALSE
  ) +
  geom_label_contour(data = plot_list_ex_op$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = c(0), # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "red"
                     ) +
  # add labels including the adjusted estimate:
  geom_label_repel(data = plot_list_ex_op$points,
                  mapping = aes(x = x, y = y,
                                label = label,
                                color = group
                                ),
                  show.legend = FALSE,
                  seed = 21,
                  size = 3,
                  #nudge_x = 0.002  # Added nudge to move labels right
                  ) +
  # Add benchmark points
  geom_point(
    data = plot_list_ex_op$points,
    mapping = aes(
      x = x, y = y,
      color = group,
      shape = group
      # shape = bound_label
    ),
    size = 3
  ) +
  scale_color_manual(values = c("black", "red")) +
  # scale_shape_manual(values = c(21, 24)) +
  # Formatting
  scale_linetype_manual(values = c("solid", "longdash")) +
  xlab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ treatment)) +
  ylab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ outcome)) +
  # color legend no title
  # guides(color = guide_legend(title = NULL)) +
  theme_hanno() +
  theme(
    legend.position = "none") +
  xlim(0, 0.0075) +
  ylim(0, 0.05) 

ggsave("results/figures/sensitivity/sensitivity_plot_expenditure_op.pdf", width = 7, height = 7)


# Expenditure - regulatory ---------------------------------------------------

## Create sensitivity plots ---------------------------------------------------


# Get contour data from first sensitivity analysis (they're all the same)
sensitivity_df_ex_rg <- sensitivity_dfs[[4]]$ebit
sensitivity_df_ex_rg$sensitivity_stats
sensitivity_df_ex_rg$info
sensitivity_df_ex_rg$bounds
dat_ex_rg <- plot(sensitivity_df_ex_rg)

# Contours are equivalent for each treatment-DV combination

# Prepare contour data
contour_ex_rg <- dat_ex_rg$value %>%
  as.data.frame() %>%
  `rownames<-`(dat_ex_rg$r2dz.x) %>%
  `colnames<-`(dat_ex_rg$r2yz.dx) %>%
  rownames_to_column("x") %>%
  pivot_longer(cols = -"x",
                names_to = "y",
                values_to = "estim") %>%
  mutate(x = as.numeric(x),
          y = as.numeric(y))

head(contour_ex_rg)


# Initialize an empty list to store all bounds
all_bounds_ex_rg <- list()

# Loop over each covariate
for (cov in covariates) {
  # Get contour data for the current covariate
  # Using get() to dynamically access the covariate name from the sensitivity_dfs object
  data <- sensitivity_dfs[[4]][[cov]]
  bounds <- data$bounds |>
  mutate(
    # remove all non-digits from bound_label
    bound_label = str_remove_all(bound_label, "[^0-9]")
  )
  
  # Add the bounds to our list with the covariate name
  all_bounds_ex_rg[[cov]] <- bounds
}

# If you want to combine all bounds into a single dataframe with an identifier
# You can do this (optional):
combined_bounds_ex_rg <- do.call(rbind, lapply(names(all_bounds_ex_rg), function(cov) {
  data <- all_bounds_ex_rg[[cov]]
  data$covariate <- cov  # Add column to identify the covariate
  return(data)
})) |>
  mutate(
    covariate = case_when(
      covariate == "ebit" ~ "EBIT",
      covariate == "ebit_at" ~ "EBIT/Assets",
      covariate == "us_dummy" ~ "US Headquarter",
      covariate == "total_lobby_quarter" ~ "Total Lobbying",
      covariate == "cso_exists" ~ "CSO Exists",
      covariate == "cdp_report" ~ "CDP Report",
      covariate == "n_employees" ~ "Number of Employees",
      covariate == "multinational" ~ "Multinational"
    )
  )

# Get unadjusted point
unadjusted_point_ex_rg <- sensitivity_df_ex_rg$sensitivity_stats

names(unadjusted_point_ex_rg)

names(combined_bounds_ex_rg)

points_ex_rg <- combined_bounds_ex_rg |>
    filter(bound_label == 100) |>
    select(bound_label, x = r2dz.x, y = r2yz.dx, adjusted_estimate, covariate) |>
    mutate(
      label = paste0(covariate, " ", bound_label, "x",
                     " (", 
                     sprintf("%.2f", adjusted_estimate), 
                     ")"),
      group = "Adjusted"
    ) |>
    select(-bound_label, -covariate, -adjusted_estimate) |>
    bind_rows(unadjusted_point_ex_rg |>
    select(estimate, y = r2yd.x) |>
    mutate(
      label = paste0("Unadjusted (", sprintf("%.2f", estimate), ")"),
      x = 0,
      group = "Unadjusted"
    ) |>
    select(-estimate)
    )


# combine into list
plot_list_ex_rg <- list(contour = contour_ex_rg, points = points_ex_rg)

# Define breaks for contour plot
breaks <- seq(-0.3, 0.3, by = 0.05) |> round(2)

# Create plot
ggplot() +
  # Add contours
  # Add contours for non-zero levels in grey
  geom_contour(
    data = plot_list_ex_rg$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = breaks[breaks != 0],
    color = "grey70",
    linetype = "solid",
    show.legend = FALSE
  ) +
  # add little labels on the lines that tell us what each contour line is:
  geom_label_contour(data = plot_list_ex_rg$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = breaks[breaks != 0], # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "grey70"
                     ) +
  # Add zero contour in red
  geom_contour(
    data = plot_list_ex_rg$contour,
    mapping = aes(
      x = x, y = y, z = estim
    ),
    breaks = c(0),
    color = "red", 
    linetype = "longdash",
    show.legend = FALSE
  ) +
  geom_label_contour(data = plot_list_ex_rg$contour,
                     mapping = aes(x = x, y = y, z = estim),
                     breaks = c(0), # same splits as above
                     # next are some arguments for controlling aesthetics:
                     label.size = 0, # I don't want a label box
                     skip = 0, # don't skip any line while labelling
                     show.legend = FALSE,
                     color = "red"
                     ) +
  # add labels including the adjusted estimate:
  geom_label_repel(data = plot_list_ex_rg$points,
                  mapping = aes(x = x, y = y,
                                label = label,
                                color = group
                                ),
                  show.legend = FALSE,
                  seed = 21,
                  size = 3,
                  #nudge_x = 0.002  # Added nudge to move labels right
                  ) +
  # Add benchmark points
  geom_point(
    data = plot_list_ex_rg$points,
    mapping = aes(
      x = x, y = y,
      color = group,
      shape = group
      # shape = bound_label
    ),
    size = 3
  ) +
  scale_color_manual(values = c("black", "red")) +
  # scale_shape_manual(values = c(21, 24)) +
  # Formatting
  scale_linetype_manual(values = c("solid", "longdash")) +
  xlab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ treatment)) +
  ylab(expression(Partial ~ R^2 ~ of ~ confounders ~ with ~ the ~ outcome)) +
  # color legend no title
  # guides(color = guide_legend(title = NULL)) +
  theme_hanno() +
  theme(
    legend.position = "none") +
  xlim(0, 0.0075) +
  ylim(0, 0.05) 

ggsave("results/figures/sensitivity/sensitivity_plot_expenditure_rg.pdf", width = 7, height = 7)


# Lagged DV ---------------------------------------------------------------

# create lag
df <- df |>
  # group by unit (in our case: firm)
  group_by(isin) |>
  # arrange by year
  arrange(yearqtr) |>
  # for one year
  mutate(#log_co2_l1 = lag(log_co2, 1),
    CLI_lag = lag(CLI, 1),
    CLI_amount_lag = lag(log_CLI_amount, 1)) |>
  #ungroup
  ungroup()

# Augment models
m_occurrence_ldv <- feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_lag | industry_year,
  df,
  vcov = ~isin
)

m_amount_ldv <- feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_amount_lag | industry_year,
  df,
  vcov = ~isin
)


get_bounds_ldv <- function(model, treatment_var, lagged_var) {
  # Fit model for treatment on other variables
  model_vars <- names(coef(model))
  model_vars <- model_vars[!grepl("^industry_year", model_vars)] # Remove fixed effects
  other_vars <- setdiff(model_vars, treatment_var)
  
  formula_str <- paste0(treatment_var, " ~ ", 
                       paste(other_vars, collapse = " + "), 
                       " | industry_year")
  
  # Fit auxiliary model
  D_on_Z <- feols(as.formula(formula_str), data = df, vcov = ~ isin)
  
  # Get t-statistics
  t_stat_treatment <- tstat(D_on_Z)[[lagged_var]]
  t_stat_outcome <- tstat(model)[[lagged_var]]
  
  # Get degrees of freedom
  df_residual_D_on_Z <- df.residual(D_on_Z)
  df_residual_model <- df.residual(model)
  
  # Calculate partial R2 values
  r2dxj.x <- partial_r2(t_statistic = t_stat_treatment, 
                        dof = df_residual_D_on_Z)
  r2yxj.dx <- partial_r2(t_statistic = t_stat_outcome, 
                        dof = df_residual_model)
  
  # Run sensitivity analysis
  sense <- sensemakr(
    estimate = coef(model)[[treatment_var]],
    se = se(model)[[treatment_var]],
    dof = df.residual(model),
    r2dxj.x = r2dxj.x,
    r2yxj.dx = r2yxj.dx,
    treatment = treatment_var,
    benchmark_covariates = lagged_var,
    kd = c(0, 0.5, 1)
  )
  
  # Return bounds
  return(sense$bounds)
}

# For occurrence model
bounds_occurrence_op <- get_bounds_ldv(m_occurrence_ldv, "op_expo_ew", "CLI_lag") |> mutate(treatment = "Opportunity")
bounds_occurrence_rg <- get_bounds_ldv(m_occurrence_ldv, "rg_expo_ew", "CLI_lag") |> mutate(treatment = "Regulatory")
bounds_occurrence_ph <- get_bounds_ldv(m_occurrence_ldv, "ph_expo_ew", "CLI_lag") |> mutate(treatment = "Physical")

# For amount model
bounds_amount_op <- get_bounds_ldv(m_amount_ldv, "op_expo_ew", "CLI_amount_lag") |> mutate(treatment = "Opportunity")
bounds_amount_rg <- get_bounds_ldv(m_amount_ldv, "rg_expo_ew", "CLI_amount_lag") |> mutate(treatment = "Regulatory")
bounds_amount_ph <- get_bounds_ldv(m_amount_ldv, "ph_expo_ew", "CLI_amount_lag") |> mutate(treatment = "Physical")

# Combine bounds
bounds <- bind_rows(bounds_occurrence_op, bounds_occurrence_rg, bounds_occurrence_ph,
                   bounds_amount_op, bounds_amount_rg, bounds_amount_ph) |>
  mutate(dv = case_when(
    str_detect(bound_label, "CLI_lag") ~ "Occurrence",
    str_detect(bound_label, "CLI_amount_lag") ~ "Amount"
  ))

# Coefficient plot
bounds |>
  mutate(
    bound_label = factor(as.numeric(str_remove_all(bound_label, "[a-zA-Z_]")), levels = c(1, 0.5, 0), ordered = TRUE),
    dv = factor(dv, levels = c("Occurrence", "Amount")),
    treatment = factor(treatment, levels = c("Opportunity", "Regulatory", "Physical"))
    ) |>
  ggplot(aes(x = adjusted_estimate, y = bound_label, xmin = adjusted_lower_CI, xmax = adjusted_upper_CI)) +
  facet_wrap(~dv, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "longdash", color = "grey") +
  geom_point(aes(color = treatment, shape = treatment), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(color = treatment), width = 0,
                position = position_dodge(width = 0.5)) +
  labs(y = "Multiplier",
       x = "Adjusted Coefficient Estimate") +
  theme_hanno() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
    ) +
  scale_color_viridis_d(option = "D", end = 0.9) +
  scale_shape_manual(values = c(16, 17, 15))

ggsave("results/figures/sensitivity/sensitivity_plot_ldv.pdf", width = 7, height = 3.5)



# move_plots_to_overleaf("./")





# END
