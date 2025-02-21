### Firms & Climate Lobbying
### Placebo checks


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haschaR, broom, fixest, readxl, kableExtra, equivtest)

# prevent scientific notation
options(scipen = 999)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/Dropbox (Princeton)/BBH/BBH1/")}


###Placebo exploration 
#load dataset
df <- fread("data/03_final/lobbying_df_quarterly_REVISE_NEW_placebos.csv")


glimpse(df)

# read in lobbying issue codes
codes <- read_excel("data/01_raw/lobbyview/lobbying_issue_codes.xlsx")

names(df)

# Convert issue lobbying columns to 0 if NA and some other transformations
issue_columns <- names(df)[which(names(df)=="ENV"):which(names(df)=="MON")]
issue_columns_amount <- paste0(issue_columns, "_amount")


# Plot number of reports by issue code
df %>%
    summarise(
        across(all_of(issue_columns), sum, na.rm = TRUE)
    ) %>%
    # Convert to long format
    pivot_longer(everything(),
        names_to = "issue_code",
        values_to = "count"
    ) %>%
    left_join(
        df %>%
            summarise(
                across(all_of(issue_columns_amount), sum, na.rm = TRUE)
            ) %>%
            # Convert to long format
            pivot_longer(everything(),
                names_to = "issue_code",
                values_to = "amount"
            ) %>%
            # remove _amount from issue_code
            mutate(
                issue_code = str_remove(issue_code, "_amount")
            ),
        by = c("issue_code")
    ) %>%
    pivot_longer(
        cols = c("count", "amount"),
        names_to = "variable",
        values_to = "value"
    ) %>%
    left_join(
        codes,
        by = c("issue_code" = "Code")
    ) %>%
    # Sort by count descending
    arrange(desc(value)) %>%
    mutate(
        variable = ifelse(variable == "count", "Number of Reports", "Total Expenditure (in Mio USD)"),
        value = ifelse(variable == "Total Expenditure (in Mio USD)", value / 1000000, value),
        climate = ifelse(Description %in% c("Energy/Nuclear", "Environmental/Superfund", "Clean Air & Water (Quality)", "Fuel/Gas/Oil"), 1, 0)
        ) %>%
    # Create plot
    ggplot(aes(x = reorder(Description, value), y = value, fill = factor(climate))) +
    facet_wrap(facets = ~variable, scales = "free_x") +
    geom_col() +
    coord_flip() +
    theme_hanno() +
    theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none"
    ) +
    # scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    scale_fill_manual(values = c("grey35", "red"))  

# Save plot
ggsave("results/Figures/descriptives/issue_codes_n_amount.pdf", width = 9, height = 16)



# Occurrence --------------------------------------------------------------

##Identify placebo variables
# Specify columns to remove: climate issues + REL constant zero (no lobbying) +  NA
columns_to_remove <- c("CAW", "ENV", "ENG", "FUE", "REL")


# Remove specified columns
df_placebo <- df |> select(-all_of(columns_to_remove))

names(df_placebo)

start_index <- which(names(df_placebo) == "FIN")
end_index <- which(names(df_placebo) == "MON")

# Extract column names between "AGR" and "REL" into a character vector
dependent_vars <- names(df_placebo)[start_index:end_index]

# Initialize an empty list to store model results
results_list_occurrence <- list()

# Iterate over the dependent variables and fit the feglm model with fixed effects for each

for (dv in dependent_vars) { {
  formula <- as.formula(paste(dv, "~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year")) # Include fixed effects in the formula
  
  # Debugging
  print(formula)
  
  # Fit the feglm model
  model <- feols(formula, data = df_placebo, vcov = ~ isin + year)
  
  # Store the model results in the list
  results_list_occurrence[[paste(dv, sep = "_")]] <- summary(model)
}
}

#summary(results_list_occurrence[[1]])

# Create an empty dataframe to store tidied results
tidied_results <- data.frame()

# Iterate over the results_list to tidy each model's results
for (dv in names(results_list_occurrence)) {
  # Extract 95% confidence intervals
  tidied_model_95 <- tidy(results_list_occurrence[[dv]], conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
    mutate(dependent_var = dv) %>%
    rename(conf.low95 = conf.low, conf.high95 = conf.high)
  
  # Extract 90% confidence intervals
  tidied_model_90 <- tidy(results_list_occurrence[[dv]], conf.int = TRUE, conf.level = 0.90) %>%
    filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
    mutate(dependent_var = dv) %>%
    select(-c(statistic, p.value, std.error, estimate)) %>%
    rename(conf.low90 = conf.low, conf.high90 = conf.high)
  
  # Join the 95% and 90% results by dependent_var and term
  tidied_model <- left_join(tidied_model_95, tidied_model_90, by = c("dependent_var", "term"))
  
  # Add number of observations to the tidied results
  nobs <- nobs(results_list_occurrence[[dv]])
  tidied_model$nobs <- nobs
  
  tidied_results <- bind_rows(tidied_results, tidied_model)
}


# save tidied_results
fwrite(tidied_results, "results/Tables/placebo_results_occurrence_NEW.csv")


# Load tidied results
tidied_results <- fread("results/Tables/placebo_results_occurrence_NEW.csv")


### Plot

# Climate-adjacent issues
climate_adjacent <- c("TAX", "BUD", "TRA", "TRD", "NAT", "HCR", "LBR", "AGR", "HOM", "DEF", "UTI")

tidied_results |>
  mutate(
    term = case_when(
      term == "op_expo_ew" ~ "Opportunity",
      term == "rg_expo_ew" ~ "Regulatory",
      term == "ph_expo_ew" ~ "Physical",
      TRUE ~ term
    ),
    term = factor(term, levels = c("Opportunity", "Regulatory", "Physical")),
    # New color coding based on significance and direction
    color = case_when(
      p.value < 0.05 & estimate > 0 ~ "darkblue", # Blue for positive significant
      p.value < 0.05 & estimate < 0 ~ "darkred", # Red for negative significant
      TRUE ~ "darkgrey" # Grey for non-significant
    ),
    climate_adjacent = ifelse(dependent_var %in% climate_adjacent,
      "Climate-Adjacent",
      "Placebos"
    ),
    dependent_var = factor(dependent_var, levels = rev(levels(factor(dependent_var))))
  ) |>
  ggplot(aes(x = estimate, y = dependent_var, color = color)) +
  facet_grid(climate_adjacent ~ term, scales = "free_y", space = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .25, alpha = 0.75) +
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
  geom_point(size = 2) +
  scale_color_identity(guide = "none") +
  labs(x = "Coefficient") +
  theme_hanno() +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )


ggsave("results/figures/regressions/placebos_occurrence_NEW.pdf", width = 8.5, height = 11)


# Expenditure -------------------------------------------------------------

#names(df)

##Identify placebo variables (+ REL)
columns_to_remove <- c(
  "CAW_amount", "ENV_amount", "ENG_amount", "FUE_amount", 
  "REL_amount")


# Remove specified columns
df_placebo <- df |> select(-all_of(columns_to_remove))

names(df_placebo)

start_index <- which(names(df_placebo) == "FIN_amount")
end_index <- which(names(df_placebo) == "MON_amount")

# Extract column names between "AGR" and "REL" into a character vector
dependent_vars <- names(df_placebo)[start_index:end_index]

# across all dependent vars: take log + 1
df_placebo <- df_placebo %>%
  mutate_at(vars(all_of(dependent_vars)), ~log(. + 1))


# Initialize an empty list to store model results
results_list_expenditure <- list()


for (dv in dependent_vars) { {
  formula <- as.formula(paste(dv, "~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year")) # Include fixed effects in the formula
  
  # Debugging
  print(formula)
  
  # Fit the feglm model
  model <- feols(formula, data = df_placebo, vcov = ~ isin + year)
  
  # Store the model results in the list
  results_list_expenditure[[paste(dv, sep = "_")]] <- summary(model)
}
}

summary(results_list_expenditure[[9]])

# Create an empty dataframe to store tidied results
tidied_results <- data.frame()

# Iterate over the results_list to tidy each model's results
for (dv in names(results_list_expenditure)) {
  # Extract 95% confidence intervals
  tidied_model_95 <- tidy(results_list_expenditure[[dv]], conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
    mutate(dependent_var = dv) %>%
    rename(conf.low95 = conf.low, conf.high95 = conf.high)
  
  # Extract 90% confidence intervals
  tidied_model_90 <- tidy(results_list_expenditure[[dv]], conf.int = TRUE, conf.level = 0.90) %>%
    filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
    mutate(dependent_var = dv) %>%
    select(-c(statistic, p.value, std.error, estimate)) %>%
    rename(conf.low90 = conf.low, conf.high90 = conf.high)
  
  # Join the 95% and 90% results by dependent_var and term
  tidied_model <- left_join(tidied_model_95, tidied_model_90, by = c("dependent_var", "term"))
  
  # Add number of observations to the tidied results
  nobs <- nobs(results_list_expenditure[[dv]])
  tidied_model$nobs <- nobs
  
  tidied_results <- bind_rows(tidied_results, tidied_model)
}


# save tidied_results
fwrite(tidied_results, "results/Tables/pacebo_results_amount_NEW.csv")


# Load tidied results
tidied_results <- fread("results/Tables/pacebo_results_amount_NEW.csv")


### Plot

tidied_results |>
  mutate(
    dependent_var = str_replace(dependent_var, "_amount", ""),
    term = case_when(
      term == "op_expo_ew" ~ "Opportunity",
      term == "rg_expo_ew" ~ "Regulatory",
      term == "ph_expo_ew" ~ "Physical",
      TRUE ~ term
    ),
    term = factor(term, levels = c("Opportunity", "Regulatory", "Physical")),
    # New color coding based on significance and direction
    color = case_when(
      p.value < 0.05 & estimate > 0 ~ "darkblue", # Blue for positive significant
      p.value < 0.05 & estimate < 0 ~ "darkred", # Red for negative significant
      TRUE ~ "darkgrey" # Grey for non-significant
    ),
    climate_adjacent = ifelse(dependent_var %in% climate_adjacent,
      "Climate-Adjacent",
      "Placebos"
    ),
    dependent_var = factor(dependent_var, levels = rev(levels(factor(dependent_var))))
  ) |>
  ggplot(aes(x = estimate, y = dependent_var, color = color)) +
  facet_grid(climate_adjacent ~ term, scales = "free_y", space = "free_y") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = .25, alpha = 0.75) +
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
  geom_point(size = 2) +
  scale_color_identity(guide = "none") +
  labs(x = "Coefficient") +
  theme_hanno() +
  theme(
    axis.title.y = element_blank()
  )

ggsave("results/figures/regressions/placebos_amount_NEW.pdf", width = 8.5, height = 11)


move_plots_to_overleaf("./")






# Equivalence Testing -----------------------------------------------------


# =========================================
# TOST with z-approx for fixest coefficient
# =========================================
# - model_list: a named list of fixest model objects (e.g. results_list_occurrence)
# - dv_names:   character vector of equal length to model_list, specifying each model’s DV name
# - data:       data.frame used to fit the models
# - treat_vars: which coefficient names to run TOST on (default: op_expo_ew, rg_expo_ew, ph_expo_ew)
# - alpha:      significance level (default 0.05)
#
# Output: a data.frame with TOST results for each model & coefficient

tost_equiv_fixest <- function(
  model_list,
  dv_names,
  data,
  treat_vars = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"),
  alpha = 0.05
) {
  # Basic check: must have same length
  if (length(model_list) != length(dv_names)) {
    stop("model_list and dv_names must have the same length!")
  }
  
  # Initialize container for all results
  all_results <- data.frame()
  
  # Loop over models
  for (i in seq_along(model_list)) {
    mod <- model_list[[i]]
    this_dv <- dv_names[i]
    
    # 1) Compute st.dev. of the outcome in 'data'
    #    (NB: If your model uses a transformed outcome, e.g., log( dv + 1 ),
    #    then the SD you get here is for the *transformed* variable!)
    y_sd <- sd(data[[this_dv]], na.rm = TRUE)
    
    # 2) Equivalence range: epsilon = 0.36 * SD
    epsilon <- 0.36 * y_sd
    
    # 3) Extract the coefficient table from the fixest model
    ctab <- summary(mod)$coeftable
    
    # 4) For each treatment variable, run TOST
    for (tv in treat_vars) {
      # If the model doesn't have this coefficient (e.g. omitted), skip or store NA
      if (! tv %in% rownames(ctab)) {
        rowdf <- data.frame(
          dv                 = this_dv,
          treat_var          = tv,
          estimate           = NA_real_,
          se                 = NA_real_,
          epsilon            = epsilon,
          T1                 = NA_real_,
          pval1              = NA_real_,
          T2                 = NA_real_,
          pval2              = NA_real_,
          reject_equivalence = NA,
          stringsAsFactors   = FALSE
        )
        
      } else {
        # Pull out estimate and std. error
        est <- ctab[tv, "Estimate"]
        se  <- ctab[tv, "Std. Error"]
        
        # TOST with normal approximation:
        #   H0 #1: beta >= +epsilon  vs  H1: beta < +epsilon   => T1 = (est - epsilon)/se
        #   H0 #2: beta <= -epsilon  vs  H1: beta > -epsilon   => T2 = (est + epsilon)/se
        # p-value #1 = pnorm(T1)
        # p-value #2 = 1 - pnorm(T2)
        
        T1    <- (est - epsilon) / se
        pval1 <- pnorm(T1)         # one-sided
        
        T2    <- (est + epsilon) / se
        pval2 <- 1 - pnorm(T2)     # one-sided
        
        # We say "equivalent" if both p-values < alpha
        reject_equiv <- (pval1 < alpha & pval2 < alpha)
        
        # Collect results
        rowdf <- data.frame(
          dv                 = this_dv,
          treat_var          = tv,
          estimate           = est,
          se                 = se,
          epsilon            = epsilon,
          T1                 = T1,
          pval1              = pval1,
          T2                 = T2,
          pval2              = pval2,
          reject_equivalence = reject_equiv,
          stringsAsFactors   = FALSE
        )
      }
      
      # Bind to the master results data.frame
      all_results <- rbind(all_results, rowdf)
    }
  }
  
  return(all_results)
}



## Occurrence --------------------------------------------------------------

dependent_vars <- names(results_list_occurrence)

# For Occurrence
equiv_occ <- tost_equiv_fixest(
  model_list = results_list_occurrence,
  dv_names   = dependent_vars,
  data       = df_placebo,
  treat_vars = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"),  # main treatments
  alpha      = 0.05
)

# Inspect
head(equiv_occ)

# Modify plot to show standardized effects
equiv_occ %>%
  mutate(
    treat_var = case_when(
      treat_var == "op_expo_ew" ~ "Opportunity",
      treat_var == "rg_expo_ew" ~ "Regulatory", 
      treat_var == "ph_expo_ew" ~ "Physical"
    ),
    climate_adjacent = ifelse(dv %in% climate_adjacent,
      "Climate-Adjacent", "Placebos"
    ),
    dv = factor(dv, levels = rev(levels(factor(dv)))),
    treat_var = factor(treat_var, levels = c("Opportunity", "Regulatory", "Physical"))
  ) %>%
  ggplot(aes(x = dv, y = estimate)) +
  facet_grid(climate_adjacent ~ treat_var, scales = "free", space = "free") +
  # Gray bars for equivalence range
  geom_errorbar(aes(ymin = -epsilon, ymax = epsilon), 
                width = 0, 
                color = "darkgrey",
                linewidth = 3,
                alpha = 0.5) +
  # # Dashed lines for equivalence bounds
  # geom_hline(yintercept = 0.36, linetype = "dashed", color = "black", linewidth = .25, alpha = 0.75) +
  # geom_hline(yintercept = -0.36, linetype = "dashed", color = "black", linewidth = .25, alpha = 0.75) +
  # Point estimates
  geom_point(size = 2, color = "black", shape = "diamond") +
  coord_flip() +
  labs(
    x = "Issue Code",
    y = expression(paste("Equivalence Range (in standard deviations ", sigma, ")"))
  ) +
  theme_hanno() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

ggsave("results/figures/regressions/placebo_equiv_tests_occurrence.pdf", 
       width = 8.5, height = 11)


# Similar plot for expenditure results
# For Expenditure
equiv_exp <- tost_equiv_fixest(
  model_list = results_list_expenditure,
  dv_names   = dependent_vars,
  data       = df_placebo,
  treat_vars = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"),  # main treatments
  alpha      = 0.05
)

head(equiv_exp)

equiv_exp %>%
  mutate(
    dv = str_remove(dv, "_amount"),
    treat_var = case_when(
      treat_var == "op_expo_ew" ~ "Opportunity",
      treat_var == "rg_expo_ew" ~ "Regulatory", 
      treat_var == "ph_expo_ew" ~ "Physical"
    ),
    climate_adjacent = ifelse(dv %in% climate_adjacent,
      "Climate-Adjacent", "Placebos"
    ),
    dv = factor(dv, levels = rev(levels(factor(dv)))),
    treat_var = factor(treat_var, levels = c("Opportunity", "Regulatory", "Physical"))
  ) %>%
  ggplot(aes(x = dv, y = estimate)) +
  facet_grid(climate_adjacent ~ treat_var, scales = "free", space = "free") +
  # Gray bars for equivalence range
  geom_errorbar(aes(ymin = -epsilon, ymax = epsilon), 
                width = 0, 
                color = "darkgrey",
                linewidth = 3,
                alpha = 0.5) +
  # # Dashed lines for equivalence bounds  
  # geom_hline(yintercept = 0.36, linetype = "dashed", color = "black", linewidth = .25, alpha = 0.75) +
  # geom_hline(yintercept = -0.36, linetype = "dashed", color = "black", linewidth = .25, alpha = 0.75) +
  # Point estimates
  geom_point(size = 2, color = "black", shape = "diamond") +
  coord_flip() +
  labs(
    x = "Issue Code",
    y = expression(paste("Equivalence Range (in standard deviations ", sigma, ")"))
  ) +
  theme_hanno() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

ggsave("results/figures/regressions/placebo_equiv_tests_expenditure.pdf", 
       width = 8.5, height = 11)

move_plots_to_overleaf("./")

# Issue codes table -------------------------------------------------------

codes <- read_excel("data/01_raw/lobbyview/lobbying_issue_codes.xlsx")

nrow(codes)

codes |> kbl(format = "latex", booktabs = T, longtable = T,
             caption = "Lobbying Report Issue Codes") |> 
  save_kable("data/01_raw/lobbyview/lobbying_issue_codes.tex")

# Create simple text combining issue codes and descriptions
codes |>
  mutate(
    Description = str_replace_all(Description, "&", "\\\\&"),
    Description = str_replace_all(Description, "/", " / "),
    combined_text = paste0(Code, " = ", Description)
  ) |>
  pull(combined_text) |>
  paste(collapse = ", ") |>
  cat()


### END