### Firms & Lobbying
### Auto Industry Analysis

### Diff in Diffs

# Clear environment and load packages
pacman::p_load(tidyverse, fixest, haschaR)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

# Load the data and filter for auto industry
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds") %>%
  mutate(t = as.numeric(factor(yearqtr, levels = sort(unique(yearqtr)))))

# filter for auto industry
df_auto <- df %>%
  filter(str_starts(nace_core_4digit, "29") | industry == "Transport Manufacturing")  # Filter for auto industry only

glimpse(df_auto)


df_auto %>%
  distinct(conm, isin) %>%
  arrange(conm)


# Define exposure variables for different shocks with quarters

df_auto |>
  distinct(yearqtr, t)

# Identify firms that are affected by EU regulation: EU headquarter 
df_auto %>%
  distinct(country_iso_code)

eu_auto_companies <- df_auto %>%
  filter(country_iso_code %in% c("AT", "DE", "FR", "GB", "IE", "IT", "LU", "NL", "NO", "SE")) %>%
  distinct(conm) %>%
  pull(conm)

# are these companies in the df?
df_auto %>%
  filter(conm %in% eu_auto_companies) %>%
  distinct(conm, industry)

# any of them not in the df?
setdiff(eu_auto_companies, df_auto$conm)

# # filter for these companies in df
# df %>%
#   filter(str_detect(conm, "CONTINENTAL")) %>%
#   distinct(conm, industry, nace_core_4digit)


# Update treatment assignment
df_auto <- df_auto %>%
  mutate(
    treated_eu = case_when(
      conm %in% eu_auto_companies ~ 1,
      TRUE ~ 0
    )
  )


# DV
df_auto$CLI <- as.numeric(df_auto$CLI_quarter)

df_auto$log_CLI_amount <- log(df_auto$CLI_amount_quarter + 1)

# Define shock parameters
shock_quarter <- "43"  # Q1 2015 (EU Regulation)



# Descriptives ------------------------------------------------------------

# Plot CLI and log_CLI_amount over time for all companies; highlight thailand auto companies
df_auto %>%
  mutate(
    date = as.Date(paste0(substr(yearqtr, 1, 4), "-",
                         (as.numeric(substr(yearqtr, 6, 6)) - 1) * 3 + 1, "-01")),
    affected = ifelse(treated_eu == 1, "Affected", "Not Affected")
  ) %>%
  ggplot(aes(x = date, y = as.numeric(log_CLI_amount), color = affected)) +
    geom_line(aes(group = isin)) +
    geom_point() +
    # annotate("rect", 
    #          xmin = as.Date("2011-07-25"), 
    #          xmax = as.Date("2012-01-16"),
    #          ymin = -Inf, 
    #          ymax = Inf,
    #          alpha = 0.2,
    #          fill = "grey50") +
    facet_wrap(~conm, scales = "fixed") +
    theme_hanno() +
    labs(
      title = "Climate Lobbying Intensity of Auto Companies During EU Regulation",
      x = "Quarter",
      y = "Log Climate Lobbying Amount",
      color = "EU Regulation Impact"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_color_manual(values = c("Not Affected" = "black", "Affected" = "red"))
ggsave("results/figures/did/auto_eu_regulation_cli_intensity_time_series.pdf", width = 40, height = 20)


# Plot CLI and log_CLI_amount over time for all companies; highlight eu auto companies
df_auto %>%
  mutate(
    date = as.Date(paste0(substr(yearqtr, 1, 4), "-",
                         (as.numeric(substr(yearqtr, 6, 6)) - 1) * 3 + 1, "-01"))
  ) %>%
  ggplot(aes(x = date, y = as.numeric(CLI_quarter), color = factor(treated_eu))) +
    geom_line(aes(group = factor(isin))) +
    geom_point(aes(shape = factor(treated_eu))) +
    # annotate("rect", 
    #          xmin = as.Date("2011-07-25"), 
    #          xmax = as.Date("2012-01-16"),
    #          ymin = -Inf, 
    #          ymax = Inf,
    #          alpha = 0.2,
    #          fill = "grey50") +
    facet_wrap(~conm, scales = "fixed") +
    theme_hanno() +
    labs(
      title = "Climate Lobbying Occurrence of Auto Companies During EU Regulation",
      x = "Quarter",
      y = "Climate Lobbying Occurrence",
      color = "Affected by EU Regulation"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    scale_color_manual(values = c("0" = "black", "1" = "red"))
ggsave("results/figures/did/auto_eu_regulation_cli_occurrence_time_series.pdf", width = 40, height = 20)



# DiD Analysis ------------------------------------------------------------

# Control variables
control_vars <- c(
  "ebit", "ebit_at", "us_dummy", "total_lobby_quarter"
)

# Create pre-treatment controls: 
# last non-NA value before shock_quarter
# create new variable and add suffix _pre but keep original variable
df_auto <- df_auto %>%
  group_by(isin) %>%
  mutate(across(all_of(control_vars), ~if_else(is.na(.), last(na.omit(.)), .))) %>%
  mutate(across(all_of(control_vars), ~., .names = "{col}_pre")) # Create new variables with _pre suffix while keeping originals

glimpse(df_auto)

# Function to run DiD analysis for Thai floods in auto industry
run_thai_floods_did <- function(data, outcome_var) {
  shock_quarter <- as.numeric(shock_quarter)
  
  # Prepare analysis dataset
  df_analysis <- data %>%
    filter(!is.na(treated_thailand)) %>%
    mutate(
      post = as.numeric(t >= shock_quarter) - 0.5,  # Center the post variable
      time_to_treatment = t - shock_quarter
    )
    # ) %>%
    # # Handle missing control variables
    # mutate(across(all_of(control_vars), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))

  # Run models
  did_basic <- feols(as.formula(paste(outcome_var, "~ post:treated_thailand | isin + t")), 
                    data = df_analysis, 
                    vcov = ~t + isin)
  
  did_controls <- feols(as.formula(paste(outcome_var, "~ post:treated_thailand + ", 
                                     paste(control_vars, collapse = " + "), 
                                     "| isin + t")), 
                     data = df_analysis, 
                     vcov = ~t + isin)
  
#   # Event study (limited to 12 quarters before/after)
#   df_analysis <- df_analysis %>%
#     filter(abs(time_to_treatment) <= 12)

  event_study_basic <- feols(as.formula(paste(outcome_var, "~ i(time_to_treatment, treated_thailand, ref = -1) | isin + t")), 
                            data = df_analysis %>% 
                              filter(t >= as.numeric(shock_quarter) - 5 & t <= as.numeric(shock_quarter) + 10),
                            vcov = ~ t + isin)

  event_study_controls <- feols(as.formula(paste(outcome_var, "~ i(time_to_treatment, treated_thailand, ref = -1) +",
                                     paste(control_vars, collapse = " + "),
                                     "| isin + t")), 
                            data = df_analysis %>% 
                              filter(t >= as.numeric(shock_quarter) - 5 & t <= as.numeric(shock_quarter) + 10),
                            vcov = ~ t + isin)  
  
  event_study_pre <- feols(as.formula(paste(outcome_var, "~ i(time_to_treatment, treated_thailand, ref = -1) +",
                                     "i(time_to_treatment, ebit_pre, ref = -1) + ",
                                     "i(time_to_treatment, ebit_at_pre, ref = -1) + ",
                                     "i(time_to_treatment, total_lobby_quarter_pre, ref = -1)",
                                     "| isin + t")), 
                      data = df_analysis %>% 
                        filter(t >= as.numeric(shock_quarter) - 5 & t <= as.numeric(shock_quarter) + 10),
                      vcov = ~ t + isin)
  
  # Extract results
  event_study_results <- event_study_basic  %>%
    haschaR::tidy_feols() %>%
    filter(str_detect(term, "treated_thailand")) %>%
    mutate(time_to_treatment = as.numeric(str_extract(term, "-?\\d+")),
           model = "basic"
    ) %>%
    bind_rows(event_study_controls %>%
                haschaR::tidy_feols() %>%
                filter(str_detect(term, "treated_thailand")) %>%
                mutate(time_to_treatment = as.numeric(str_extract(term, "-?\\d+")),
                       model = "controls")) %>%
    bind_rows(event_study_pre %>%
                haschaR::tidy_feols() %>%
                filter(str_detect(term, "treated_thailand")) %>%
                mutate(time_to_treatment = as.numeric(str_extract(term, "-?\\d+")),
                       model = "pre"))
  
  # Create ATT results
  att_results <- bind_rows(
    tibble(
      outcome = outcome_var,
      model = "basic",
      att = coef(did_basic)["post:treated_thailand"],
      se = se(did_basic)["post:treated_thailand"]
    ),
    tibble(
      outcome = outcome_var,
      model = "controls",
      att = coef(did_controls)["post:treated_thailand"],
      se = se(did_controls)["post:treated_thailand"]
    )
  )
  
  return(list(
    att = att_results,
    event = event_study_results
  ))
}

# Run analysis for both outcomes
outcomes <- c("CLI", "log_CLI_amount", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")

# Collect results
att_results <- map_dfr(outcomes, function(outcome) {
  results <- run_thai_floods_did(df_auto, outcome)
  results$att
})

event_study_results <- map_dfr(outcomes, function(outcome) {
  results <- run_thai_floods_did(df_auto, outcome)
  results$event %>%
    mutate(outcome = outcome)
})

# Add confidence intervals to ATT results
att_results <- att_results %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se
  )

# Save results
write_rds(att_results, "results/figures/did/auto_thai_floods_att_results.rds")
write_rds(event_study_results, "results/figures/did/auto_thai_floods_event_study_results.rds")

# Create event study plot
create_event_study_plot <- function(results_subset, outcome_var) {
  ref_point <- data.frame(
    time_to_treatment = -1,
    estimate = 0,
    std.error = 0,
    model = c("basic", "controls", "pre")
  )
  
  results_subset <- bind_rows(results_subset, ref_point)
  
  outcome_label <- case_when(
    outcome_var == "CLI" ~ "Climate Lobbying Indicator",
    outcome_var == "log_CLI_amount" ~ "Log Climate Lobbying Amount",
    outcome_var == "op_expo_ew" ~ "Opportunity Exposure",
    outcome_var == "rg_expo_ew" ~ "Regulatory Exposure",
    outcome_var == "ph_expo_ew" ~ "Physical Exposure"
  )
  
  ggplot(results_subset, aes(x = time_to_treatment, y = estimate)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                     ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_vline(xintercept = -1, linetype = "dashed") +
    geom_point(data = ref_point, size = 2) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_hanno() +
    facet_wrap(~model, ncol = 3) +
    labs(title = "Thai Floods Impact on Auto Industry",
         subtitle = paste("Outcome:", outcome_label),
         x = "Quarters Relative to Treatment",
         y = "Treatment Effect") +
    scale_x_continuous(breaks = seq(-12, 12, 3))
}

# Generate event study plots
event_study_results %>%
  group_by(outcome) %>%
  group_walk(~{
    plot <- create_event_study_plot(.x, .y$outcome)
    filename <- paste0("results/figures/did/auto_thai_floods_", 
                      tolower(.y$outcome), "_event_study.pdf")
    ggsave(filename, plot, width = 10, height = 6)
  })

# Create ATT plot
att_plot <- ggplot(att_results, aes(x = outcome, y = att, color = model, shape = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
               position = position_dodge(width = 0.5),
               width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_hanno() +
  scale_x_discrete(labels = c(
    "CLI" = "Climate Lobbying Indicator",
    "log_CLI_amount" = "Log Climate Lobbying Amount",
    "op_expo_ew" = "Opportunity Exposure",
    "rg_expo_ew" = "Regulatory Exposure",
    "ph_expo_ew" = "Physical Exposure"
  )) +
  scale_color_manual(values = c("basic" = "#1f77b4", "controls" = "#ff7f0e")) +
  scale_shape_manual(values = c("basic" = 16, "controls" = 17)) +
  labs(title = "Average Treatment Effects of Thai Floods on Auto Industry",
       x = "Outcome",
       y = "Treatment Effect",
       color = "Model Specification",
       shape = "Model Specification") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save ATT plot
ggsave("results/figures/did/auto_thai_floods_att_combined.pdf", 
       att_plot, width = 10, height = 6)


