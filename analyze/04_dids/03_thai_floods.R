# Clear environment and load packages
pacman::p_load(tidyverse, fixest, haschaR)

# set working directory (using same setup as auto analysis)
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

# Load the data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds") %>%
  mutate(t = as.numeric(factor(yearqtr, levels = sort(unique(yearqtr)))))


# Companies affected by Thai floods (organized by sector)
thailand_affected_companies <- list(
  # Electronics and Technology
  computers_electronics = c(
    "WESTERN DIGITAL CORP", "SEAGATE TECHNOLOGY HOLDINGS PLC", # seagate not in df
    "TOSHIBA CORPORATION", "SONY GROUP CORPORATION", "CANON INCORPORATED",
    "NIKON CORPORATION", "PANASONIC HOLDINGS CORPORATION", "SAMSUNG ELECTRONICS CO.,LTD.",
    "INTEL CORP", "DELL TECHNOLOGIES INC.", "HEWLETT PACKARD ENTERPRISE COMPANY", # dell not in df
    "LENOVO GROUP LIMITED", "SHARP CORPORATION", "FUJITSU LIMITED", # sharp not in df
    "NEC CORPORATION", "LG ELECTRONICS INC.", "ACER INCORPORATED",
    "ASUSTEK COMPUTER INC.", "APPLE INC.", "JVC KENWOOD CORPORATION", # JVC not in df
    "SANYO ELECTRIC CO., LTD.", "CAL-COMP ELECTRONICS" # Sanyo and cal-comp not in df
  ),
  
  # Automotive
  automobiles = c(
    "HONDA MOTOR CO., LTD.", "TOYOTA MOTOR CORPORATION", 
    "NISSAN MOTOR CO., LTD.", "FORD MOTOR CO",
    "MAZDA MOTOR CORPORATION", "MITSUBISHI MOTORS CORPORATION",
    "GENERAL MOTORS COMPANY", "ISUZU MOTORS LIMITED",
    "SUZUKI MOTOR CORPORATION", "BRIDGESTONE CORPORATION",
    "CONTINENTAL AG", "DENSO CORPORATION"
  ),
  
  # Machinery and Equipment
  machinery = c(
    "HITACHI LTD", "SIEMENS AG", "BOSCH LIMITED"
  ),
  
  # Consumer Goods
  consumer_goods = c(
    "UNILEVER PLC", "NESTLE S.A.", "PROCTER & GAMBLE CO" # unilever not in df
  )
)

df %>%
  filter(str_detect(conm, "BOSCH")) %>%
  distinct(conm, industry) %>%
  arrange(conm)


# Flatten the list for treatment assignment
all_affected_companies <- unlist(thailand_affected_companies)

# Create treatment variable and sector indicators
df <- df %>%
  mutate(
    treated_thailand = case_when(
      conm %in% all_affected_companies ~ 1,
      TRUE ~ 0
    ),
    # Create sector-specific treatment indicators
    treated_electronics = case_when(
      conm %in% thailand_affected_companies$computers_electronics ~ 1,
      TRUE ~ 0
    ),
    treated_auto = case_when(
      conm %in% thailand_affected_companies$automobiles ~ 1,
      TRUE ~ 0
    ),
    treated_machinery = case_when(
      conm %in% thailand_affected_companies$machinery ~ 1,
      TRUE ~ 0
    ),
    treated_consumer = case_when(
      conm %in% thailand_affected_companies$consumer_goods ~ 1,
      TRUE ~ 0
    )
  )

table(df$treated_thailand)

# Define shock parameters
shock_quarter <- "43"  # Q3 2011 (Thailand Floods)

# table t and yearqtr
df %>%
  distinct(t, yearqtr) %>%
  arrange(t)


# Create a more detailed physical exposure plot for affected companies
df %>%
  filter(conm %in% all_affected_companies) %>%
  mutate(
    date = as.Date(paste0(substr(yearqtr, 1, 4), "-",
                         (as.numeric(substr(yearqtr, 6, 6)) - 1) * 3 + 1, "-01")),
    industry = case_when(
      conm %in% thailand_affected_companies$computers_electronics ~ "Electronics",
      conm %in% thailand_affected_companies$automobiles ~ "Automotive",
      conm %in% thailand_affected_companies$machinery ~ "Machinery",
      conm %in% thailand_affected_companies$consumer_goods ~ "Consumer Goods",
      TRUE ~ "Other"
    )
  ) %>%
  ggplot(aes(x = date, y = ph_expo_ew, color = industry)) +
  geom_line() +
  geom_point(size = 1) +
  annotate("rect", 
           xmin = as.Date("2011-07-25"), 
           xmax = as.Date("2012-01-16"),
           ymin = -Inf, 
           ymax = Inf,
           alpha = 0.2,
           fill = "grey50") +
  facet_wrap(~conm, scales = "fixed") +
  theme_hanno() +
  labs(
    title = "Physical Climate Risk Exposure of Thai Flood-Affected Companies",
    x = "Quarter",
    y = "Physical Exposure",
    color = "Industry"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")

# Save the plot
ggsave(
  "results/Figures/exposures/thai_floods_physical_exposure_time_series.pdf",
  width = 40,
  height = 20
)


# Diff-in-diff analysis -----------------------------------------------------

# Control variables (same as auto analysis)
control_vars <- c(
  "ebit", "ebit_at", "us_dummy", "total_lobby_quarter"
)

# Dependent variables
df$CLI <- as.numeric(df$CLI_quarter)
df$log_CLI_amount <- log(df$CLI_amount_quarter + 1)

# Function to run DiD analysis for Thai floods
run_thai_floods_did <- function(data, treatment_var, outcome_var) {
  shock_quarter <- as.numeric(shock_quarter)  # Using the already defined shock_quarter
  
  # Prepare analysis dataset
  df_analysis <- data %>%
    filter(!is.na(!!sym(treatment_var))) %>%
    mutate(
      post = as.numeric(t >= shock_quarter) - 0.5,  # Center the post variable
      time_to_treatment = t - shock_quarter,
      industry_time = paste0(industry, "_", t) # Create industry-time interaction
    ) %>%
    # Handle missing control variables
    mutate(across(all_of(control_vars), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Run models with industry x time FE
  did_basic <- feols(as.formula(paste(outcome_var, "~ post:", treatment_var, "| isin + industry_time")), 
                    data = df_analysis, 
                    vcov = ~t + isin)
  
  did_controls <- feols(as.formula(paste(outcome_var, "~ post:", treatment_var, 
                                       "+ ebit + ebit_at + us_dummy + total_lobby_quarter | isin + industry_time")), 
                       data = df_analysis, 
                       vcov = ~t + isin)
  
  # Event study (limited to 12 quarters before/after)
  df_analysis <- df_analysis %>%
    filter(abs(time_to_treatment) <= 12)
  
  event_study <- feols(as.formula(paste(outcome_var, "~ i(time_to_treatment,", treatment_var, ", ref = -1) | isin + industry_time")), 
                      data = df_analysis, 
                      vcov = ~t + isin)
  
  # Extract results
  event_study_results <- event_study %>%
    haschaR::tidy_feols() %>%
    filter(str_detect(term, treatment_var)) %>%
    mutate(time_to_treatment = as.numeric(str_extract(term, "-?\\d+")))
  
  # Create ATT results
  att_results <- bind_rows(
    tibble(
      treatment = treatment_var,
      outcome = outcome_var,
      model = "basic",
      att = coef(did_basic)[paste0("post:", treatment_var)],
      se = se(did_basic)[paste0("post:", treatment_var)]
    ),
    tibble(
      treatment = treatment_var,
      outcome = outcome_var,
      model = "controls",
      att = coef(did_controls)[paste0("post:", treatment_var)],
      se = se(did_controls)[paste0("post:", treatment_var)]
    )
  )
  
  return(list(
    att = att_results,
    event = event_study_results
  ))
}

# Run analysis for all treatment variables and outcomes
treatment_vars <- c("treated_thailand", "treated_electronics", "treated_auto", 
                   "treated_machinery", "treated_consumer")
outcomes <- c("CLI", "log_CLI_amount")

# Collect results
att_results <- crossing(
  treatment = treatment_vars,
  outcome = outcomes
) %>%
  pmap_dfr(function(treatment, outcome) {
    results <- run_thai_floods_did(df, treatment, outcome)
    results$att
  })

event_study_results <- crossing(
  treatment = treatment_vars,
  outcome = outcomes
) %>%
  pmap_dfr(function(treatment, outcome) {
    results <- run_thai_floods_did(df, treatment, outcome)
    results$event %>%
      mutate(
        treatment = treatment,
        outcome = outcome
      )
  })

# Add confidence intervals to ATT results
att_results <- att_results %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se
  )

# Save results
write_rds(att_results, "results/figures/did/thai_floods_att_results.rds")
write_rds(event_study_results, "results/figures/did/thai_floods_event_study_results.rds")

# Create and save plots
create_event_study_plot <- function(results_subset, group_info) {
  ref_point <- data.frame(
    time_to_treatment = -1,
    estimate = 0,
    std.error = 0
  )
  
  results_subset <- bind_rows(results_subset, ref_point)
  
  treatment_label <- case_when(
    group_info$treatment == "treated_thailand" ~ "All Affected Companies",
    group_info$treatment == "treated_electronics" ~ "Electronics Sector",
    group_info$treatment == "treated_auto" ~ "Automotive Sector",
    group_info$treatment == "treated_machinery" ~ "Machinery Sector",
    group_info$treatment == "treated_consumer" ~ "Consumer Goods Sector"
  )
  
  ggplot(results_subset, aes(x = time_to_treatment, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                     ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_vline(xintercept = -1, linetype = "dashed") +
    geom_point(data = ref_point) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_hanno() +
    labs(title = "Thai Floods Impact",
         subtitle = paste0(
           "Treatment Group: ", treatment_label, "\n",
           "Outcome: ", group_info$outcome
         ),
         x = "Quarters Relative to Treatment",
         y = "Treatment Effect") +
    scale_x_continuous(breaks = seq(-12, 12, 3))
}

# Generate event study plots
event_study_results %>%
  filter(!is.na(time_to_treatment)) %>%
  group_by(treatment, outcome) %>%
  group_walk(~{
    plot <- create_event_study_plot(.x, .y)
    filename <- paste0("results/Figures/did/thai_floods_", 
                      tolower(str_replace_all(.y$treatment, "treated_", "")), "_",
                      tolower(.y$outcome), "_event_study.pdf")
    ggsave(filename, plot, width = 10, height = 6)
  })

# Create coefficient plot for ATT results
create_att_plot <- function(results_data) {
  ggplot(results_data, aes(x = treatment, y = att, color = model, shape = model)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                 position = position_dodge(width = 0.5),
                 width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    facet_wrap(~outcome, scales = "free_y", 
               labeller = labeller(outcome = c(
                 "CLI" = "Climate Lobbying Indicator",
                 "log_CLI_amount" = "Log Climate Lobbying Amount"
               ))) +
    theme_hanno() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 11),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("basic" = "#1f77b4", "controls" = "#ff7f0e")) +
    scale_shape_manual(values = c("basic" = 16, "controls" = 17)) +
    scale_x_discrete(labels = c(
      "treated_thailand" = "All Companies",
      "treated_electronics" = "Electronics",
      "treated_auto" = "Automotive",
      "treated_machinery" = "Machinery",
      "treated_consumer" = "Consumer Goods"
    )) +
    labs(title = "Average Treatment Effects of Thai Floods",
         subtitle = "By Sector and Outcome",
         x = "Treatment Group",
         y = "Treatment Effect",
         color = "Model Specification",
         shape = "Model Specification")
}

# Generate and save ATT plot
plot <- create_att_plot(att_results)
ggsave("results/Figures/did/thai_floods_att_results_combined.pdf", 
       plot, width = 12, height = 6)


