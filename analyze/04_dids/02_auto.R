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
  mutate(t = as.numeric(factor(yearqtr, levels = sort(unique(yearqtr))))) %>%
  filter(str_starts(nace_core_4digit, "29"))  # Filter for auto industry only

glimpse(df)


df %>%
  distinct(conm, isin) %>%
  arrange(conm)


# Define exposure variables for different shocks with quarters

df |>
  distinct(yearqtr, t)

# Identify firms that were affected by Thailand Floods - Updated list for auto industry
thailand_auto_companies <- c(
    "HONDA MOTOR CO., LTD.", "TOYOTA MOTOR CORPORATION", 
    "NISSAN MOTOR CO., LTD.", "FORD MOTOR CO",
    "MAZDA MOTOR CORPORATION", "MITSUBISHI MOTORS CORPORATION",
    "GENERAL MOTORS COMPANY", "ISUZU MOTORS LIMITED",
    "SUZUKI MOTOR CORPORATION", "BRIDGESTONE CORPORATION",
    "CONTINENTAL AG", "DENSO CORPORATION", "BOSCH LIMITED"
)

# Update treatment assignment
df <- df %>%
  mutate(
    treated_thailand = case_when(
      conm %in% thailand_auto_companies ~ 1,
      TRUE ~ 0
    )
  )

# Add Thailand floods to shocks list
shocks <- list(
    opportunity = list(
        shock_quarter = "32",  # EV Tax Credits: 2008-2009 (Q4 2008)
        treatment_var = "op_expo_ew",
        name = "EV Tax Credits"
    ),
    regulatory = list(
        shock_quarter = "43",  # CAFE Standards: July 2011
        treatment_var = "rg_expo_ew",
        name = "CAFE Standards"
    ),
    physical = list(
        shock_quarter = "43",  # Thailand Floods: Q4 2011
        treatment_var = "treated_thailand",
        name = "Thailand Floods"
    )
)

# Control variables
control_vars <- c(
  "ebit", "ebit_at", "us_dummy", "total_lobby_quarter"
)

# DV
df$CLI <- as.numeric(df$CLI_quarter)

df$log_CLI_amount <- log(df$CLI_amount_quarter + 1)

# Function to run DiD analysis for a specific combination
run_did_analysis <- function(data, shock_info, industry_filter, outcome_var) {
  shock_quarter <- as.numeric(shock_info$shock_quarter)
  treatment_var <- shock_info$treatment_var
  
  # Create treatment groups based on pre-shock quarter
  pre_shock_quarter <- data %>%
    filter(t < shock_quarter) %>%
    filter(!is.na(!!sym(treatment_var))) %>%
    group_by(isin) %>%
    summarize(
      avg_exposure = mean(!!sym(treatment_var), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      treated = as.numeric(avg_exposure > median(avg_exposure, na.rm = TRUE)),
      treated = treated - 0.5  # Center the treatment variable to avoid collinearity
    )
  
  # Prepare analysis dataset
  df_analysis <- data %>%
    filter(!is.na(!!sym(treatment_var))) %>%
    left_join(pre_shock_quarter[, c("isin", "treated")], by = "isin") %>%
    filter(!is.na(treated)) %>%
    mutate(
      post = as.numeric(t >= shock_quarter) - 0.5,  # Center the post variable
      time_to_treatment = t - shock_quarter
    ) %>%
    # Handle missing control variables
    mutate(across(all_of(control_vars), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Add error handling
  if (nrow(df_analysis) == 0) {
    warning("No observations after filtering for ", shock_info$name)
    return(NULL)
  }
  
  # Run models with try-catch
  tryCatch({
    did_basic <- feols(as.formula(paste(outcome_var, "~ post:treated | isin + t")), 
                      data = df_analysis, 
                      vcov = ~t + isin)
    
    did_controls <- feols(as.formula(paste(outcome_var, "~ post:treated + ebit + ebit_at + us_dummy + total_lobby_quarter | isin + t")), 
                         data = df_analysis, 
                         vcov = ~t + isin)
    
    # Limit event study to reasonable time window
    df_analysis <- df_analysis %>%
      filter(abs(time_to_treatment) <= 12)  # Limit to 3 years before/after
    
    event_study <- feols(as.formula(paste(outcome_var, "~ i(time_to_treatment, treated, ref = -1) | isin + t")), 
                        data = df_analysis, 
                        vcov = ~t + isin)
    
    # Extract results
    event_study_results <- event_study %>%
      haschaR::tidy_feols() %>%
      filter(str_detect(term, "treated")) %>%
      mutate(time_to_treatment = as.numeric(str_extract(term, "-?\\d+")))
    
    # Create ATT results in long format
    att_results <- bind_rows(
      tibble(
        shock_type = shock_info$name,
        industry = industry_filter,
        outcome = outcome_var,
        model = "basic",
        att = coef(did_basic)["post:treated"],
        se = se(did_basic)["post:treated"]
      ),
      tibble(
        shock_type = shock_info$name,
        industry = industry_filter,
        outcome = outcome_var,
        model = "controls",
        att = coef(did_controls)["post:treated"],
        se = se(did_controls)["post:treated"]
      )
    )
    
    # Event study results remain the same
    event_study_results <- event_study_results %>%
      select(estimate, std.error, time_to_treatment) %>%
      mutate(
        shock_type = shock_info$name,
        industry = industry_filter,
        outcome = outcome_var
      )
    
    return(list(
      att = att_results,
      event = event_study_results
    ))
  }, error = function(e) {
    warning("Error in analysis for ", shock_info$name, ": ", e$message)
    return(NULL)
  })
}

# Run analysis for all combinations
industries <- unique(df$nace_core_4digit)
outcomes <- c("CLI", "log_CLI_amount")

# Split into separate ATT and event study results
att_results <- crossing(
  shock = names(shocks),
  industry = industries,
  outcome = outcomes
) %>%
  pmap_dfr(function(shock, industry, outcome) {
    results <- run_did_analysis(df, shocks[[shock]], industry, outcome)
    if (is.null(results)) {
      return(NULL)
    }
    results$att
  })

event_study_results <- crossing(
  shock = names(shocks),
  industry = industries,
  outcome = outcomes
) %>%
  pmap_dfr(function(shock, industry, outcome) {
    results <- run_did_analysis(df, shocks[[shock]], industry, outcome)
    if (is.null(results)) {
      return(NULL)
    }
    results$event
  })

# Remove any NULL results before saving
att_results <- att_results %>% filter(!is.na(shock_type))
event_study_results <- event_study_results %>% filter(!is.na(shock_type))


# ... rest of the code remains the same until the save paths ...

# Update save paths for auto industry
write_rds(att_results, "results/figures/did/did_analysis_att_results_auto.rds")
write_rds(event_study_results, "results/figures/did/did_analysis_event_study_results_auto.rds")


# Function to create plots from the results
create_event_study_plot <- function(results_subset, group_info) {
  # Add reference period point
  ref_point <- data.frame(
    time_to_treatment = -1,
    estimate = 0,
    std.error = 0
  )
  
  # Determine treatment type
  treatment_type <- case_when(
    str_detect(group_info$shock_type, "EV Tax Credits") ~ "opportunity",
    str_detect(group_info$shock_type, "CAFE Standards") ~ "regulatory",
    str_detect(group_info$shock_type, "Thailand Floods") ~ "physical",
    TRUE ~ "unknown"
  )
  
  results_subset <- bind_rows(results_subset, ref_point)
  
  ggplot(results_subset, aes(x = time_to_treatment, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * std.error, 
                     ymax = estimate + 1.96 * std.error), width = 0.2) +
    geom_vline(xintercept = -1, linetype = "dashed") +
    geom_point(data = ref_point) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_hanno() +
    labs(title = paste0("Shock: ", group_info$shock_type),
         subtitle = paste(
                       "Treatment: above median", treatment_type, "exposure","\n",
                       "Outcome:", group_info$outcome
                       ),
         x = "Quarters Relative to Treatment",
         y = "Treatment Effect") +
    scale_x_continuous(breaks = seq(-30, 30, 5))
}

# Modified group_walk call
event_study_results %>%
  filter(!is.na(time_to_treatment)) %>%
  group_by(shock_type, industry, outcome) %>%
  group_walk(~{
    plot <- create_event_study_plot(.x, .y)
    filename <- paste0("results/Figures/did/", 
                      tolower(str_replace_all(.y$industry, " ", "_")), "_",
                      tolower(str_replace_all(.y$shock_type, " ", "_")), "_",
                      tolower(.y$outcome), "_event_study.pdf")
    ggsave(filename, plot, width = 10, height = 6)
  })

# Optional: Add confidence intervals to ATT results before saving
att_results <- att_results %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se
  )

write_rds(att_results, "results/figures/did/did_analysis_att_results_auto.rds")
# Function to create coefficient plots for ATT results with facets
create_att_plot <- function(results_data) {
  ggplot(results_data, aes(x = shock_type, y = att, color = model, shape = model)) +
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
    labs(title = "Average Treatment Effects by Shock Type",
         subtitle = "Industry: Transport Manufacturing",
         x = "Shock Type",
         y = "Treatment Effect",
         color = "Model Specification",
         shape = "Model Specification")
}

# Generate and save single plot with facets
plot <- create_att_plot(att_results)
ggsave("results/figures/did/auto_att_results_combined.pdf", 
       plot, width = 12, height = 6)
