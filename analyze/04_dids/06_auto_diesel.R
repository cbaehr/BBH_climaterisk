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


df_auto %>%
  distinct(conm, isin) %>%
  arrange(conm)

df %>% filter(str_detect(conm, "VW")) %>% distinct(conm, isin)

df %>% filter(str_detect(conm, "VOLKSWAGEN")) %>% distinct(conm, isin)

# Define the two treatment groups
directly_involved_companies <- c(
  "VOLKSWAGEN", "AUDI", "PORSCHE",  # VW Group - directly involved
  "DAIMLER", "MERCEDES-BENZ GROUP"   # Mercedes-Benz - proven to use defeat device
)

suspected_companies <- c(
  "BMW",        # Settled antitrust suit
  "RENAULT",    # Accused/high emissions in tests
  "VOLVO",      # High emissions in tests
  "PEUGEOT",    # Accused/high emissions in tests
  "FIAT",       # High emissions in tests
  "HYUNDAI",    # Accused/high emissions in tests
  "FORD"        # Accused/high emissions in tests
)

# Update treatment assignment
df_auto <- df_auto %>%
  mutate(
    treated_diesel_direct = case_when(
      str_detect(toupper(conm), paste(directly_involved_companies, collapse = "|")) ~ 1,
      TRUE ~ 0
    ),
    treated_diesel_suspected = case_when(
      str_detect(toupper(conm), paste(suspected_companies, collapse = "|")) ~ 1,
      TRUE ~ 0
    )
  )

# Define shock parameters
shock_quarter <- "2015Q3"  # September 2015 when EPA announced VW's violation

# Convert shock_quarter to numeric t
shock_quarter_numeric <- df_auto %>%
  filter(yearqtr == shock_quarter) %>%
  pull(t) %>%
  unique()

# Modify the DiD analysis function for diesel scandal
run_diesel_scandal_did <- function(data, outcome_var, treatment_var) {
  # Prepare analysis dataset
  df_analysis <- data %>%
    filter(!is.na(get(treatment_var))) %>%
    filter(!is.na(get(outcome_var))) %>%
    mutate(
      post = as.numeric(t >= shock_quarter_numeric) - 0.5,
      time_to_treatment = t - shock_quarter_numeric
    )
    
  # Check if we have enough data
  if(nrow(df_analysis) == 0) {
    warning(paste("No valid data for outcome:", outcome_var))
    return(NULL)
  }

  # Run models with the specified treatment variable
  did_basic <- feols(
    as.formula(paste(outcome_var, "~ post:", treatment_var, "| isin + t")), 
    data = df_analysis, 
    vcov = ~t + isin
  )
  
  did_controls <- feols(
    as.formula(paste(
      outcome_var, "~ post:", treatment_var, "+", 
      paste(control_vars, collapse = " + "), 
      "| isin + t"
    )), 
    data = df_analysis, 
    vcov = ~t + isin
  )

  # Event study
  event_study <- feols(
    as.formula(paste(
      outcome_var, "~ i(time_to_treatment,", treatment_var, ", ref = -1) | isin + t"
    )), 
    data = df_analysis %>% 
      filter(t >= shock_quarter_numeric - 8 & t <= shock_quarter_numeric + 8),
    vcov = ~t + isin
  )

  # Return results
  return(list(
    att = tibble(
      outcome = outcome_var,
      treatment = treatment_var,
      model = c("basic", "controls"),
      att = c(
        coef(did_basic)[paste0("post:", treatment_var)],
        coef(did_controls)[paste0("post:", treatment_var)]
      ),
      se = c(
        se(did_basic)[paste0("post:", treatment_var)],
        se(did_controls)[paste0("post:", treatment_var)]
      )
    ),
    event = event_study %>%
      tidy_feols() %>%
      filter(str_detect(term, treatment_var)) %>%
      mutate(
        time_to_treatment = as.numeric(str_extract(term, "-?\\d+")),
        treatment = treatment_var
      )
  ))
}

# Run analysis for both treatment groups and all outcomes
outcomes <- c("CLI", "log_CLI_amount", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")
treatments <- c("treated_diesel_direct", "treated_diesel_suspected")

# Collect results
att_results <- map2_dfr(
  rep(outcomes, each = length(treatments)),
  rep(treatments, times = length(outcomes)),
  ~{
    tryCatch({
      results <- run_diesel_scandal_did(df_auto, .x, .y)
      if(is.null(results)) return(NULL)
      results$att
    }, error = function(e) {
      warning(paste("Error processing outcome:", .x, "treatment:", .y, "-", e$message))
      return(NULL)
    })
  }
)

# Add confidence intervals to ATT results
att_results <- att_results %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se
  )

# Save results
write_rds(att_results, "results/figures/did/auto_diesel_scandal_att_results.rds")

# Create ATT plot
att_plot <- ggplot(att_results, aes(x = outcome, y = att, color = treatment, shape = treatment)) +
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
  scale_color_manual(values = c("treated_diesel_direct" = "#1f77b4", "treated_diesel_suspected" = "#ff7f0e")) +
  scale_shape_manual(values = c("treated_diesel_direct" = 16, "treated_diesel_suspected" = 17)) +
  labs(title = "Average Treatment Effects of Diesel Scandal on Auto Industry",
       x = "Outcome",
       y = "Treatment Effect",
       color = "Treatment Group",
       shape = "Treatment Group") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save ATT plot
ggsave("results/figures/did/auto_diesel_scandal_att_combined.pdf", 
       att_plot, width = 10, height = 6)


