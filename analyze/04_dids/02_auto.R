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
  filter(industry == "Transport Manufacturing")  # Filter for auto industry only

glimpse(df)

# Define exposure variables for different shocks with quarters

df |>
  distinct(yearqtr, t)

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
    treatment_var = "ph_expo_ew",
    name = "Thailand Floods"
  )
)

# ... rest of the code remains the same until the save paths ...

# Update save paths for auto industry
write_rds(att_results, "results/dids/did_analysis_att_results_auto.rds")
write_rds(event_study_results, "results/dids/did_analysis_event_study_results_auto.rds")

# Update plot titles and save paths
create_event_study_plot <- function(results_subset, group_info) {
  # ... existing code ...
  labs(title = paste0("Shock: ", group_info$shock_type),
       subtitle = paste(
         "Auto Industry\n",
         "Treatment: above median", treatment_type, "exposure","\n",
         "Outcome:", group_info$outcome
       ))
  # ... rest of function remains the same ...
}

# Update final plot title
create_att_plot <- function(results_data) {
  # ... existing code ...
  labs(title = "Average Treatment Effects by Shock Type",
       subtitle = "Industry: Automobiles & Parts",
       # ... rest remains the same ...
  )
}

ggsave("results/Figures/did/auto_att_results_combined.pdf", 
       plot, width = 12, height = 6)
