### Firms & Lobbying
### Utilities Industry Analysis

# Clear environment and load packages
pacman::p_load(tidyverse, fixest, haschaR)

# Load both datasets
df_q <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds") %>%
  mutate(t = as.numeric(factor(yearqtr, levels = sort(unique(yearqtr)))))

df_y <- read_rds("data/03_final/lobbying_df_annual_REVISE_normal.rds") %>%
  mutate(t = as.numeric(factor(year)))

# Filter for utilities in both datasets
df_utilities_q <- df_q %>%
  filter(industry == "Utilities")

df_utilities_y <- df_y %>%
  filter(industry == "Utilities")

# Create exposure plots
exposure_vars <- c(
  "op_expo_ew" = "Opportunity Exposure",
  "rg_expo_ew" = "Regulatory Exposure",
  "ph_expo_ew" = "Physical Exposure"
)

# Function to create and save plots
create_exposure_plot <- function(data, var, time_period, date_col) {
  # Filter firms that have at least one non-zero value for this exposure
  df_filtered <- data %>%
    group_by(isin) %>%
    filter(any(!!sym(var) >= 0)) %>%
    ungroup()
  
  plot <- df_filtered %>%
    ggplot(aes(x = !!sym(date_col), y = !!sym(var), group = conm)) +
    geom_line() +
    geom_point(size = 1) +
    facet_wrap(~conm, scales = "fixed") +
    theme_hanno() +
    labs(
      title = paste("Utilities Industry:", exposure_vars[var], "Over Time -", time_period),
      x = ifelse(time_period == "Quarterly", "Quarter", "Year"), 
      y = exposure_vars[var]
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  if (time_period == "Quarterly") {
    plot <- plot + scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  }
  
  # Save the plot
  ggsave(
    paste0("results/Figures/exposures/utilities_", var, "_", tolower(time_period), "_time_series.pdf"),
    plot,
    width = 40,
    height = 20
  )
}

# Create quarterly plots
for (var in names(exposure_vars)) {
  df_utilities_q <- df_utilities_q %>%
    mutate(date = as.Date(paste0(substr(yearqtr, 1, 4), "-",
                                as.numeric(substr(yearqtr, 6, 6)) * 3, "-01")))
  create_exposure_plot(df_utilities_q, var, "Quarterly", "date")
}

# Create yearly plots
for (var in names(exposure_vars)) {
  create_exposure_plot(df_utilities_y, var, "Yearly", "year")
}



