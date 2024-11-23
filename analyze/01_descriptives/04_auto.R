### Firms & Lobbying
### Auto Industry Analysis

# Clear environment and load packages
pacman::p_load(tidyverse, fixest, haschaR)


# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

# Load both datasets
df_q <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds") %>%
  mutate(t = as.numeric(factor(yearqtr, levels = sort(unique(yearqtr)))))

df_y <- read_rds("data/03_final/lobbying_df_annual_REVISE_normal.rds") %>%
  mutate(t = as.numeric(factor(year)))

# Filter for auto industry in both datasets using NACE codes
df_auto_q <- df_q %>%
  filter(str_starts(nace_core_4digit, "29") | industry == "Transport Manufacturing")  # Filter for auto industry only

df_auto_y <- df_y %>%
  filter(str_starts(nace_core_4digit, "29") | industry == "Transport Manufacturing")  # Filter for auto industry only



# Get a table of the top 20 firm-quarters by each exposure variable
df_auto_q %>%
  select(conm, yearqtr, rg_expo_ew) %>%
  arrange(desc(rg_expo_ew)) %>%
  head(30)


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
      title = paste("Auto Industry:", exposure_vars[var], "Over Time -", time_period),
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
    paste0("results/Figures/exposures/auto_", var, "_", tolower(time_period), "_time_series.pdf"),
    plot,
    width = 40,
    height = 20
  )
}

# Create quarterly plots
for (var in names(exposure_vars)) {
  df_auto_q <- df_auto_q %>%
    mutate(date = as.Date(paste0(substr(yearqtr, 1, 4), "-",
                                as.numeric(substr(yearqtr, 6, 6)) * 3, "-01")))
  create_exposure_plot(df_auto_q, var, "Quarterly", "date")
}

# Create yearly plots
for (var in names(exposure_vars)) {
  create_exposure_plot(df_auto_y, var, "Yearly", "year")
}

