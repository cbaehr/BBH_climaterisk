### Firms & Climate Lobbying
### Search for Placebo


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haschaR, broom)

# prevent scientific notation
options(scipen = 999)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/Dropbox (Princeton)/BBH/BBH1/")}


# ###Placebo exploration 
# #load dataset
# df <- fread("data/00_old/data/03_final/lobbying_df_wide_reduced_normal.csv")
# 
# # read in lobbying issue codes
# codes <- read_excel("data/01_raw/lobbyview/lobbying_issue_codes.xlsx")
# 
# # merge df with codes
# df <- df %>% 
#   left_join(codes, by = c("issue_code" = "Code")) %>% 
#   rename(issue_code_txt = Description)
# 
# 
# # Plot number of reports by issue code txt
# df %>% 
#   group_by(issue_code_txt) %>% 
#   summarise(n = n()) %>% 
#   arrange(desc(n)) %>% 
#   filter(n > 100) %>% 
#   # Highlight Energy/Nuclear, Environmental/Superfund, Clean Air & Water (Quality), Fuel/Gas/Oil
#   mutate(climate = ifelse(issue_code_txt %in% c("Energy/Nuclear", "Environmental/Superfund", "Clean Air & Water (Quality)", "Fuel/Gas/Oil"), 1, 0)) %>%
#   # Highlight climate == 1 in red
#   ggplot(aes(x = reorder(issue_code_txt, n), y = n, fill = factor(climate))) +
#   geom_col() +
#   coord_flip() +
#   labs(x = "Issue Code", y = "Number of Reports", title = "Number of Reports by Issue Code (1999-2020)") +
#   theme_hanno()+ 
#   scale_fill_manual(values = c("grey", "red")) +
#   theme(legend.position = "none")
# 
# # Save plot
# ggsave("results/Figures/descriptives/number_of_reports_by_issue_code.pdf", width = 9, height = 13)
# 
# 
# # Plot mean lobbying expenditure by issue code txt (in billions)
# df %>% 
#   group_by(issue_code_txt) %>% 
#   summarise(total_expenditure = sum(amount_num)) %>% 
#   # divide by 1000000 to get in millions
#   mutate(total_expenditure = total_expenditure / 1000000) %>%
#   arrange(desc(total_expenditure)) %>% 
#   filter(total_expenditure > 1000) %>% 
#   # Highlight Energy/Nuclear, Environmental/Superfund, Clean Air & Water (Quality), Fuel/Gas/Oil
#   mutate(climate = ifelse(issue_code_txt %in% c("Energy/Nuclear", "Environmental/Superfund", "Clean Air & Water (Quality)", "Fuel/Gas/Oil"), 1, 0)) %>%
#   # Highlight climate == 1 in red
#   ggplot(aes(x = reorder(issue_code_txt, total_expenditure), y = total_expenditure, fill = factor(climate))) +
#   geom_col() +
#   coord_flip() +
#   # label x axis correctly
#   labs(x = "Issue Code", y = "Total Expenditure (billions)", title = "Total Expenditure by Issue Code (1999-2020)") +
#   theme_hanno()+ 
#   scale_fill_manual(values = c("grey", "red")) +
#   theme(legend.position = "none")
# 
# 
# # Save plot
# ggsave("results/Figures/descriptives/expenditure_by_issue_code.pdf", width = 9, height = 13)




#Load dataset
df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")

#Create new variable that is ebit/assets and normalize
df <- df |> mutate(
  ebit_at = scale(ebit / at)
)

# Specify covariate names
cm <- c("op_expo_ew_y" = "Opportunity Exposure",
        "rg_expo_ew_y" = "Regulatory Exposure",
        "ph_expo_ew_y" = "Physical Exposure", 
        "cc_expo_ew_y" = "Overall Exposure", 
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying ($)"
)

#Run model for first issue code (agriculture) - this works
# ag <- feglm(AGR ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_at + us_dummy + total_lobby| year + industry + industry_year, family = "binomial", df)


# Occurrence --------------------------------------------------------------



##Identify placebo variables
# Specify columns to remove
columns_to_remove <- c("CAW", "ENV", "ENG", "FUE", "REL") # climate issues + REL constant zero (no lobbying)

# Remove specified columns
df_placebo <- df |>
  select(-all_of(columns_to_remove)) |>
  # all to numeric
  mutate_at(vars(AGR:FAM), as.numeric)

start_index <- which(names(df_placebo) == "AGR")
end_index <- which(names(df_placebo) == "FAM")

# Extract column names between "AGR" and "REL" into a character vector
dependent_vars <- names(df_placebo)[start_index:end_index]

# View the character vector of column names
print(dependent_vars)

# Initialize an empty list to store model results
results_list <- list()

# Iterate over the dependent variables and fit the feglm model with fixed effects for each

for (dv in dependent_vars) { {
  formula <- as.formula(paste(dv, "~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_at + us_dummy + total_lobby| year + industry + industry_year")) # Include fixed effects in the formula
  
  # Debugging
  print(formula)
  
  # Fit the feglm model
  model <- feglm(formula, family = "binomial", data = df_placebo)
  
  # Store the model results in the list
  results_list[[paste(dv, sep = "_")]] <- summary(model)
}
}

# Create an empty dataframe to store tidied results
tidied_results <- data.frame()

# Iterate over the results_list to tidy each model's results
for (dv in names(results_list)) {
  # Extract 95% confidence intervals
  tidied_model_95 <- tidy(results_list[[dv]], conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) %>%
    mutate(dependent_var = dv) %>%
    rename(conf.low95 = conf.low, conf.high95 = conf.high)
  
  # Extract 90% confidence intervals
  tidied_model_90 <- tidy(results_list[[dv]], conf.int = TRUE, conf.level = 0.90) %>%
    filter(term %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) %>%
    mutate(dependent_var = dv) %>%
    select(-c(statistic, p.value, std.error, estimate)) %>%
    rename(conf.low90 = conf.low, conf.high90 = conf.high)
  
  # Join the 95% and 90% results by dependent_var and term
  tidied_model <- left_join(tidied_model_95, tidied_model_90, by = c("dependent_var", "term"))
  
  # Add number of observations to the tidied results
  nobs <- nobs(results_list[[dv]])
  tidied_model$nobs <- nobs
  
  tidied_results <- bind_rows(tidied_results, tidied_model)
}


### Plot

tidied_results |>
  mutate(term = case_when(
    term == "op_expo_ew_y" ~ "Opportunity",
    term == "rg_expo_ew_y" ~ "Regulatory",
    term == "ph_expo_ew_y" ~ "Physical",
    TRUE ~ term
  ),
  color = ifelse(p.value < 0.05, "black", "darkgrey"),
  dependent_var = paste(dependent_var, "(n =", nobs, ")")
  ) |>
  ggplot(aes(x = estimate, y = dependent_var, color = color)) +
  facet_wrap(facets = ~term, scales = "free_x") +
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
  labs(x = "Coefficient", y = "Issue") +
  theme(text = element_text(size = 15)) +
  theme_bw()

ggsave("results/figures/regressions/placebos_occurrence.pdf", width = 8.5, height = 11)




# Expenditure -------------------------------------------------------------

##Identify placebo variables
# Specify columns to remove
columns_to_remove <- c("amount_num_CAW", "amount_num_ENV", "amount_num_ENG", "amount_num_FUE", "amount_num_REL") # climate issues + REL constant zero (no lobbying)

# Remove specified columns
df_placebo <- df |>
  select(-all_of(columns_to_remove)) |>
  # all to numeric
  mutate_at(vars(amount_num_AGR:amount_num_FAM), as.numeric)

start_index <- which(names(df_placebo) == "amount_num_AGR")
end_index <- which(names(df_placebo) == "amount_num_FAM")

# Extract column names between "AGR" and "REL" into a character vector
dependent_vars <- names(df_placebo)[start_index:end_index]

# View the character vector of column names
print(dependent_vars)

# Initialize an empty list to store model results
results_list_expenditure <- list()

# Iterate over the dependent variables and fit the feols model with fixed effects for each
for (dv in dependent_vars) {
  formula <- as.formula(paste("log(", dv, " + 1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year"))
  
  # Debugging
  print(formula)
  
  # Fit the feols model
  model <- feols(formula, data = df_placebo)
  
  # Store the model results in the list
  results_list_expenditure[[paste(dv, sep = "_")]] <- summary(model)
}

# Create an empty dataframe to store tidied results
tidied_results_expenditure <- data.frame()

# Iterate over the results_list to tidy each model's results
for (dv in names(results_list_expenditure)) {
  # Debug
  print(dv)
  
  # Extract 95% confidence intervals
  tidied_model_95 <- tidy(results_list_expenditure[[dv]], conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) %>%
    mutate(dependent_var = dv) %>%
    rename(conf.low95 = conf.low, conf.high95 = conf.high)
  
  # Extract 90% confidence intervals
  tidied_model_90 <- tidy(results_list_expenditure[[dv]], conf.int = TRUE, conf.level = 0.90) %>%
    filter(term %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) %>%
    mutate(dependent_var = dv) %>%
    select(-c(statistic, p.value, std.error, estimate)) %>%
    rename(conf.low90 = conf.low, conf.high90 = conf.high)
  
  # Join the 95% and 90% results by dependent_var and term
  tidied_model <- left_join(tidied_model_95, tidied_model_90, by = c("dependent_var", "term"))
  
  # Add number of observations to the tidied results
  nobs <- nobs(results_list_expenditure[[dv]])
  tidied_model$nobs <- nobs
  
  tidied_results_expenditure <- bind_rows(tidied_results_expenditure, tidied_model)
}



### Plot

tidied_results_expenditure |>
  mutate(term = case_when(
    term == "op_expo_ew_y" ~ "Opportunity",
    term == "rg_expo_ew_y" ~ "Regulatory",
    term == "ph_expo_ew_y" ~ "Physical",
    TRUE ~ term
  ),
  color = ifelse(p.value < 0.05, "black", "darkgrey"),
  dependent_var = str_replace(dependent_var, "amount_num_", ""),
  ) |>
  ggplot(aes(x = estimate, y = dependent_var, color = color)) +
  facet_wrap(facets = ~term, scales = "free_x") +
  geom_point(size = 2) + 
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1.25) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
  # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
  labs(x = "Coefficient", y = "Issue") +
  theme(text = element_text(size = 15)) +
  theme_bw()

ggsave("results/figures/regressions/placebos_expenditure.pdf", width = 8.5, height = 11)


# Issue codes table -------------------------------------------------------

codes <- read_excel("data/01_raw/lobbyview/lobbying_issue_codes.xlsx")

codes |> kbl(format = "latex", booktabs = T, longtable = T,
             caption = "Lobbying Report Issue Codes") |> 
  save_kable("data/01_raw/lobbyview/lobbying_issue_codes.tex")

### END