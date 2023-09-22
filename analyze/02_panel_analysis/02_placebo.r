### Firms & Climate Lobbying
### Search for Placebo


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor, readxl, haschaR)

pacman::p_load(data.table, tidyverse, modelsummary, 
               marginaleffects, kableExtra, fixest,
               janitor, viridis, censReg)

# prevent scientific notation
options(scipen = 999)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/Dropbox (Princeton)/BBH/BBH1/")}


###Placebo exploration 
#load dataset
df <- fread("data/lobbying_df_w_directionality.csv") |> 
  mutate(
    year_quarter = paste0(year, "_", report_quarter_code),
    amount_num = as.numeric(amount_num)
    )

# read in lobbying issue codes
codes <- read_excel("data/LOBBYING LobbyView/lobbying_issue_codes.xlsx")

# merge df with codes
df <- df %>% 
  left_join(codes, by = c("issue_code" = "Code")) %>% 
  rename(issue_code_txt = Description)


# Plot number of reports by issue code txt
df %>% 
  group_by(issue_code_txt) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  filter(n > 100) %>% 
  # Highlight Energy/Nuclear, Environmental/Superfund, Clean Air & Water (Quality), Fuel/Gas/Oil
  mutate(climate = ifelse(issue_code_txt %in% c("Energy/Nuclear", "Environmental/Superfund", "Clean Air & Water (Quality)", "Fuel/Gas/Oil"), 1, 0)) %>%
  # Highlight climate == 1 in red
  ggplot(aes(x = reorder(issue_code_txt, n), y = n, fill = factor(climate))) +
  geom_col() +
  coord_flip() +
  labs(x = "Issue Code", y = "Number of Reports", title = "Number of Reports by Issue Code (1999-2020)") +
  theme_hanno()+ 
  scale_fill_manual(values = c("grey", "red")) +
  theme(legend.position = "none")

# Save plot
ggsave("results/Figures/descriptives/number_of_reports_by_issue_code.pdf", width = 9, height = 13)


# Plot mean lobbying expenditure by issue code txt (in billions)
df %>% 
  group_by(issue_code_txt) %>% 
  summarise(total_expenditure = sum(amount_num)) %>% 
  # divide by 1000000 to get in millions
  mutate(total_expenditure = total_expenditure / 1000000) %>%
  arrange(desc(total_expenditure)) %>% 
  filter(total_expenditure > 1000) %>% 
  # Highlight Energy/Nuclear, Environmental/Superfund, Clean Air & Water (Quality), Fuel/Gas/Oil
  mutate(climate = ifelse(issue_code_txt %in% c("Energy/Nuclear", "Environmental/Superfund", "Clean Air & Water (Quality)", "Fuel/Gas/Oil"), 1, 0)) %>%
  # Highlight climate == 1 in red
  ggplot(aes(x = reorder(issue_code_txt, total_expenditure), y = total_expenditure, fill = factor(climate))) +
  geom_col() +
  coord_flip() +
  # label x axis correctly
  labs(x = "Issue Code", y = "Total Expenditure (billions)", title = "Total Expenditure by Issue Code (1999-2020)") +
  theme_hanno()+ 
  scale_fill_manual(values = c("grey", "red")) +
  theme(legend.position = "none")


# Save plot
ggsave("results/Figures/descriptives/expenditure_by_issue_code.pdf", width = 9, height = 13)


##Placebo analysis 
#Load dataset
df <- fread("data/03_final/lobbying_df_wide_reduced.csv")

#Create new variable that is ebit/assets
# Create a new variable 'Ratio' by dividing 'Var1' by 'Var2'
df$ebit_at <- df$ebit / df$at

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
ag <- feglm(AGR ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_at + us_dummy + total_lobby| year + industry + industry_year, family = "binomial", df)

# Create a vector of dependent variable names (starting with subset)
dependent_vars <- c("AGR", "BUD", "FOO", "MIA")

# Create an empty list to store model results
model_results <- list()

# Define the common independent variable formula
common_formula <- as.formula("dependent ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_at + us_dummy + total_lobby")

# Run the feglm model for each dependent variable
for (dep_var in dependent_vars) {
  # Update the formula with the current dependent variable
  current_formula <- update(common_formula, as.formula(paste(paste0(dep_var, " ~ ."))))
  
  # Fit the feglm model with fixed effects - this does not work, it says industry is not in the dataset
  model <- feglm(current_formula | year + industry + industry_year, family = binomial, data = df)
  
  # Store the model result in the list
  model_results[[dep_var]] <- summary(model)
}

# View the model results for each dependent variable
for (dep_var in dependent_vars) {
  cat("Model results for", dep_var, ":\n")
  print(model_results[[dep_var]])
  cat("\n")
}
