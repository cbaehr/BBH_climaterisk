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

##Placebo analysis 
#Load normalized dataset
df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")


#Create new variable that is ebit/assets
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

############
##Identify placebo variables
# Specify columns to remove
columns_to_remove <- c("CAW", "ENV", "ENG", "FUE")

# Remove specified columns
df_placebo <- df |>
  select(-all_of(columns_to_remove))

start_index <- which(names(df_placebo) == "AGR")
end_index <- which(names(df_placebo) == "REL")

# Extract column names between "AGR" and "REL" into a character vector
dependent_vars <- names(df_placebo)[start_index:end_index]

# View the character vector of column names
print(dependent_vars)

# Initialize an empty list to store model results
results_list <- list()

# Iterate over the dependent variables and fit the feglm model with fixed effects for each

for (dv in dependent_vars) { {
  formula <- as.formula(paste(dv, "~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_at + us_dummy + total_lobby| year + industry + industry_year")) # Include fixed effects in the formula
  
  # Fit the feglm model
  model <- feglm(formula, family = "binomial", data = df)
  
  # Store the model results in the list
  results_list[[paste(dv, sep = "_")]] <- summary(model)
}
}

# Access the results for each dependent variable and fixed effect combination
for (dv in dependent_vars) {
  cat("Model results for", dv, ":\n")
  print(results_list[[paste(dv, sep = "_")]])
}


############################

###Placebo exploration - Vincent's old code
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


