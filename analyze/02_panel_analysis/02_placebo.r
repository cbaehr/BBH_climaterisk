### Firms & Climate Lobbying
### Search for Placebo


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, haschaR, broom, fixest, readxl, kableExtra, equivtest)

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


# Occurrence --------------------------------------------------------------


# #Load dataset
# df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")
# 
# 
# # Specify covariate names
# cm <- c("op_expo_ew" = "Opportunity Exposure",
#         "rg_expo_ew" = "Regulatory Exposure",
#         "ph_expo_ew" = "Physical Exposure", 
#         "cc_expo_ew" = "Overall Exposure",
#         "ebit" = "EBIT",
#         "ebit_at" = "EBIT/Assets",
#         "us_dummy" = "US HQ",
#         "total_lobby_quarter" = "Total Lobbying (\\$)"
# )
# 
# ##Identify placebo variables
# # Specify columns to remove: climate issues + REL constant zero (no lobbying) +  NA
# columns_to_remove <- c("CLI_CAW_quarter", "CLI_ENV_quarter", "CLI_ENG_quarter", 
#                        "CLI_FUE_quarter", "CLI_NA_quarter", "CLI_REL_quarter") 
# 
# 
# # Remove specified columns
# df_placebo <- df |> select(-all_of(columns_to_remove))
# 
# start_index <- which(names(df_placebo) == "CLI_HOM_quarter")
# end_index <- which(names(df_placebo) == "CLI_UNM_quarter")
# 
# # Extract column names between "AGR" and "REL" into a character vector
# dependent_vars <- names(df_placebo)[start_index:end_index]
# 
# # View the character vector of column names
# print(dependent_vars)
# 
# # Initialize an empty list to store model results
# results_list <- list()
# 
# # Iterate over the dependent variables and fit the feglm model with fixed effects for each
# 
# for (dv in dependent_vars) { {
#   formula <- as.formula(paste(dv, "~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | industry_year")) # Include fixed effects in the formula
#   
#   # Debugging
#   print(formula)
#   
#   # Fit the feglm model
#   model <- feglm(formula, family = "binomial", data = df_placebo, vcov = ~ isin + year)
#   
#   # Store the model results in the list
#   results_list[[paste(dv, sep = "_")]] <- summary(model)
# }
# }
# 
# # Create an empty dataframe to store tidied results
# tidied_results <- data.frame()
# 
# # Iterate over the results_list to tidy each model's results
# for (dv in names(results_list)) {
#   # Extract 95% confidence intervals
#   tidied_model_95 <- tidy(results_list[[dv]], conf.int = TRUE, conf.level = 0.95) %>%
#     filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
#     mutate(dependent_var = dv) %>%
#     rename(conf.low95 = conf.low, conf.high95 = conf.high)
#   
#   # Extract 90% confidence intervals
#   tidied_model_90 <- tidy(results_list[[dv]], conf.int = TRUE, conf.level = 0.90) %>%
#     filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
#     mutate(dependent_var = dv) %>%
#     select(-c(statistic, p.value, std.error, estimate)) %>%
#     rename(conf.low90 = conf.low, conf.high90 = conf.high)
#   
#   # Join the 95% and 90% results by dependent_var and term
#   tidied_model <- left_join(tidied_model_95, tidied_model_90, by = c("dependent_var", "term"))
#   
#   # Add number of observations to the tidied results
#   nobs <- nobs(results_list[[dv]])
#   tidied_model$nobs <- nobs
#   
#   tidied_results <- bind_rows(tidied_results, tidied_model)
# }
# 
# 
# # save tidied_results
# fwrite(tidied_results, "results/Tables/pacebo_results_logit.csv")


# Load tidied results
tidied_results <- fread("results/Tables/pacebo_results_logit.csv")


### Plot

plot_df_logit <- tidied_results |>
  mutate(
    term = case_when(
      term == "op_expo_ew" ~ "Opportunity",
      term == "rg_expo_ew" ~ "Regulatory",
      term == "ph_expo_ew" ~ "Physical",
      TRUE ~ term
    ),
    color = ifelse(p.value < 0.05, "black", "darkgrey")
    #,dependent_var = paste(dependent_var, "(n =", nobs, ")")
  ) |>
  # Remove CLI_ and _quarter from dependent_var
  mutate(
    dependent_var = str_remove(dependent_var, "CLI_"),
    dependent_var = str_remove(dependent_var, "_quarter")
  )

plot_df_logit |>
  ggplot(aes(x = estimate, y = dependent_var, color = color)) +
  facet_wrap(facets = ~term, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
  geom_point(shape = 21, fill = "white", size = 2) +
  scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
  labs(x = "Coefficient", y = "Issue") +
  theme(text = element_text(size = 15)) +
  theme_bw()

ggsave("results/figures/regressions/placebos_occurrence.pdf", width = 8.5, height = 11)




# Expenditure -------------------------------------------------------------

# Load tobit regression results
df_tobit <- read_xlsx("results/tables/placebo_results_tobit.xlsx")

# Into long format
df_tobit_long <- df_tobit |>
  pivot_longer(
    cols = starts_with("b_") | starts_with("se_"),
    names_to = c(".value", "exposure"),
    names_pattern = "([a-z]+)_([a-z]+)"
  ) |>
  # rename op, rg, ph, to opportunity, regulatory, physical
  mutate(
    exposure = case_when(
      exposure == "op" ~ "Opportunity",
      exposure == "rg" ~ "Regulatory",
      exposure == "ph" ~ "Physical",
      TRUE ~ exposure
    )
  ) |>
  # rename b to estimate, se to std.error
  rename(
    estimate = b,
    std.error = se
  ) 

# inspect
insp <- df_tobit_long |>
  # count n of std.error = 0 
  summarise(
    zero_se = sum(std.error == 0),
    n = n())
insp
# 102 / 225 = 45% of specifications have std.error = 0.000

# Filter out specifications where std.error is 0.000
df_tobit_long <- df_tobit_long |>
  filter(std.error > 0)


# Create plot_df

plot_df_tobit <- df_tobit_long |>
  # calculate p.value and 90% and 95% confidence intervals
  mutate(
    p.value = 2 * (1-pnorm(abs(estimate / std.error))),
    color = ifelse(p.value < 0.05, "black", "darkgrey"),
    conf.low95 = estimate - 1.96 * std.error,
    conf.high95 = estimate + 1.96 * std.error,
    conf.low90 = estimate - 1.645 * std.error,
    conf.high90 = estimate + 1.645 * std.error
  ) |>
  rename(
    dependent_var = issue,
    term = exposure
  )



### Plot

plot_df_tobit |>
  ggplot(aes(x = estimate, y = dependent_var, color = color)) +
  facet_wrap(facets = ~term, scales = "free_x") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
  geom_point(shape = 21, fill = "white", size = 2) +
  scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
  # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
  labs(x = "Coefficient", y = "Issue") +
  theme(text = element_text(size = 15)) +
  theme_bw()

ggsave("results/figures/regressions/placebos_expenditure.pdf", width = 8.5, height = 11)


# ##Identify placebo variables
# # Specify columns to remove
# columns_to_remove <- c("amount_num_CAW", "amount_num_ENV", "amount_num_ENG", "amount_num_FUE", "amount_num_REL") # climate issues + REL constant zero (no lobbying)
# 
# # Remove specified columns
# df_placebo <- df |>
#   select(-all_of(columns_to_remove)) |>
#   # all to numeric
#   mutate_at(vars(amount_num_AGR:amount_num_FAM), as.numeric)
# 
# start_index <- which(names(df_placebo) == "amount_num_AGR")
# end_index <- which(names(df_placebo) == "amount_num_FAM")
# 
# # Extract column names between "AGR" and "REL" into a character vector
# dependent_vars <- names(df_placebo)[start_index:end_index]
# 
# # View the character vector of column names
# print(dependent_vars)
# 
# # Initialize an empty list to store model results
# results_list_expenditure <- list()
# 
# # Iterate over the dependent variables and fit the feols model with fixed effects for each
# for (dv in dependent_vars) {
#   formula <- as.formula(paste("log(", dv, " + 1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year"))
#   
#   # Debugging
#   print(formula)
#   
#   # Fit the feols model
#   model <- feols(formula, data = df_placebo)
#   
#   # Store the model results in the list
#   results_list_expenditure[[paste(dv, sep = "_")]] <- summary(model)
# }
# 
# # Create an empty dataframe to store tidied results
# tidied_results_expenditure <- data.frame()
# 
# # Iterate over the results_list to tidy each model's results
# for (dv in names(results_list_expenditure)) {
#   # Debug
#   print(dv)
#   
#   # Extract 95% confidence intervals
#   tidied_model_95 <- tidy(results_list_expenditure[[dv]], conf.int = TRUE, conf.level = 0.95) %>%
#     filter(term %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) %>%
#     mutate(dependent_var = dv) %>%
#     rename(conf.low95 = conf.low, conf.high95 = conf.high)
#   
#   # Extract 90% confidence intervals
#   tidied_model_90 <- tidy(results_list_expenditure[[dv]], conf.int = TRUE, conf.level = 0.90) %>%
#     filter(term %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) %>%
#     mutate(dependent_var = dv) %>%
#     select(-c(statistic, p.value, std.error, estimate)) %>%
#     rename(conf.low90 = conf.low, conf.high90 = conf.high)
#   
#   # Join the 95% and 90% results by dependent_var and term
#   tidied_model <- left_join(tidied_model_95, tidied_model_90, by = c("dependent_var", "term"))
#   
#   # Add number of observations to the tidied results
#   nobs <- nobs(results_list_expenditure[[dv]])
#   tidied_model$nobs <- nobs
#   
#   tidied_results_expenditure <- bind_rows(tidied_results_expenditure, tidied_model)
# }
# 
# 
# 
# ### Plot
# 
# tidied_results_expenditure |>
#   mutate(term = case_when(
#     term == "op_expo_ew_y" ~ "Opportunity",
#     term == "rg_expo_ew_y" ~ "Regulatory",
#     term == "ph_expo_ew_y" ~ "Physical",
#     TRUE ~ term
#   ),
#   color = ifelse(p.value < 0.05, "black", "darkgrey"),
#   dependent_var = str_replace(dependent_var, "amount_num_", ""),
#   ) |>
#   ggplot(aes(x = estimate, y = dependent_var, color = color)) +
#   facet_wrap(facets = ~term, scales = "free_x") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
#   geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
#   geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
#   geom_point(shape = 21, fill = "white", size = 2) +
#   scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
#   # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
#   labs(x = "Coefficient", y = "Issue") +
#   theme(text = element_text(size = 15)) +
#   theme_bw()
# 
# ggsave("results/figures/regressions/placebos_expenditure.pdf", width = 8.5, height = 11)



# Produce tables for selected issue codes ---------------------------------


# bind plot_dfs
plot_df_comb <- rbind(
  plot_df_logit |> 
    select(-c(nobs, statistic)) |>
    mutate(Outcome = "Occurrence"), 
  plot_df_tobit |>
    mutate(Outcome = "Expenditure")
)

# Only keep issue codes with 6 observations
plot_df_comb <- plot_df_comb |>
  group_by(dependent_var) |>
  mutate(n = n()) |>
  filter(n == 6) |>
  ungroup() |>
  select(-n)


# # # Plot this: dodge by outcome and different colors for outcome
# plot_df_comb |>
#   mutate(term = case_when(
#     term == "op_expo_ew" ~ "Opportunity",
#     term == "rg_expo_ew" ~ "Regulatory",
#     term == "ph_expo_ew" ~ "Physical",
#     TRUE ~ term
#   ),
#   Outcome = case_when(
#     Outcome == "Occurrence" ~ "Occurrence",
#     Outcome == "Expenditure" ~ "Expenditure",
#     TRUE ~ Outcome
#   ),
#   color = ifelse(p.value < 0.05, "black", "darkgrey"),
#   dependent_var = str_replace(dependent_var, "amount_num_", ""),
#   ) |>
#   ggplot(aes(x = estimate, y = dependent_var, color = color)) +
#   facet_wrap(facets = ~term, scales = "free_x") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) +
#   # dodge errorbars & points by outcome
#   geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95, color = Outcome), width = 0, linewidth = .5, position = position_dodge(width = 0.5)) +
#   geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90, color = Outcome), width = 0, linewidth = 1, position = position_dodge(width = 0.5)) +
#   geom_point(aes(color = Outcome), shape = 21, fill = "white", size = 2, position = position_dodge(width = 0.5)) +
#   # color red and blue
#   scale_color_manual(values = c("red", "blue")) +
#   # scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
#   # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
#   labs(x = "Coefficient", y = "Issue") +
#   theme_bw() +
#   theme(text = element_text(size = 15), legend.position = "bottom")
# 
# 
# 
# plot_df_comb |>
#   filter(dependent_var %in% c("AGR", "AUT", "BUD", "NAT", "TAX", "TRA")) |>
#   mutate(
#     color = ifelse(p.value < 0.05, "black", "darkgrey"),
#     Outcome=factor(Outcome, levels=c("Occurrence", "Expenditure")),
#     ) |>
#   ggplot(aes(x = estimate, y = dependent_var, color = color)) +
#   facet_grid(Outcome ~ term, scales = "free") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
#   geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
#   geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
#   geom_point(shape = 21, fill = "white", size = 2) +
#   scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
#   # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
#   labs(x = "Coefficient", y = "Issue") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         text = element_text(size = 12))


placebo_plot_df <- plot_df_comb |>
  filter(dependent_var %in% c("CSP", "IMM", "BAN", "VET")) |>
  mutate(
    dependent_var = case_when(
      dependent_var == "CSP" ~ "Consumer Protection",
      dependent_var == "IMM" ~ "Immigration",
      dependent_var == "BAN" ~ "Banking",
      dependent_var == "VET" ~ "Veterans",
      TRUE ~ dependent_var
    )
  )


# Plot
placebo_plot_df |>
  mutate(
    Outcome=factor(Outcome, levels=c("Expenditure", "Occurrence")),
    term = factor(term, levels = c("Opportunity", "Regulatory", "Physical")),
  ) |>
  ggplot(aes(x = estimate, y = dependent_var, color = Outcome, shape = Outcome)) +
  facet_grid(~ term, scales = "fixed") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
  geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5, position = position_dodge(width = 0.25)) +
  geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1, position = position_dodge(width = 0.25)) +
  geom_point(size = 2, position = position_dodge(width = 0.25)) +
  scale_y_discrete(limits=rev) +
  # color manual black and darkgrey
  scale_color_manual(values = c("black", "dimgray"), breaks = c("Occurrence", "Expenditure")) +
  scale_shape_manual(values = c(15, 16), breaks = c("Occurrence", "Expenditure")) +
  #scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
  # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
  labs(x = "Coefficient", y = "Lobbying Issue", color = "", shape = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 12),
        legend.position = "bottom")


ggsave("results/figures/regressions/placebos_sample.pdf", width = 8, height = 4)





# placebo_plot_df <- tidied_results |>
#   mutate(Outcome = "Occurrence") |>
#   bind_rows(tidied_results_expenditure |>
#               mutate(Outcome = "Expenditure")) |>
#   mutate(dependent_var = str_replace(dependent_var, "amount_num_", "")) |>
#   filter(dependent_var %in% c("TAR", "MIA", "MON", "FIR", "TAX", "TRA", "ROD", "AUT")) |>
#   mutate(
#     term = case_when(
#       term == "op_expo_ew_y" ~ "Opportunity",
#       term == "rg_expo_ew_y" ~ "Regulatory",
#       term == "ph_expo_ew_y" ~ "Physical",
#       TRUE ~ term
#       ),
#     Category = case_when(
#       dependent_var == "TAR" ~ "Not Climate-related",
#       dependent_var == "MIA" ~ "Not Climate-related",
#       dependent_var == "MON" ~ "Not Climate-related",
#       dependent_var == "FIR" ~ "Not Climate-related",
#       dependent_var == "TAX" ~ "Climate-related",
#       dependent_var == "TRA" ~ "Climate-related",
#       dependent_var == "ROD" ~ "Climate-related",
#       dependent_var == "AUT" ~ "Climate-related",
#       TRUE ~ dependent_var
#     ),
#     dependent_var = case_when(
#       dependent_var == "TAR" ~ "Tariffs",
#       dependent_var == "MIA" ~ "Media",
#       dependent_var == "MON" ~ "Money",
#       dependent_var == "FIR" ~ "Guns",
#       dependent_var == "TAX" ~ "Taxation",
#       dependent_var == "TRA" ~ "Transportation",
#       dependent_var == "ROD" ~ "Roads",
#       dependent_var == "AUT" ~ "Automotives",
#       TRUE ~ dependent_var
#     )
#   )
# 
# 
# # Plot
# placebo_plot_df |>
#   mutate(
#     color = ifelse(p.value < 0.05, "black", "darkgrey"),
#     Outcome=factor(Outcome, levels=c("Occurrence", "Expenditure")),
#     Category = factor(Category, levels = c("Not Climate-related", "Climate-related"))
#     ) |>
#   ggplot(aes(x = estimate, y = dependent_var, color = color)) +
#   facet_grid(Category + Outcome ~ term, scales = "free") +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
#   geom_errorbar(aes(xmin = conf.low95, xmax = conf.high95), width = 0, linewidth = .5) +
#   geom_errorbar(aes(xmin = conf.low90, xmax = conf.high90), width = 0, linewidth = 1) +
#   geom_point(shape = 21, fill = "white", size = 2) +
#   scale_color_identity(guide = "none") +  # Use the actual colors in the 'color' column without a legend
#   # scale_y_discrete(limits = rev(unique(tidied_results$dependent_var))) +
#   labs(x = "Coefficient", y = "Issue") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), 
#         text = element_text(size = 12))
#   
# 
# ggsave("results/figures/regressions/placebos_sample.pdf", width = 8.5, height = 6.2)


# Issue codes table -------------------------------------------------------

codes <- read_excel("data/01_raw/lobbyview/lobbying_issue_codes.xlsx")

nrow(codes)

codes |> kbl(format = "latex", booktabs = T, longtable = T,
             caption = "Lobbying Report Issue Codes") |> 
  save_kable("data/01_raw/lobbyview/lobbying_issue_codes.tex")

# Create simple text combining issue codes and descriptions
codes |>
  mutate(
    Description = str_replace_all(Description, "&", "\\\\&"),
    Description = str_replace_all(Description, "/", " / "),
    combined_text = paste0(Code, " = ", Description)
  ) |>
  pull(combined_text) |>
  paste(collapse = ", ") |>
  cat()




# Equivalence testing -----------------------------------------------------

# Load data
df <- fread("data/03_final/lobbying_df_quarterly_REVISE_normal.csv")

names(df)

?equiv.t.test

# Function to run TOST for each exposure type and issue
run_tost_analysis <- function(data, dv, exposures = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), 
                            eps_sub = 0.1, alpha = 0.05) {
  
  results <- data.frame()
  
  for(exposure in exposures) {
    # Get treatment and control groups based on exposure within industry-quarter groups
    grouped_data <- data %>%
      group_by(industry, yearqtr) %>%
      mutate(
        exposure_group = ifelse(!!sym(exposure) > median(!!sym(exposure), na.rm = TRUE), 
                              "high", "low")
      ) %>%
      ungroup()
    
    # Split into high and low exposure groups
    high_exposure <- grouped_data %>% 
      filter(exposure_group == "high") %>% 
      pull(!!sym(dv))
    
    low_exposure <- grouped_data %>% 
      filter(exposure_group == "low") %>% 
      pull(!!sym(dv))
    
    # Skip if either group is empty
    if(length(high_exposure) == 0 || length(low_exposure) == 0) {
      results <- rbind(results, data.frame(
        issue = dv,
        exposure = exposure,
        rejected_null = NA,
        lower_t = NA,
        upper_t = NA,
        stringsAsFactors = FALSE
      ))
      next
    }
    
    # Run TOST
    tost_result <- try(tost(high_exposure, low_exposure, eps_sub = eps_sub, alpha = alpha), silent = TRUE)
    
    if(inherits(tost_result, "try-error")) {
      # If test fails, add row with NAs
      results <- rbind(results, data.frame(
        issue = dv,
        exposure = exposure,
        rejected_null = NA,
        lower_t = NA,
        upper_t = NA,
        stringsAsFactors = FALSE
      ))
    } else {
      # If test succeeds, add results
      results <- rbind(results, data.frame(
        issue = dv,
        exposure = exposure,
        rejected_null = tost_result$reject,
        lower_t = tost_result$t.lower,
        upper_t = tost_result$t.upper,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

# For occurrence
# Get all issue variables (CLI_*_quarter)
occurrence_vars <- names(df)[grep("^CLI_.*_quarter$", names(df))]
occurrence_vars <- occurrence_vars[!occurrence_vars %in% 
                                 c("CLI_CAW_quarter", "CLI_ENV_quarter", "CLI_ENG_quarter", 
                                   "CLI_FUE_quarter", "CLI_NA_quarter", "CLI_REL_quarter")]

# Run TOST for each occurrence variable
occurrence_results <- do.call(rbind, lapply(occurrence_vars, function(x) {
  run_tost_analysis(df, x)
}))

# For expenditure
# Get all amount variables (amount_num_*)
expenditure_vars <- names(df)[grep("^amount_num_", names(df))]
expenditure_vars <- expenditure_vars[!expenditure_vars %in% 
                                   c("amount_num_CAW", "amount_num_ENV", "amount_num_ENG", 
                                     "amount_num_FUE", "amount_num_REL")]

# Run TOST for each expenditure variable
expenditure_results <- do.call(rbind, lapply(expenditure_vars, function(x) {
  run_tost_analysis(df, x)
}))

# Combine results
all_results <- rbind(
  cbind(occurrence_results, type = "Occurrence"),
  cbind(expenditure_results, type = "Expenditure")
)

# Clean up issue codes
all_results <- all_results %>%
  mutate(
    issue = str_remove(issue, "CLI_"),
    issue = str_remove(issue, "_quarter"),
    issue = str_remove(issue, "amount_num_"),
    exposure = case_when(
      exposure == "op_expo_ew" ~ "Opportunity",
      exposure == "rg_expo_ew" ~ "Regulatory",
      exposure == "ph_expo_ew" ~ "Physical"
    )
  )

# Save results
fwrite(all_results, "results/Tables/equivalence_test_results.csv")

# Create summary visualization
ggplot(all_results, aes(x = exposure, y = issue, fill = rejected_null)) +
  facet_wrap(~type) +
  geom_tile() +
  scale_fill_manual(values = c("white", "red"), 
                    labels = c("Not Equivalent", "Equivalent")) +
  labs(x = "Exposure Type", y = "Issue", fill = "Test Result") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("results/figures/regressions/equivalence_tests.pdf", width = 10, height = 12)


### END

# Test run for a single exposure and outcome
test_exposure <- "op_expo_ew"
test_outcome <- "CLI_CON_quarter"  # Using Homeland Security as test placebo

# Group data and create exposure groups
test_grouped <- df %>%
  group_by(industry, yearqtr) %>%
  mutate(
    exposure_group = ifelse(!!sym(test_exposure) > median(!!sym(test_exposure), na.rm = TRUE), 
                          "high", "low")
  ) %>%
  ungroup()

# Split into high and low exposure groups
high_exposure <- test_grouped %>% 
  filter(exposure_group == "high") %>% 
  pull(!!sym(test_outcome))

low_exposure <- test_grouped %>% 
  filter(exposure_group == "low") %>% 
  pull(!!sym(test_outcome))

# Print summary statistics
print("Summary of test groups:")
print(paste("High exposure group n:", length(high_exposure)))
print(paste("Low exposure group n:", length(low_exposure)))
print(paste("High exposure mean:", mean(high_exposure, na.rm = TRUE)))
print(paste("Low exposure mean:", mean(low_exposure, na.rm = TRUE)))

# Run 
# ?equiv.t.test
test_equiv <- equiv.t.test(high_exposure, low_exposure, eps_tol = "strict", alpha = 0.05)
summary(test_equiv)


# Print results
print("TOST results:")
print(test_tost)
