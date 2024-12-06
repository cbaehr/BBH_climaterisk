### Firms & Lobbying
### Analysis

### Quarterly

rm(list=ls())

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra, corrplot)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

# Rename fixed effects variables
df <- df |>
  mutate(
    Firm = isin,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure",
        "op_sent_ew" = "Opportunity Sentiment",
        "rg_sent_ew" = "Regulatory Sentiment",
        "ph_sent_ew" = "Physical Sentiment",
        "op_pos_ew" = "Opportunity Pos. Sent.",
        "rg_pos_ew" = "Regulatory Pos. Sent.",
        "ph_pos_ew" = "Physical Pos. Sent.",
        "op_neg_ew" = "Opportunity Neg. Sent.",
        "rg_neg_ew" = "Regulatory Neg. Sent.",
        "ph_neg_ew" = "Physical Neg. Sent.",
        "op_risk_ew" = "Opportunity Risk",
        "rg_risk_ew" = "Regulatory Risk",
        "ph_risk_ew" = "Physical Risk",
        "cc_expo_ew" = "Overall Exposure",
        "op_expo_ew:rg_expo_ew" = "Opp. x Reg.",
        "op_expo_ew:ph_expo_ew" = "Opp. x Phy.",
        "rg_expo_ew:ph_expo_ew" = "Reg. x Phy.",
        "op_expo_ew:op_sent_ew" = "Opp. x Sent.",
        "rg_expo_ew:rg_sent_ew" = "Reg. x Sent.",
        "ph_expo_ew:ph_sent_ew" = "Phy. x Sent.",
        "op_expo_ew:cc_sent_ew" = "Opp. x General Sent.",
        "cc_sent_ew:rg_expo_ew" = "Reg. x General Sent.",
        "cc_sent_ew:ph_expo_ew" = "Phy. x General Sent.",
        "op_expo_ew:op_pos_ew" = "Opp. x Pos.",
        "rg_expo_ew:rg_pos_ew" = "Reg. x Pos.",
        "ph_expo_ew:ph_pos_ew" = "Phy. x Pos.",
        "op_expo_ew:op_neg_ew" = "Opp. x Neg.",
        "rg_expo_ew:rg_neg_ew" = "Reg. x Neg.",
        "ph_expo_ew:ph_neg_ew" = "Phy. x Neg.",
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_quarter" = "Total Lobbying (\\$)",
        "CLI_l1" = "Lagged DV",
        "log_CLI_amount_l1" = "Lagged DV"
)

df$CLI <- as.numeric(df$CLI_quarter)

df$log_CLI_amount <- log(df$CLI_amount_quarter + 1)

df <- df %>%
  group_by(Firm) %>%
  mutate(CLI_l1 = lag(CLI, n=1, order_by=yearqtr),
         log_CLI_amount_l1 = lag(log_CLI_amount, n=1, order_by=yearqtr))

glimpse(df)

df %>%
  filter(CLI_amount_quarter<100000 & CLI_amount_quarter>0) %>%
  filter(year %in% c(2017, 2021)) %>%
  ggplot() +
  geom_histogram(aes(x=CLI_amount_quarter), binwidth=500) +
  geom_vline(xintercept = 13000, col="red", lty=2) +
  theme_classic() +
  scale_x_continuous(breaks=seq(0, 100000, 10000)) +
  labs(x="Quarterly Lobbying Disbursement (2017-21)")
ggsave("ajps_review/disburse_q_REVISION.pdf", width=15, height=8)

#cor(df$op_expo_ew, df$op_sent_ew, use="pairwise.complete.obs")
#cor(df$rg_expo_ew, df$rg_sent_ew, use="pairwise.complete.obs")
#cor(df$ph_expo_ew, df$ph_sent_ew, use="pairwise.complete.obs")
expo_sent_cor <- cor(df[ , c("op_expo_ew", "op_sent_ew", "rg_expo_ew", "rg_sent_ew", "ph_expo_ew", "ph_sent_ew")], use="pairwise.complete.obs")
colnames(expo_sent_cor) <- rownames(expo_sent_cor) <- c("Opp. Expo.", "Opp. Sent.", "Reg. Expo.", "Reg. Sent.", "Phy. Expo.", "Phy. Sent.")
pdf("results/figures/descriptives/corrplot_expo_sent.pdf", width=5, height=5)
corrplot(expo_sent_cor)
dev.off()

expo_risk_cor <- cor(df[ , c("op_expo_ew", "op_risk_ew", "rg_expo_ew", "rg_risk_ew", "ph_risk_ew", "ph_risk_ew")], use="pairwise.complete.obs")
colnames(expo_risk_cor) <- rownames(expo_risk_cor) <- c("Opp. Expo.", "Opp. Risk", "Reg. Expo.", "Reg. Risk", "Phy. Expo.", "Phy. Risk")
pdf("results/figures/descriptives/corrplot_expo_risk.pdf", width=5, height=5)
corrplot(expo_risk_cor)
dev.off()

# Lobbying Occurrence ------------------------------------------------------


## Overall exposure  -------------------------------------------------

# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew_y, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
#   "(6)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
#   "(7)" = feglm(CLI ~ cc_expo_ew + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
# )
# 
# 
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_year_NEW.tex"
# )


# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + gvkey, family = "binomial", df)
# )
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_qrt_FIRM.tex"
# )



## Exposure components -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Year + Firm),
  "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, family = "binomial", df, vcov = ~ Year + Firm),
  "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, family = "binomial", df, vcov = ~ Year + Firm),
  "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, family = "binomial", df, vcov = ~ Year + Firm),
  "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm),
  "(7)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, family = "binomial", df, vcov = ~ Year + Firm),
  "(8)" = feglm(CLI ~ op_pos_ew + rg_pos_ew + ph_pos_ew + op_neg_ew + rg_neg_ew + ph_neg_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(9)" = feglm(CLI ~ op_sent_ew + rg_sent_ew + ph_sent_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(10)" = feglm(CLI ~ op_risk_ew + rg_risk_ew + ph_risk_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_logit_qrt_bycomponent_MODELS_REVISION.RData")

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add Wald tests
# compute_wald <- function(fixest_mod, var1, var2) {
#   a <- fixest_mod$coefficients[var1] #var1 coef
#   b <- fixest_mod$coefficients[var2] #var2 coef
#   a_var <- vcov(fixest_mod)[var1, var1] #var1 variance
#   b_var <- vcov(fixest_mod)[var2, var2] #var2 variance
#   ab_cov <- vcov(fixest_mod)[var1, var2] #var1-2 covariance
#   wald <- a-b / sqrt(a_cov + b_cov - 2 * ab_cov) #wald stat
#   return(wald)
# }
# 
# compute_wald(models[[1]], "op_expo_ew", "ph_expo_ew")
# compute_wald(models[[1]], "rg_expo_ew", "ph_expo_ew")
# wald <- lapply(models[1:7], FUN = function(x) {c(compute_wald(x, "op_expo_ew", "rg_expo_ew"), compute_wald(x, "op_expo_ew", "ph_expo_ew"), compute_wald(x, "rg_expo_ew", "ph_expo_ew"))})
# #wald[8] <- c(compute_wald(models[[8]], "op_expo_ew", "rg_expo_ew"), compute_wald(models[[8]], "op_expo_ew", "ph_expo_ew"), compute_wald(models[[8]], "rg_expo_ew", "ph_expo_ew"))
# do.call(cbind, wald)


### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', '', '', '', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', '', '', '', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark', '', '', ''),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
    )
  

### Bind together
stats <- bind_rows(adjusted_r2_df, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_logit_qrt_bycomponent_REVISION.tex"
  , escape = FALSE
)



# Inspect model 6
m6 <- models$`(6)`
m6[["fixef_id"]][["Year"]] # year fixed effects: 2001-2020
models$`(7)`[["fixef_id"]][["Firm"]] #  firm fixed effects: 614


# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|RMSE'
# )

### calculate F statistics for each model
# fitstat

# Calculate additional fit statistics
# fitstat(models$`(1)`, ~ f + wf + wald + my)
### 

### Interactions  -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew, family = "binomial", df, vcov = ~ Year + Firm),
  "(2)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, family = "binomial", df, vcov = ~ Year + Firm),
  "(3)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, family = "binomial", df, vcov = ~ Year + Firm),
  "(4)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, family = "binomial", df, vcov = ~ Year + Firm),
  "(5)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(6)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm),
  "(7)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, family = "binomial", df, vcov = ~ Year + Firm),
  "(8)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(9)" = feglm(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, family = "binomial", df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_logit_qrt_bycomponent_interactions_MODELS_REVISION.RData")

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', '', '', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', '', '', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark', '\\checkmark', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_logit_qrt_bycomponent_interactions_REVISION.tex"
  , escape = FALSE
)

### Lagged DV  -----------------------------------------------------


## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + CLI_l1, family = "binomial", df, vcov = ~ Year + Firm),
  "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1, family = "binomial", df, vcov = ~ Year + Firm),
  "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | Year, family = "binomial", df, vcov = ~ Year + Firm),
  "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | Year + Industry, family = "binomial", df, vcov = ~ Year + Firm),
  "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
  "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm),
  "(7)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, family = "binomial", df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_logit_qrt_bycomponent_laggeddv_MODELS_REVISION.RData")

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_logit_qrt_bycomponent_laggeddv_REVISION.tex"
  , escape = FALSE
)



## OLS -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION.RData")

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_qrt_bycomponent_REVISION.tex"
  , escape = FALSE
)



# Inspect model 6
m6 <- models$`(6)`
m6[["fixef_id"]][["Year"]] # year fixed effects: 2001-2020
models$`(7)`[["fixef_id"]][["Firm"]] #  firm fixed effects: 614


# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|RMSE'
# )

### calculate F statistics for each model
# fitstat

# Calculate additional fit statistics
# fitstat(models$`(1)`, ~ f + wf + wald + my)
### 

## OLS - Interactions -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm),
  "(8)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(9)" = feols(CLI ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', '', '', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', '', '', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark', '\\checkmark', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_qrt_bycomponent_interactions_REVISION.tex"
  , escape = FALSE
)

## OLS - Lagged DV -----------------------------------------------------


## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + CLI_l1, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + CLI_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + CLI_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_qrt_bycomponent_laggeddv_REVISION.tex"
  , escape = FALSE
)



## OLS - Amount -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)

save(models, file="data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION.RData")

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_amount_qrt_bycomponent_REVISION.tex"
  , escape = FALSE
)



# Inspect model 6
m6 <- models$`(6)`
m6[["fixef_id"]][["Year"]] # year fixed effects: 2001-2020
models$`(7)`[["fixef_id"]][["Firm"]] #  firm fixed effects: 614


# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|RMSE'
# )

### calculate F statistics for each model
# fitstat

# Calculate additional fit statistics
# fitstat(models$`(1)`, ~ f + wf + wald + my)
### 

## OLS - Interactions - Amount -----------------------------------------------------

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm),
  "(8)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(9)" = feols(log_CLI_amount ~ op_expo_ew*rg_expo_ew + op_expo_ew*ph_expo_ew + rg_expo_ew*ph_expo_ew + ebit + ebit_at + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', '', '', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', '', '', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark', '\\checkmark', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_amount_qrt_bycomponent_interactions_REVISION.tex"
  , escape = FALSE
)

## OLS - Lagged DV - Amount -----------------------------------------------------


## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + log_CLI_amount_l1, data=df, vcov = ~ Year + Firm),
  "(2)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1, data=df, vcov = ~ Year + Firm),
  "(3)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | Year, data=df, vcov = ~ Year + Firm),
  "(4)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | Year + Industry, data=df, vcov = ~ Year + Firm),
  "(5)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year`, data=df, vcov = ~ Year + Firm),
  "(6)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + log_CLI_amount_l1 | Year + Firm, data=df, vcov = ~ Year + Firm),
  "(7)" = feols(log_CLI_amount ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter + log_CLI_amount_l1 | `Industry x Year` + Firm, data=df, vcov = ~ Year + Firm)
)

### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark', ''),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' ', ''),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' ', '\\checkmark'),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_amount_qrt_bycomponent_laggeddv_REVISION.tex"
  , escape = FALSE
)

### w/ firm fixed effects ---------------------------------------------------
# 
# models <- list(
#   "(1)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year, family = "binomial", df),
#   "(5)" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + gvkey, family = "binomial", df)
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   #title = 'Effect of Climate Change Attention (components) on Lobbying on Climate Issues',
#   coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_qrt_FIRM.tex"
# )



## Quarter FEs -------------------------------------------------------------

df <- df |>
  mutate(
    Quarter = yearqtr,
    `Industry x Quarter` = paste(Industry, Quarter)
    )

## Effect of climate exposure on lobbying occurrence
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Quarter, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Quarter + Industry, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Quarter`, family = "binomial", df, vcov = ~ Quarter + Firm),
  "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Quarter + Firm, family = "binomial", df, vcov = ~ Quarter + Firm)
)


### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Quarter FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' '),
  `Industry x Quarter FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' '),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Quarter.FE" ~ "Quarter FE",
      Test == "Industry.x.Quarter.FE" ~ "Industry x Quarter FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_logit_qrt_bycomponent_qrtFEs.tex"
  , escape = FALSE
)



# ## All standardized --------------------------------------------------------
# 
# ## Effect of climate exposure on lobbying occurrence
# models <- list(
#   "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Year + Firm),
#   "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew | Year, family = "binomial", df, vcov = ~ Year + Firm),
#   "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled | Year, family = "binomial", df, vcov = ~ Year + Firm),
#   "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | Year, family = "binomial", df, vcov = ~ Year + Firm),
#   "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | Year + Industry, family = "binomial", df, vcov = ~ Year + Firm),
#   "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + us_dummy_scaled + total_lobby_quarter_scaled | Year + Industry + `Industry x Year`, family = "binomial", df, vcov = ~ Year + Firm),
#   "(7)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_scaled + ebit_at_scaled + total_lobby_quarter_scaled | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm)
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01)
#   #, coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   # ,output = "results/tables/climate_logit_qrt_bycomponent.tex"
# )



# Lobbying Expenditure ----------------------------------------------------

## Overall climate lobbying (DOLLARS), overall exposure for quarter
models <- list(
  "(1)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew, df, vcov = ~ Year + Firm),
  "(2)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter, df, vcov = ~ Year + Firm),
  "(3)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year, df, vcov = ~ Year + Firm),
  "(4)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | Year + Industry, df, vcov = ~ Year + Firm),
  "(5)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, df, vcov = ~ Year + Firm),
  "(6)" = feols(log(CLI_amount_quarter +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + total_lobby_quarter | Year + Firm, df, vcov = ~ Year + Firm)
)



### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Year FE` = c(' ', '', '\\checkmark', '\\checkmark', '', '\\checkmark'),
  `Industry FE` = c(' ', ' ', ' ', '\\checkmark', '', ' '),
  `Industry x Year FE` = c(' ', ' ', ' ', ' ', '\\checkmark', ' '),
  `Firm FE` = c(' ', ' ', ' ', ' ', ' ', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.FE" ~ "Industry FE",
      Test == "Firm.FE" ~ "Firm FE",
      Test == "Year.FE" ~ "Year FE",
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm,
  gof_map = "nobs"
  , add_rows = stats
  ,output = "results/tables/climate_ols_amount_qrt_bycomponent.tex"
  , escape = FALSE
)



# ## Plot --------------------------------------------------------------------
# 
# # Base R Plot
# plotmods <- list(models[[4]], models[[5]])
# pdf("results/figures/regressions/coefplot_byexposure_qrt.pdf")
# coefplot(plotmods,
#          dict = c(op_expo_ew="Opportunity", rg_expo_ew="Regulatory", ph_expo_ew="Physical"),
#          keep = c("Opportunity", "Regulatory", "Physical"), horiz=T, ylim.add = c(-0.5, 1), ci.lty=c(1),
#          main = " ")
# legend("topright", col = 1:2, pch = c(16, 17), lwd = 1, lty = 1,
#        legend = c("Year", "+ Year*Industry"), title = "Fixed Effects")
# dev.off()




# Occurence by Issue Area -------------------------------------------------


# ## Disaggregated lobby issues, overall climate exposure, quarter
# models2 <- list(
#   "(1)" = feglm(CLI_CAW_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df),
#   "(2)" = feglm(CLI_ENG_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df),
#   "(3)" = feglm(CLI_ENV_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df),
#   "(4)" = feglm(CLI_FUE_quarter ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df)
# )
# 
# x <- modelsummary(
#   models2
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   ,coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output="latex"
#   ,vcov = ~ year + industry
# ) |>
#   # column labels
#   add_header_above(c(
#     " " = 1,
#     "Clean Air and Water" = 1,
#     "Energy" = 1, 
#     "Environment" = 1,
#     "Fuel, Gas, and Oil" = 1))
# save_kable(x, file="results/tables/climate_logit_qrt_separate_issues.tex", keep_tex = T)


# Aggreate and disaggregated lobby issues, disaggregated exposure types, quarter
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(2)" = feglm(CLI_CAW_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(3)" = feglm(CLI_ENG_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(4)" = feglm(CLI_ENV_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df),
  "(5)" = feglm(CLI_FUE_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | `Industry x Year`, family = "binomial", df)
)


### Get all adjusted pseudo R2
adjusted_r2_df <- data.frame(
  Test = rep("Adj. Pseudo R2", length(models)),
  Value = sapply(models, function(model) {
    r2(model, type = "apr2")
  }),
  Model = names(models)
) %>%
  # round to three decimals
  mutate(Value = round(Value, 3)) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))


### Get F stat for exposure variables
# Apply the wald function to each model in the list
models_f <- wald_results <- lapply(models, function(model) {
  wald(model, keep = c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"), vcov = vcov(model))
})

# Extract Wald test statistics for each model
wald_stats <- lapply(models_f, function(w) {
  c(F_stat = round(w$stat, 3), P_val = round(w$p, 3))
})

wald_stats <- data.frame(
  Test = rep(c("Exposure F. Stat", "Exposure F p-val"), each = length(models)),
  Value = c(
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["F_stat"])),
    sapply(wald_stats, function(stat) sprintf("%.3f", stat["P_val"]))
  ),
  Model = rep(names(models), 2)
) %>%
  pivot_wider(names_from = Model, values_from = Value) %>%
  mutate(across(everything(), as.character))

### Add fixed effects checkmarks: as data.frame
fes <- data.frame(
  `Industry x Year FE` = c('\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark', '\\checkmark'),
  Model = names(models)) %>%
  # invert dataframe
  pivot_longer(cols = -Model, names_to = "Fixed Effects", values_to = "Value") %>%
  # to wider
  pivot_wider(names_from = Model, values_from = Value) %>%
  # add test name
  rename(Test = `Fixed Effects`) %>%
  # Test: Industry x Year FE
  mutate(
    Test = case_when(
      Test == "Industry.x.Year.FE" ~ "Industry x Year FE",
      TRUE ~ " "
    )
  )


### Bind together
stats <- bind_rows(adjusted_r2_df, wald_stats, fes)


# Generate model summary with additional Wald test statistics
y <- modelsummary(
  models
  , stars = c('*' = .1, '**' = .05, '***' = .01)
  , coef_map = cm
  , gof_map = "nobs"
  , add_rows = stats
  , output = "latex"
  , escape = FALSE
  , vcov = ~ Year + Firm
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Climate" = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))
save_kable(y, file="results/tables/climate_logit_qrt_bycomponent_separate_issues.tex", keep_tex = T)


# Region level analysis --------------------------------------------------

# us <- c("United States")
# eur <- c("France", "Germany", "Ireland", "Netherlands", "Switzerland", "United Kingdom", "Sweden", "Finland", "Norway", "Italy", "Denmark", 
#          "Belgium", "Luxembourg", "Spain", "Czechia", "Russia", "Austria")
# asia <- c("Japan", "China", "South Korea", "India", "Singapore", "Philippines", "Taiwan")
# 
# df$hqloc <- ifelse(df$country_name %in% us, "usa",
#                    ifelse(df$country_name %in% eur, "europe",
#                           ifelse(df$country_name %in% asia, "asia", NA)))


# ## Overall climate lobbying, overall exposure for quarter by specific attention component
# models <- list(
#   "USA" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df[which(df$hqcountrycode=="usa"), ]),
#   "Europe" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df[which(df$hqcountrycode=="europe"), ]),
#   "Asia" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df[which(df$hqcountrycode=="asia"), ])
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   #title = 'Effect of Climate Change Attention on Lobbying on Climate Issues, by Region',
#   coef_map = cm
#   ,vcov = ~ year + industry
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   output = "latex_tabular"
#   #,output = "climate_logit_qrt.tex"
# )|>
#   # column labels
#   add_header_above(c(
#     " " = 1,
#     "USA" = 1,
#     "Europe" = 1,
#     "Asia" = 1))


# # Plots -------------------------------------------------------------------
# 
# ###Lobbying compared across time for top 10 industries by total attention 
# #filter industries 
# top10ind_total <- df |> 
#   filter(industry %in% c("Automotive Dealers and Gasoline Service Stations", "Coal Mining", "Construction - General Contractors & Operative Builders", "Electric, Gas, and Sanitary Services", "Electronic & Other Electrical Equipment and Components", "Heavy Construction, Except Building Construction and Contractors", "Local & Suburban Transit and Interurban Highway Transportation", "Petroleum Refining and Related Industries", "Primary Metal Industries", "Transportation Equipment"))
# 
# #plot total lobbying reports on climate for each industry year 
# top10ind_lobby <- top10ind_total |>
#   group_by(year, industry) |>
#   summarise(total_climate_reports = sum(CLI), na.rm=TRUE)
# 
# ggplot(data = top10ind_lobby, aes(x = year, y = total_climate_reports, group = industry)) +
#   geom_line(aes(color = industry)) +
#   scale_color_viridis(discrete=TRUE) + 
#   labs(title = " ", x = "Year", y = "Total Climate Lobbying (# Reports)", color = "Industry") +
#   theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))
# 
# ggsave("results/Figures/lobbing_timeseries.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

#plot total spending in reports tagged to climate for each industry year - needs to be updated for correct amount variable
#top10ind_spend <- top10ind_total |>
#  filter(CLI == 1)|>
# group_by(year, industry) |> 
#summarise(total_climate_spend = sum(amount))

#ggplot(data = top10ind_spend, aes(x = year, y = total_climate_spend, group = industry)) +
#geom_line(aes(color = industry)) +
#scale_color_viridis(discrete=TRUE) + 
#labs(title = " ", x = "Year", y = "Total Climate Lobbying (\\$)", color = "Industry") +
#theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

#ggsave("../results/Figures/lobbingspend_timeseries_industry.pdf", width=unit(8, units="in"), height=unit(6, units="in"))


###Scatterplot of climate attention and total lobbying 

