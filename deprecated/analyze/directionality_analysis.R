# Directionality anaylsis


# Directionality ----------------------------------------------------------

## Climate Attention ----------------
models <- list(
  "(1)" = feglm(pro_CLI ~ cc_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(pro_CLI ~ cc_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(pro_CLI ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(pro_CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(pro_CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
  "(6)" = feglm(pro_CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention on Lobbying Pro Climate Regulation',
  coef_map = cm
  ,vcov = ~ year + industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/climate_directionality_logit_year.tex"
)



## Differentiate attention measures --------

models <- list(
  "(1)" = feglm(pro_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(pro_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(pro_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(pro_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(pro_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
  "(6)" = feglm(pro_CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention (components) on Lobbying Pro Climate Regulation',
  coef_map = cm
  ,vcov = ~ year + industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/climate_directionality_logit_bycomponent_year.tex"
)
