### Firms & Lobbying
### Analysis

rm(list=ls())

# load packages
pacman::p_load(tidyverse, fixest, modelsummary, kableExtra)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- read_rds("data/03_final/lobbying_df_annual_REVISE_normal.rds")

# Rename fixed effects variables
df <- df |>
  rename(
    Firm = gvkey,
    Year = year,
    Industry = industry,
    `Industry x Year` = industry_year
  )

# Specify covariate names
cm <- c("op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure", 
        "cc_expo_ew" = "Overall Exposure", 
        "ebit" = "EBIT",
        "ebit_at" = "EBIT/Assets",
        "us_dummy" = "US HQ",
        "total_lobby_annual" = "Total Lobbying ($)"
)

# Change classes for analysis ---------------------------------------------


df <- df %>% mutate( CLI = as.numeric(CLI_annual ))


table(df$CLI, useNA = "ifany")
class(df$cc_expo_ew)

# Lobbying Occurrence ------------------------------------------------------


## Overall exposure annual -------------------------------------------------

# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby | year + industry, family = "binomial", df),
#   "(6)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
#   "(7)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
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
#   ,output = "results/tables/climate_logitear_NEW.tex"
# )


# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + gvkey, family = "binomial", df)
# )
# 
# modelsummary(
#   models
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
#   ,coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   ,output = "results/tables/climate_logit_year_FIRM.tex"
# )



## Exposure components -----------------------------------------------------

## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df, vcov = ~ Year + Industry),
  "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew | Year, family = "binomial", df, vcov = ~ Year + Industry),
  "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at | Year, family = "binomial", df, vcov = ~ Year + Industry),
  "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year, family = "binomial", df, vcov = ~ Year + Industry),
  "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry, family = "binomial", df, vcov = ~ Year + Industry),
  "(6)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, family = "binomial", df, vcov = ~ Year + Industry),
  "(7)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Firm, family = "binomial", df, vcov = ~ Year + Firm)
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = cm
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/tables/climate_logit_year_bycomponent.tex"
)



# ### w/ firm fixed effecrs ---------------------------------------------------
# 
# models <- list(
#   "(1)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew, family = "binomial", df),
#   "(2)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + gvkey, family = "binomial", df)
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   coef_map = cm
#   ,vcov = ~ year + gvkey
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   #,output = "latex"
#   ,output = "results/tables/climate_logit_year_bycomponent_FIRM.tex"
# )



# Lobbying Expenditure ----------------------------------------------------


# Overall exposure annual -------------------------------------------------


## Overall climate lobbying (DOLLARS), overall exposure for annual
models <- list(
  "(1)" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew, df, vcov = ~ Year + Industry),
  "(2)" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual, df, vcov = ~ Year + Industry),
  "(3)" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year, df, vcov = ~ Year + Industry),
  "(4)" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry, df, vcov = ~ Year + Industry),
  "(5)" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, df, vcov = ~ Year + Industry),
  "(6)" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Firm, df, vcov = ~ Year + Firm)
)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  ,coef_map = cm
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/tables/climate_ols_amount_year_bycomponent.tex"
)



# ## Plot --------------------------------------------------------------------
# 
# # Base R Plot
# plotmods <- list(models[[4]], models[[5]])
# pdf("results/figures/regressions/coefplot_byexposure_year.pdf")
# coefplot(plotmods,
#          dict = c(op_expo_ew="Opportunity", rg_expo_ew="Regulatory", ph_expo_ew="Physical"),
#          keep = c("Opportunity", "Regulatory", "Physical"), horiz=T, ylim.add = c(-0.5, 1), ci.lty=c(1),
#          main = " ")
# legend("topright", col = 1:2, pch = c(16, 17), lwd = 1, lty = 1,
#        legend = c("Year", "+ Year*Industry"), title = "Fixed Effects")
# dev.off()




# # Occurence by Issue Area -------------------------------------------------
# 
# 
# ## Disaggregated lobby issues, overall climate exposure, annual
# models2 <- list(
#   "(1)" = feglm(CLI_CAW_annual ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df),
#   "(2)" = feglm(CLI_ENG_annual ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df),
#   "(3)" = feglm(CLI_ENV_annual ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df),
#   "(4)" = feglm(CLI_FUE_annual ~ cc_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df)
# )
# 
# x <- modelsummary(
#   models2
#   ,stars = c('*' = .1, '**' = .05, '***' = .01)
#   #title = 'Effect of Climate Change Attention on Lobbying Across Disaggregated Climate Issues',
#   ,coef_map = cm
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   #,output = "results/climate_logitear_separateissues.tex"
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
# save_kable(x, file="results/tables/climate_logit_year_separate_issues.tex", keep_tex = T)


##Aggreate and disaggregated lobby issues, disaggregated exposure types, annual
models3 <- list(
  "(1)" = feglm(CLI_annual ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, family = "binomial", df),
  "(2)" = feglm(CLI_CAW_annual ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, family = "binomial", df),
  "(3)" = feglm(CLI_ENG_annual ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, family = "binomial", df),
  "(4)" = feglm(CLI_ENV_annual ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, family = "binomial", df),
  "(5)" = feglm(CLI_FUE_annual ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | Year + Industry + `Industry x Year`, family = "binomial", df)
)

y <- modelsummary(
  models3
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  ,coef_map = cm
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "latex"
  ,vcov = ~ Year + Industry
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Climate" = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))
save_kable(y, file="results/tables/climate_logit_year_bycomponent_separate_issues.tex", keep_tex = T)


# # Region level analysis --------------------------------------------------
# 
# us <- c("United States")
# eur <- c("France", "Germany", "Ireland", "Netherlands", "Switzerland", "United Kingdom", "Sweden", "Finland", "Norway", "Italy", "Denmark", 
#          "Belgium", "Luxembourg", "Spain", "Czechia", "Russia", "Austria")
# asia <- c("Japan", "China", "South Korea", "India", "Singapore", "Philippines", "Taiwan")
# 
# df$hqloc <- ifelse(df$country_name %in% us, "usa",
#                    ifelse(df$country_name %in% eur, "europe",
#                           ifelse(df$country_name %in% asia, "asia", NA)))
# 
# 
# ## Overall climate lobbying, overall exposure for annual by specific attention component
# models <- list(
#   "USA" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df[which(df$hqloc=="usa"), ]),
#   "Europe" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df[which(df$hqloc=="europe"), ]),
#   "Asia" = feglm(CLI ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df[which(df$hqloc=="asia"), ])
# )
# 
# modelsummary(
#   models,
#   stars = c('*' = .1, '**' = .05, '***' = .01),
#   #title = 'Effect of Climate Change Attention on Lobbying on Climate Issues, by Region',
#   coef_map = cm
#   ,vcov = ~ year + industry
#   ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
#   #,output = "latex"
#   #,output = "climate_logit_year.tex"
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
#labs(title = " ", x = "Year", y = "Total Climate Lobbying ($)", color = "Industry") +
#theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

#ggsave("../results/Figures/lobbingspend_timeseries_industry.pdf", width=unit(8, units="in"), height=unit(6, units="in"))


###Scatterplot of climate attention and total lobbying 

