### Firms & Lobbying
### Analysis

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, modelsummary, 
               marginaleffects, kableExtra, fixest,
               janitor, viridis, censReg)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")
df_REV <- fread("data/03_final/lobbying_df_annual_REVISE_normal.csv")
df_quarterly <- fread("data/03_final/lobbying_df_quarterly_REVISE_normal.csv")

#View(df[, c("year_quarter", "gvkey", "CLI", "CLI_dollars")])

# Specify covariate names
cm <- c("op_expo_ew_y" = "Opportunity Exposure",
        "rg_expo_ew_y" = "Regulatory Exposure",
        "ph_expo_ew_y" = "Physical Exposure", 
        "op_expo_ew" = "Opportunity Exposure",
        "rg_expo_ew" = "Regulatory Exposure",
        "ph_expo_ew" = "Physical Exposure", 
        "cc_expo_ew_y" = "Overall Exposure", 
        "cc_expo_ew_q" = "Overall Exposure",
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "ebit_at" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying ($)",
        "total_lobby_annual" = "Total Lobbying ($)",
        "total_lobby_quarter" = "Total Lobbying ($)"
)



# Lobbying Occurrence ------------------------------------------------------


## Overall exposure annual -------------------------------------------------

# models <- list(
#   "(1)" = feglm(CLI ~ cc_expo_ew_y, family = "binomial", df),
#   "(2)" = feglm(CLI ~ cc_expo_ew_y | year, family = "binomial", df),
#   "(3)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
#   "(4)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
#   "(5)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
#   "(6)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
#   "(7)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
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

df$bvdid <- df$gvkey_n

sum(is.na(df_REV$CLI_annual))

table(df_REV$CLI_annual)

summary(df$total_lobby)
summary(df_REV$total_lobby_annual)

df_REV$CLI_annual <- as.numeric(df_REV$CLI_annual)

models <- list(
  "Orig" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "New" = feglm(CLI_annual ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, family = "binomial", df_REV),
  "New Q" = feglm(CLI_quarter ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df_quarterly)
)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Main Logit, Year-Industry Effects'
  ,coef_map = cm
  ,vcov = ~ year + bvdid
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  # ,output = "results/tables/climate_logit_year_COMPARE.tex"
)


# Lobbying Expenditure ----------------------------------------------------


# Overall exposure annual -------------------------------------------------


## Overall climate lobbying (DOLLARS), overall exposure for annual
models <- list(
  "Old" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, df),
  "New" = feols(log(CLI_amount_annual +1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_at + us_dummy + total_lobby_annual | year + industry + industry_year, df_REV),
  "New Q" = feols(log(CLI_amount_quarter+1) ~ op_expo_ew + rg_expo_ew + ph_expo_ew + ebit_at + us_dummy + total_lobby_quarter | year + industry + industry_year, family = "binomial", df_quarterly)
)

# Tobit model
#test <- censReg(CLI_dollars ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby + factor(year) + factor(gvkey), data=df, left=0, right=Inf)
#summary(test)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + bvdid
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  # ,output = "results/tables/climate_ols_dollars_year_COMPARE.tex"
)



## Plot --------------------------------------------------------------------

# Base R Plot
plotmods <- list(models[[5]], models[[6]])
pdf("results/figures/regressions/coefplot_byexposure.pdf")
coefplot(plotmods,
         dict = c(op_expo_ew_y="Opportunity", rg_expo_ew_y="Regulatory", ph_expo_ew_y="Physical"),
         keep = c("Opportunity", "Regulatory", "Physical"), horiz=T, ylim.add = c(-0.5, 1), ci.lty=c(1),
         main = " ")
legend("topright", col = 1:2, pch = c(16, 17), lwd = 1, lty = 1,
       legend = c("Year", "+ Year*Industry"), title = "Fixed Effects")
dev.off()




# Occurence by Issue Area -------------------------------------------------


## Disaggregated lobby issues, overall climate exposure, annual
models2 <- list(
  "(1)" = feglm(CAW ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(2)" = feglm(ENG ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(3)" = feglm(ENV ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(4)" = feglm(FUE ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

x <- modelsummary(
  models2
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #title = 'Effect of Climate Change Attention on Lobbying Across Disaggregated Climate Issues',
  ,coef_map = cm
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  #,output = "results/climate_logit_year_separateissues.tex"
  ,output="latex"
  ,vcov = ~ year + industry
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))
save_kable(x, file="results/tables/climate_logit_year_separateissues.tex", keep_tex = T)

##Aggreate and disaggregated lobby issues, disaggregated exposure types, annual
models3 <- list(
  "(1)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(2)" = feglm(CAW ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(3)" = feglm(ENG ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(4)" = feglm(ENV ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(5)" = feglm(FUE ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
  )

y <- modelsummary(
  models3
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Different Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "latex"
  ,vcov = ~ year + industry
) |>
  # column labels
  add_header_above(c(
    " " = 1,
    "Climate" = 1,
    "Clean Air and Water" = 1,
    "Energy" = 1, 
    "Environment" = 1,
    "Fuel, Gas, and Oil" = 1))
save_kable(y, file="results/tables/climate_logit_year_bycomponent_separateissues.tex", keep_tex = T)


# Region level analysis --------------------------------------------------

us <- c("United States")
eur <- c("France", "Germany", "Ireland", "Netherlands", "Switzerland", "United Kingdom", "Sweden", "Finland", "Norway", "Italy", "Denmark", 
         "Belgium", "Luxembourg", "Spain", "Czechia", "Russia", "Austria")
asia <- c("Japan", "China", "South Korea", "India", "Singapore", "Philippines", "Taiwan")

df$hqloc <- ifelse(df$country_name %in% us, "usa",
                    ifelse(df$country_name %in% eur, "europe",
                           ifelse(df$country_name %in% asia, "asia", NA)))


## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "USA" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df[which(df$hqloc=="usa"), ]),
  "Europe" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df[which(df$hqloc=="europe"), ]),
  "Asia" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df[which(df$hqloc=="asia"), ])
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention on Lobbying on Climate Issues, by Region',
  coef_map = cm
  ,vcov = ~ year + industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  #,output = "latex"
  #,output = "climate_logit_year.tex"
)|>
  # column labels
  add_header_above(c(
    " " = 1,
    "USA" = 1,
    "Europe" = 1,
    "Asia" = 1))


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

