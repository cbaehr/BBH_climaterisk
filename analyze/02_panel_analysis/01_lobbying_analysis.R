### Firms & Lobbying
### Analysis

rm(list=ls())

# devtools::install_github('IQSS/Zelig') # needs be done only once
# load packages
pacman::p_load(data.table, tidyverse, modelsummary, 
               marginaleffects, kableExtra, fixest,
               janitor, viridis, censReg, Amelia, Zelig, texreg)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
#df <- fread("data/03_final/lobbying_df_wide_reduced_normal.csv")
df <- fread("data/03_final/lobbying_df_wide_reduced.csv")

# Specify covariate names
cm <- c("op_expo_ew_y" = "Opportunity Exposure",
        "rg_expo_ew_y" = "Regulatory Exposure",
        "ph_expo_ew_y" = "Physical Exposure", 
        "cc_expo_ew_y" = "Overall Exposure", 
        "cc_expo_ew_q" = "Overall Exposure",
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying ($)"
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


models <- list(
  "(1)" = feglm(CLI ~ cc_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(CLI ~ cc_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
)
modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/tables/climate_logit_year_FIRM.tex"
)



## Overall exposure quarterly ----------------------------------------------

modelsq <- list(
  "(1)" = feglm(CLI ~ cc_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(CLI ~ cc_expo_ew_q | year, family = "binomial", df),
  "(3)" = feglm(CLI ~ cc_expo_ew_q + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(CLI ~ cc_expo_ew_q + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(CLI ~ cc_expo_ew_q + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
  "(6)" = feglm(CLI ~ cc_expo_ew_q + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

modelsummary(
  modelsq,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention on Lobbying on Climate Issues',
  coef_map = cm
  ,vcov = ~ year + industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/tables/climate_logit_qtr.tex"
)




## Exposure components -----------------------------------------------------

## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
  "(6)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(7)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention (components) on Lobbying on Climate Issues',
  coef_map = cm
  ,vcov = ~ year + industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  #,output = "latex"
  ,output = "results/tables/climate_logit_year_bycomponent.tex"
)



### w/ firm fixed effecrs ---------------------------------------------------

models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention (components) on Lobbying on Climate Issues',
  coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  #,output = "latex"
  ,output = "results/tables/climate_logit_year_bycomponent_FIRM.tex"
)



# Lobbying Expenditure ----------------------------------------------------


# Overall exposure annual -------------------------------------------------


## Overall climate lobbying (DOLLARS), overall exposure for annual
models <- list(
  "(1)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y, df[which(df$conm!="BANK OF AMERICA CORPORATION"),]),
  "(2)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y | year, df),
  "(3)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) | year, df),
  "(4)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, df),
  "(5)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, df),
  "(6)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, df),
  "(7)" = feols(log(CLI_dollars +1) ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, df[which(df$conm!="BANK OF AMERICA CORPORATION"),])
)

# Tobit model
test <- censReg(CLI_dollars ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby + factor(year) + factor(gvkey), data=df, left=0, right=Inf)
summary(test)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/tables/climate_ols_dollars_year_FIRM.tex"
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

########################

## Multiple Imputation of missing data

nms <- c("CLI", "op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y", "ebit", "at", 
         "us_dummy", "total_lobby", "year", "report_quarter_code", "gvkey", "industry")
df.pre_imp <- data.frame(df)
df.pre_imp <- df.pre_imp[, nms]
df.pre_imp$ebit_per_at <- df.pre_imp$ebit / df.pre_imp$at
df.pre_imp$year_industry <- paste(df.pre_imp$year, df.pre_imp$industry)
df.pre_imp$year <- as.numeric(df.pre_imp$year)

## dont place any logical bounds on the values variables can take (e.g. ebit negative sometimes)
## include quadratic time trends BY INDUSTRY in prediction
## also include lagged and lead versions of the dependent variable in prediction
df.imp <- amelia(df.pre_imp, m=5, ts="year", cs="industry", 
                 idvars = c("report_quarter_code", "gvkey", "year_industry"), polytime = 2,
                 intercs = T, lags = "CLI", leads = "CLI")

f1 <- "CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y"
f2 <- "CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_per_at + us_dummy + total_lobby"
f3 <- "CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_per_at + us_dummy + total_lobby + factor(year)"
f4 <- "CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_per_at + us_dummy + total_lobby + factor(year) + factor(industry)"
f5 <- "CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_per_at + us_dummy + total_lobby + factor(year) + factor(industry) + factor(year_industry)"
f6 <- "CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + ebit_per_at + us_dummy + total_lobby + factor(year) + factor(gvkey)"

form <- lapply(list(f1, f2, f3, f4, f5, f6), as.formula)


#ind <- c(1:3)
z.out <- lapply(form, FUN = function(x) zelig(x, data=df.imp, model="logit", cite=F))
# for(i in ind) {
#   if(i==1) { z.out <- list() }
#   z.out[[i]] <- zelig(form[[i]], data=df.imp, model="logit", cite=F)
#   print(i)
# }

z.out <- lapply(z.out, extract)

texreg(z.out, "/Users/christianbaehr/Desktop/out.tex", stars = c(0.01, 0.05, 0.1), 
       omit.coef = "year_industry|year[0-9]{4}|gvkey|Intercept", digits = 3,
       custom.model.names = paste0("(", 1:5, ")"))








