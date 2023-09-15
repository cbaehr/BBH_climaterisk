### Firms & Lobbying
### Analysis

rm(list=ls())

# load packages
library(data.table)
library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(kableExtra)
library(fixest)
library(janitor)
library(viridis)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
df <- fread("data/lobbying_df_wide__red_w_directionality.csv")



# Transform Variables -----------------------------------------------------

# dummy variable: climate issues
df <- df |>
  mutate(CLI = ifelse(ENV == 1 | 
                            CAW == 1 |
                            ENG == 1 |
                            FUE == 1,
                          1,0))

df$CLI_dollars <- apply(df[, c("amount_num_ENV", "amount_num_CAW", "amount_num_ENG", "amount_num_FUE")],
                        1, function(x) sum(x, na.rm=T) / 1000000)

#summary stats for lobbying dummy variables
climate_table <- df |> 
  tabyl(CLI)

cleanair_table <- df |> 
  tabyl(CAW)

energy_table <- df |> 
  tabyl(ENG)

fuel_table <- df |>
  tabyl(FUE)

env_table <- df |>
  tabyl(ENV)


# Control variables -------------------------------------------------------

#US dummy variable 
df <- df |>
  mutate(us_dummy = ifelse(hqcountrycode == "US",1,0))

#Rename CO2 emissions variable 
# df <- df |>
#   rename(co2_emissions = En_En_ER_DP023)

#Total annual lobbying (total dollars)
df <- df |>
  group_by(gvkey, year) |>
  # mutate(total_lobby = n_distinct(report_uuid))
  mutate(total_lobby = sum(c_across(grep("amount_num", names(df), value=T))))


##Summary statistics for control variables 
datasummary((`Earnings Before Interest and Taxes ($M)` = ebit) + (`Total Assets ($M)` = at) + (`Total Lobbying Per Year(#)` = total_lobby) ~ Mean + SD + P25 + P75 + N,
            data = df,
            title = 'Control Variables Summary Statistics',
            align = 'lccccc',
            fmt = 0,
            output = 'latex')

#Log and lag emissions variable 
# df <- df |>
#   mutate(log_co2 = log(co2_emissions + 1))

df <- df |>
  # group by unit (in our case: firm)
  group_by(gvkey) |>
  # arrange by year
  arrange(year) |>
  # for one year
  mutate(#log_co2_l1 = lag(log_co2, 1),
         total_lobby_l1 = lag(total_lobby, 1)) |>
  #ungroup
  ungroup()


df$industry <- df$bvd_sector
df <- df[which(df$industry!=""), ]
df$industry_year <- paste(df$industry, df$year)

sum(duplicated(df[, c("year", "report_quarter_code", "gvkey")]))

## continuous variables in regression models
df_cont_vars <- c("cc_expo_ew_y", "cc_expo_ew_q", "op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y",
                  "ebit", "at", "total_lobby")
## pull from main data
df_cont <- df[, df_cont_vars]
## rescale to standard normal
df_cont <- scale(df_cont)
## slot back into main df
df[, df_cont_vars] <- df_cont




# Lobbying Occurence ------------------------------------------------------

## Logit models ------------------------------------------------------------

## Overall climate lobbying, overall exposure for annual
models <- list(
  "(1)" = feglm(CLI ~ cc_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(CLI ~ cc_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
  "(6)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df),
  "(7)" = feglm(CLI ~ cc_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + gvkey, family = "binomial", df)
)

# names
cm <- c("cc_expo_ew_y" = "Overall Exposure", 
        "ebit" = "EBIT",
        "I(ebit/at)" = "EBIT/Assets",
        "log_co2_l1" = "Log(Total CO2 Emissions)",
        "us_dummy" = "US HQ",
        "total_lobby" = "Total Lobbying ($)",
        "cc_expo_ew_q" = "Overall Exposure",
        "op_expo_ew_y" = "Opportunity Exposure",
        "rg_expo_ew_y" = "Regulatory Exposure",
        "ph_expo_ew_y" = "Physical Exposure")

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/climate_logit_year_NEW.tex"
)

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
  ,output = "results/climate_logit_year_FIRM.tex"
)



## Overall climate lobbying, overall exposure for quarterly
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
  ,output = "results/climate_logit_qtr.tex"
)

###

## Overall climate lobbying, overall exposure for annual by specific attention component
models <- list(
  "(1)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y, family = "binomial", df),
  "(2)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y | year, family = "binomial", df),
  "(3)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) | year, family = "binomial", df),
  "(4)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year, family = "binomial", df),
  "(5)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry, family = "binomial", df),
  "(6)" = feglm(CLI ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby | year + industry + industry_year, family = "binomial", df)
)

modelsummary(
  models,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  #title = 'Effect of Climate Change Attention (components) on Lobbying on Climate Issues',
  coef_map = cm
  ,vcov = ~ year + industry
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  #,output = "latex"
  ,output = "results/climate_logit_year_bycomponent.tex"
)

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
  ,output = "results/climate_logit_year_bycomponent_FIRM.tex"
)


library(censReg)
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

test <- censReg(CLI_dollars ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/at) + us_dummy + total_lobby + factor(year) + factor(gvkey), data=df, left=0, right=Inf)
summary(test)

modelsummary(
  models
  ,stars = c('*' = .1, '**' = .05, '***' = .01)
  #,title = 'Effect of Climate Change Attention on Lobbying on Climate Issues'
  ,coef_map = cm
  ,vcov = ~ year + gvkey
  ,gof_omit = 'AIC|BIC|Log.Lik|Std.Errors|RMSE'
  ,output = "results/climate_ols_dollars_year_FIRM.tex"
)


#################################################################################

plotmods <- list(models[[5]], models[[6]])
pdf("results/Figures/coefplot_byexposure.pdf")
coefplot(plotmods, 
         dict = c(op_expo_ew_y="Opportunity", rg_expo_ew_y="Regulatory", ph_expo_ew_y="Physical"),
         keep = c("Opportunity", "Regulatory", "Physical"), horiz=T, ylim.add = c(-0.5, 1), ci.lty=c(1),
         main = " ")
legend("topright", col = 1:2, pch = c(16, 17), lwd = 1, lty = 1,
       legend = c("Year", "+ Year*Industry"), title = "Fixed Effects")
dev.off()

###

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
save_kable(x, file="results/climate_logit_year_separateissues.tex", keep_tex = T)

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
save_kable(y, file="results/climate_logit_year_bycomponent_separateissues.tex", keep_tex = T)

## Region level analysis --------------------------------------------------

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


# Plots -------------------------------------------------------------------

###Lobbying compared across time for top 10 industries by total attention 
#filter industries 
top10ind_total <- df |> 
  filter(industry %in% c("Automotive Dealers and Gasoline Service Stations", "Coal Mining", "Construction - General Contractors & Operative Builders", "Electric, Gas, and Sanitary Services", "Electronic & Other Electrical Equipment and Components", "Heavy Construction, Except Building Construction and Contractors", "Local & Suburban Transit and Interurban Highway Transportation", "Petroleum Refining and Related Industries", "Primary Metal Industries", "Transportation Equipment"))

#plot total lobbying reports on climate for each industry year 
top10ind_lobby <- top10ind_total |>
  group_by(year, industry) |>
  summarise(total_climate_reports = sum(CLI), na.rm=TRUE)

ggplot(data = top10ind_lobby, aes(x = year, y = total_climate_reports, group = industry)) +
  geom_line(aes(color = industry)) +
  scale_color_viridis(discrete=TRUE) + 
  labs(title = " ", x = "Year", y = "Total Climate Lobbying (# Reports)", color = "Industry") +
  theme_light() + theme(plot.title = element_text(hjust = 0.5), axis.title=element_text(size=14))

ggsave("results/Figures/lobbing_timeseries.pdf", width=unit(8, units="in"), height=unit(6, units="in"))

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

