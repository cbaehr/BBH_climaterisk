### Within industry variation in climate change exposure


# load packages
pacman::p_load(data.table, tidyverse, haschaR)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
# df <- fread("02_processed/exposure_year.csv", colClasses = c("sic"="character"))
# df$sic <- as.numeric(substr(df$sic, 1, 2))

df_orig <- arrow::read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")
df <- df_orig

# Within industry variation in exposure -----------------------------------

glimpse(df)
summary(df$op_expo_ew)
summary(df$rg_expo_ew)
summary(df$ph_expo_ew)
# merge
df <- df |> 
  # filter out empty industry
  filter(industry != "") |> 
  # select variables we need
  select(isin, year, industry, op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  pivot_longer(cols = op_expo_ew:ph_expo_ew, names_to = "Exposure", values_to = "Value") |>
  mutate(Exposure = recode(Exposure,
                           op_expo_ew = "Opportunity",
                           ph_expo_ew = "Physical",
                           rg_expo_ew = "Regulatory"))

# calculate number of firms per industry
industry_n_firms <- df |>
  group_by(industry) |>
  count()

# Calculate variance within each industry
industry_var <- df |>
  # group by industry
  group_by(industry, Exposure) |>
  # get variance for each industry
  summarise(Variance = var(Value, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(industry)) |>
  arrange(desc(Variance))

# Calculate mean variance across exposure variables
industry_var_levels <- industry_var |>
  group_by(industry) |>
  summarise(mean = mean(Variance, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(industry)) |>
  arrange((mean)) |>
  tail(15)


## Violin Plot -----
df |>
  filter(industry %in% industry_var_levels$industry) |>
  mutate(
    industry = factor(industry, levels = industry_var_levels$industry),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical")),
    ) |>
  ggplot(aes(y = Value, x = industry)) +
  facet_wrap(vars(Exposure), nrow = 1, scales = "free_x") +
  geom_violin(trim = T,
              fill = "darkgrey",
              scale = "width") +
  coord_flip() +
  theme_bw() +
  labs(y = "Distribution of Exposure Variables", x = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))


ggsave("results/Figures/descriptives/within_industry_variances_TOP15_violin.pdf", width=10, height=6)

## BoxPlot ------
df |>
  filter(industry %in% industry_var_levels$industry) |>
  mutate(
    industry=factor(industry, levels=industry_var_levels$industry),
    industry = fct_relabel(industry, ~str_wrap(., width = 40)),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical")),
    ) |>
  ggplot(aes(y=Value,x=industry)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "free_x") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
               ) +
  coord_flip() + 
  theme_bw() +
  labs(y = "Distribution of Exposure Variables", x = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))


ggsave("results/Figures/descriptives/within_industry_variances_TOP15_boxplot.pdf", width=10, height=6)


## BoxPlot with same axis -----
df |>
  filter(industry %in% industry_var_levels$industry) |>
  mutate(
    industry=factor(industry, levels=industry_var_levels$industry),
    industry = fct_relabel(industry, ~str_wrap(., width = 40)),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical"))
  ) |>
  ggplot(aes(y=Value,x=industry)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "fixed") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
  ) +
  coord_flip() + 
  theme_bw() +
  labs(y = "Distribution of Exposure Variables", x = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))


ggsave("results/Figures/descriptives/within_industry_variances_TOP15_boxplot_fixed.pdf", width=10, height=6)


glimpse(df)
summary(df$Value)

# summary(log$Value)

# log |>
#   filter(Value == 0)
#   # none

# Log scale version
df |>
  filter(industry %in% industry_var_levels$industry) |>
  mutate(
    Value = log(Value + 0.000001),
    industry=factor(industry, levels=industry_var_levels$industry),
    industry = fct_relabel(industry, ~str_wrap(., width = 35)),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical"))
  ) |>
  ggplot(aes(y=Value,x=industry)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "fixed") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
  ) +
  coord_flip() + 
  # scale_y_log10(labels = scales::label_number()) +
  theme_bw() +
  labs(y = "Distribution of Exposure Variables (log scale)", x = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))

ggsave("results/Figures/descriptives/within_industry_variances_TOP15_boxplot_fixed_log.pdf", width=10, height=8)


# W/ sautner data before processing ----------------------------------------

df_orbis <- data.table::fread("data/02_processed/exposure_orbis_client_quarter_long_REVISE_NEW.csv")

summary(df_orbis$op_expo_ew)
hist(df_orbis$op_expo_ew)
names(df_orbis)
nrow(df_orbis)
# merge
df <- df_orbis |> 
  # filter out empty industry
  filter(bvdsector != "") |> 
  # select variables we need
  select(isin, year, bvdsector, op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  pivot_longer(cols = op_expo_ew:ph_expo_ew, names_to = "Exposure", values_to = "Value") |>
  mutate(Exposure = recode(Exposure,
                           op_expo_ew = "Opportunity",
                           ph_expo_ew = "Physical",
                           rg_expo_ew = "Regulatory"),
         round = round(Value, digits = 5))

glimpse(df)
hist(df$Value)
# calculate number of firms per industry
industry_n_firms <- df |>
  group_by(bvdsector) |>
  count()

# Calculate variance within each industry
industry_var <- df |>
  # group by industry
  group_by(bvdsector, Exposure) |>
  # get variance for each industry
  summarise(Variance = var(Value, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(bvdsector)) |>
  arrange(desc(Variance))

# Calculate mean variance across exposure variables
industry_var_levels <- industry_var |>
  group_by(bvdsector) |>
  summarise(mean = mean(Variance, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(bvdsector)) |>
  arrange((mean)) |>
  tail(15)


## BoxPlot with same axis -----
df |>
  filter(bvdsector %in% industry_var_levels$bvdsector) |>
  mutate(
    bvdsector=factor(bvdsector, levels=industry_var_levels$bvdsector),
    bvdsector = fct_relabel(bvdsector, ~str_wrap(., width = 40)),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical"))
  ) |>
  ggplot(aes(y=Value,x=bvdsector)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "fixed") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
  ) +
  coord_flip() + 
  theme_bw() +
  labs(y = "Distribution of Exposure Variables", x = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))


ggsave("results/Figures/descriptives/within_industry_variances_TOP15_boxplot_fixed_orbis.pdf", width=10, height=6)

# Log scale version
summary(df$Value)


df |>
  filter(bvdsector %in% industry_var_levels$bvdsector) |>
  mutate(
    Value = log(Value),
    bvdsector=factor(bvdsector, levels=industry_var_levels$bvdsector),
    bvdsector = fct_relabel(bvdsector, ~str_wrap(., width = 35)),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical"))
  ) |>
  ggplot(aes(y=Value,x=bvdsector)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "fixed") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
  ) +
  coord_flip() + 
  # scale_y_log10(labels = scales::label_number()) +
  theme_bw() +
  labs(y = "Distribution of Exposure Variables (log scale)", x = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))

ggsave("results/Figures/descriptives/within_industry_variances_TOP15_boxplot_fixed_log_orbis.pdf", width=10, height=8)




glimpse(df)
summary(df$Value)

# summary(log$Value)

# log |>
#   filter(Value == 0)
#   # none

# Log scale version
df |>
  filter(bvdsector %in% industry_var_levels$bvdsector) |>
  mutate(
    Value = log(Value + 0.000001),
    bvdsector=factor(bvdsector, levels=industry_var_levels$bvdsector),
    bvdsector = fct_relabel(bvdsector, ~str_wrap(., width = 35)),
    Exposure = factor(Exposure, levels = c("Opportunity", "Regulatory", "Physical"))
  ) |>
  ggplot(aes(y=Value,x=bvdsector)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "fixed") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
  ) +
  coord_flip() + 
  # scale_y_log10(labels = scales::label_number()) +
  theme_bw() +
  labs(y = "Distribution of Exposure Variables (log scale)", x = "") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "bottom",
        text = element_text(size = 15))

ggsave("results/Figures/descriptives/within_industry_variances_TOP15_boxplot_fixed_log.pdf", width=10, height=8)





# Create balance table ----------------------------------------------------

# load data
df <- arrow::read_parquet("data/03_final/lobbying_df_quarterly_REVISE_normal_NEW.parquet")


# get extra covars (similar code to 01d_lobbying_analysis_quarterly_trimmed.R)
df$subs_iso3c <- unlist(lapply(df$subs_iso3c, FUN = function(x) paste(gsub("n.a.", "", unique(unlist(strsplit(x, "\n")))), collapse="|")))
df$subs_iso3c <- gsub("NA", "", df$subs_iso3c)

df$subs_iso3c[which(nchar(df$subs_iso3c)==3)] <- gsub("\\|", "", df$subs_iso3c[which(nchar(df$subs_iso3c)==3)])
df$subs_iso3c[which(df$subs_iso3c=="")] <- NA

df$country_iso_code[which(df$isin=="NL00150002Q7")] <- "DE"

df$multinational <- 0
df$multinational[which(df$subs_iso3c != df$country_iso_code)] <- 1

## If the subsidiary column isnt EXACTLY equal to the man country column, this
## implies there is a subsidiary in another country.

newdat <- read.csv("data/01_raw/boards_rep_v2/analysis_data_no_proprietary.csv", stringsAsFactors = F)
newdat <- newdat[ , c("gvkey", "year", "cso_exists", "cdp_report")]
df <- merge(df, newdat, by=c("gvkey", "year"), all.x=T)

df$cso_exists[which(is.na(df$cso_exists) & df$year %in% c(2000:2019))] <- 0
df$cdp_report[which(is.na(df$cdp_report) & df$year %in% c(2000:2019))] <- 0



# Load covariate data
cov <- df |>
  select(isin, year, qtr, op_expo_ew, rg_expo_ew, ph_expo_ew, ebit, ebit_at, total_lobby_quarter, n_employees, cso_exists, cdp_report, multinational, us_dummy)


clist <- c("ebit", "ebit_at", "total_lobby_quarter", "op_expo_ew", "rg_expo_ew", "ph_expo_ew", "n_employees", "cso_exists", "cdp_report", "multinational", "us_dummy") 

names <- c("EBIT", "EBIT/Assets", "Total Lobbying ($)", "Opportunity", "Regulatory", "Physical", "Number of Employees", "CSO Exists", "CDP Report", "Multinational", "US Headquarters")

# We want to get the characteristics of firms that are above median and below median in their industry for each exposure variable and see how they differ on covariates
# 1. identify whether firms is above or below/equal median for each industry - year
# 2. calculate mean of covariates for each group

# transform
df_trans <- df |> 
  # filter out empty industry
  filter(industry != "") |> 
  # select variables we need
  select(isin, year, qtr, industry, op_expo_ew, rg_expo_ew, ph_expo_ew) |>
  pivot_longer(cols = op_expo_ew:ph_expo_ew, names_to = "Exposure", values_to = "Value") |>
  mutate(Exposure = recode(Exposure,
                           op_expo_ew = "Opportunity",
                           ph_expo_ew = "Physical",
                           rg_expo_ew = "Regulatory"),
         round = round(Value, digits = 2))

names(df)

# Note: we select 2010, qtr == 1as year for balance table
df_10 <- df_trans |>
  filter(year == 2010 & qtr == 1)


# 1. identify whether firms is above or below/equal median for each industry - year
above_median <- df_10 |>
  group_by(industry, Exposure) |>
  mutate(median = median(Value, na.rm = TRUE)) |>
  mutate(above_median = Value > median) |>
  ungroup() |>
  filter(!is.na(industry)) |>
  select(isin, Exposure, Value, median, above_median) |>
  distinct() |>
  arrange(isin, Exposure)

# 2. calculate mean and sds of covariates for each group
prep_df <- df_10 |>
  left_join(above_median |>
              select(isin, Exposure, above_median),
            by = c("isin", "Exposure")) |>
  left_join(cov |> 
              filter(year == 2010 & qtr == 1) |>
              select(-c(year, qtr)),
            by = c("isin")) |>
  filter(!is.na(above_median)) |>
  select(-c(isin, year, qtr))

prep_df_opp <- prep_df |> 
  filter(Exposure =="Opportunity") |>
  mutate(above_median = ifelse(above_median == TRUE, 1, 0))

prep_df_reg <- prep_df |> 
  filter(Exposure =="Regulatory") |>
  mutate(above_median = ifelse(above_median == TRUE, 1, 0))

prep_df_phy <- prep_df |>
  filter(Exposure =="Physical") |>
  mutate(above_median = ifelse(above_median == TRUE, 1, 0))

# get balance for opportunity
bal1 <- haschaR::get_bal(treatvar = "above_median", 
                         cov_list = clist,
                         data.df = prep_df_opp,
                         FE = NULL, weights = NULL
) 

bal2 <- haschaR::get_bal(treatvar = "above_median", 
                         cov_list = clist,
                         data.df = prep_df_opp,
                         FE = "industry", weights = NULL
)

b_plot_levels_opp <- bal1 %>%
  mutate(fe = "Base") %>%
  bind_rows(bal2 %>% mutate(fe = "Differences in levels within industry")) %>%
  left_join(data.frame(cov = clist, name = names), by = "cov") |>
  mutate(exp = "Opportunity") |>
  filter(cov != "op_expo_ew")


# get balance for regulatory
bal3 <- haschaR::get_bal(treatvar = "above_median", 
                         cov_list = clist,
                         data.df = prep_df_reg,
                         FE = NULL, weights = NULL
)

bal4 <- haschaR::get_bal(treatvar = "above_median", 
                         cov_list = clist,
                         data.df = prep_df_reg,
                         FE = "industry", weights = NULL
)

b_plot_levels_reg <- bal3 %>%
  mutate(fe = "Base") %>%
  bind_rows(bal4 %>% mutate(fe = "Differences in levels within industry")) %>%
  left_join(data.frame(cov = clist, name = names), by = "cov")|>
  mutate(exp = "Regulatory") |>
  filter(cov != "rg_expo_ew")

# get balance for physical
bal5 <- haschaR::get_bal(treatvar = "above_median", 
                         cov_list = clist,
                         data.df = prep_df_phy,
                         FE = NULL, weights = NULL
)

bal6 <- haschaR::get_bal(treatvar = "above_median", 
                         cov_list = clist,
                         data.df = prep_df_phy,
                         FE = "industry", weights = NULL
)

b_plot_levels_phy <- bal5 %>%
  mutate(fe = "Base") %>%
  bind_rows(bal6 %>% mutate(fe = "Differences in levels within industry")) %>%
  left_join(data.frame(cov = clist, name = names), by = "cov") |>
  mutate(exp = "Physical") |>
  filter(cov != "ph_expo_ew")

b_plot_levels <- bind_rows(b_plot_levels_opp, b_plot_levels_reg, b_plot_levels_phy)  |>
  filter(!is.na(exp))

pd <- position_dodge(0.5)

b_plot_levels %>%
  mutate(
    # reorder: Opportunity, Regulatory, Physical, Ebit, EBIT/Assets, Total Lobbying
    name = factor(name, levels = c("Multinational", "US Headquarters", "Number of Employees", "CSO Exists", "CDP Report", "Total Lobbying ($)", "EBIT/Assets", "EBIT",  "Physical", "Regulatory", "Opportunity")),
    exp = factor(exp, levels = c("Opportunity", "Regulatory", "Physical"))
  ) |>
  ggplot(aes(x = name, y = coef, color = fe, group = fe)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.0, position = pd) +
  geom_point(position = pd) +
  facet_wrap(~exp, scales = "free_x") +
  haschaR::theme_hanno() +
  labs(
    x = "",
    y = "Correlations with Exposure Variables",
    color = ""
  ) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Base" = "black", "Differences in levels within industry" = "darkgrey"))

ggsave("results/figures/descriptives/balance_plot.pdf", width = 7, height = 6)


#move_plots_to_overleaf("./")


# Correlation plot --------------------------------------------------------


df |>
  select(`Climate Occurrence` = CLI_quarter, `Climate Expenditure` = CLI_amount_quarter, `Opportunity` = op_expo_ew, `Regulatory` = rg_expo_ew, `Physical` = ph_expo_ew, `EBIT` = ebit, `EBIT/Assets` = ebit_at, `Total Lobbying` = total_lobby_quarter, `Number of Employees` = n_employees, `CSO Exists` = cso_exists, `CDP Report` = cdp_report, `Multinational` = multinational, `US Headquarters` = us_dummy) |>
  cor(use = "complete.obs") |>
  corrplot::corrplot(method = 'square', addCoef.col = 'black',
  type = "upper")


# End
