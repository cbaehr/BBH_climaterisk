### Within industry variation in climate change exposure


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# load data
# df <- fread("02_processed/exposure_year.csv", colClasses = c("sic"="character"))
# df$sic <- as.numeric(substr(df$sic, 1, 2))

df <- read_rds(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.rds")


# Within industry variation in exposure -----------------------------------


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
                           rg_expo_ew = "Regulatory"),
         round = round(Value, digits = 2))

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




# Create balance table ----------------------------------------------------

# load data
df <- read_rds(df, file="data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

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

# Load covariate data
cov <- df |>
  select(isin, year, qtr, op_expo_ew, rg_expo_ew, ph_expo_ew, ebit, ebit_at, total_lobby_quarter)

clist <- c("ebit", "ebit_at", "total_lobby_quarter", "op_expo_ew", "rg_expo_ew", "ph_expo_ew") 

names <- c("EBIT", "EBIT/Assets", "Total Lobbying ($)", "Opportunity", "Regulatory", "Physical")

# We want to get the characteristics of firms that are above median and below median in their industry for each exposure variable and see how they differ on covariates
# 1. identify whether firms is above or below/equal median for each industry - year
# 2. calculate mean of covariates for each group

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
                         FE = NULL, weights = NULL
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
    name = factor(name, levels = c("Total Lobbying ($)", "EBIT/Assets", "EBIT", "Physical", "Regulatory", "Opportunity")),
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
    y = "Standardized mean difference in levels in Q1 2010",
    color = ""
  ) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Base" = "black", "Differences in levels within industry" = "darkgrey"))

ggsave("results/figures/descriptives/balance_plot.pdf", width = 7, height = 3.5)

ggsave("../../Apps/Overleaf/Climate Exposure and Political Activity_BBH/graphs/balance_plot.pdf", width = 7, height = 3.5)

# means <- prep_df |>
#   group_by(Exposure, above_median) |>
#   summarise(
#     op_expo_ew = mean(op_expo_ew, na.rm = TRUE),
#     rg_expo_ew = mean(rg_expo_ew, na.rm = TRUE),
#     ph_expo_ew = mean(ph_expo_ew, na.rm = TRUE),
#     ebit = mean(ebit, na.rm = TRUE),
#     ebit_at = mean(ebit_at, na.rm = TRUE),
#     total_lobby_quarter = mean(total_lobby_quarter, na.rm = TRUE)
#   ) |>
#   # pivot longer
#   pivot_longer(cols = c(op_expo_ew, rg_expo_ew, ph_expo_ew, ebit, ebit_at, total_lobby_quarter),
#                names_to = "variable",
#                values_to = "mean")
# 
# sds <- prep_df |>
#   group_by(Exposure, above_median) |>
#   summarise(
#     op_expo_ew = sd(op_expo_ew, na.rm = TRUE),
#     rg_expo_ew = sd(rg_expo_ew, na.rm = TRUE),
#     ph_expo_ew = sd(ph_expo_ew, na.rm = TRUE),
#     ebit = sd(ebit, na.rm = TRUE),
#     ebit_at = sd(ebit_at, na.rm = TRUE),
#     total_lobby_quarter = sd(total_lobby_quarter, na.rm = TRUE)
#   ) |>
#   # pivot longer
#   pivot_longer(cols = c(op_expo_ew, rg_expo_ew, ph_expo_ew, ebit, ebit_at, total_lobby_quarter),
#                names_to = "variable",
#                values_to = "sd")
# 
# balance_table <- means |>
#   left_join(sds, by = c("Exposure", "above_median", "variable")) |>
#   mutate(
#     above_median = case_when(
#       above_median == TRUE ~ "Above Median",
#       above_median == FALSE ~ "Below Median"
#     ),
#     variable = case_when(
#       variable == "op_expo_ew" ~ "Opportunity Exposure",
#       variable == "rg_expo_ew" ~ "Regulatory Exposure",
#       variable == "ph_expo_ew" ~ "Physical Exposure",
#       variable == "ebit" ~ "EBIT",
#       variable == "ebit_at" ~ "EBIT/Assets",
#       variable == "total_lobby_quarter" ~ "Total Lobbying (\\$)"
#       )
#     )
# 
# # Plot balance table with geom points and error bars
# # facet grid by covariate and exposure
# balance_table |>
#   filter(Exposure == "Opportunity") |>
#   ggplot(aes(y=above_median, x=mean)) +
#   geom_vline(xintercept = 0, linetype="dashed", color = "grey") +
#   geom_point() +
#   geom_errorbar(aes(xmin=mean-sd, xmax=mean+sd), width=.2) +
#   facet_wrap(~variable, scales = "free") +
#   theme_bw() +
#   labs(y = "Mean of Covariates", x = "") +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(), text = element_text(size = 15))
#   
# 
# # pivot wider and remove sd
# balance_table |>
#   select(-sd) |>
#   pivot_wider(names_from = above_median, values_from = mean) |>
#   # do t.test
#   mutate(ttest = t.test(`Above Median`, `Below Median`, "two.sided")$p.value)
# 

# # calculate average climate change exposure by firm
# firm_mean <- df |>
#   group_by(isin) |>
#   summarise(ccexp = mean(ccexp, na.rm=T)) |>
#   left_join(df |> select(isin,sic),
#             by = "isin") |>
#   left_join(sic_desc, by = "sic") |>
#   filter(!is.na(industry))
# 
# # Calculate variance within each industry
# sic_var <- firm_mean |>
#   # group by industry
#   group_by(industry) |>
#   # get variance for each industry
#   summarise(sic_var=var(ccexp,na.rm=T)) |>
#   # delete all industries with NAs
#   filter(!is.na(industry)) |>
#   # round
#   mutate(sic_var = round(sic_var, digits = 2)) |>
#   arrange(sic_var)
# 
# # Only those industries with more than 50 firms
# sic_ct <- firm_mean |>
#   count(sic) #|>
#   filter(n>50)
# 
# # plot
# within_industry_variance <- firm_mean |>
#   filter(sic %in% sic_ct$sic) |>
#   mutate(industry=factor(industry, levels=sic_var$industry)) |>
#   ggplot(aes(x=industry, y=ccexp)) +
#   # geom_jitter(alpha=0.1) +
#   geom_boxplot(fill="red", outlier.alpha = .075) +
#   theme_bw() +
#   theme(
#     legend.position="none",
#     plot.title = element_text(size=11)
#   ) +
#   labs(x = "", y = "Climate Change Exposure") + 
#   coord_flip()
# 
# # save
# ggsave("../results/Figures/descriptives/within_industry_variance.pdf", within_industry_variance, width=11, height=8.5)
# 
# 
# # Variances over all exposure measures ------------------------------------
# 
# # get averages for all four exposure variables for each firm
# firm_means <- df |>
#   group_by(isin) |>
#   summarise(across(c(opexpo,rgexpo,phexpo), ~ mean(.x, na.rm=T))) |>
#   left_join(df |> select(isin,sic),
#             by = "isin") |>
#   left_join(sic_desc, by = "sic") |>
#   filter(!is.na(industry))
# 
# # calculate variance for each variable within industries
# exposure_var <- firm_means |> 
#   group_by(industry) |>
#   summarise(across(opexpo:phexpo, ~ var(.x, na.rm = TRUE))) |>
#   mutate(avg_var = rowMeans(across(opexpo:phexpo))) |>
#   arrange(-avg_var)
# 
# # Only top 15 variance industries
# sic_ct <- exposure_var |>
#   arrange(-avg_var) |>
#   head(15)
# 
# # Plot
# firm_means |>
#   filter(industry %in% sic_ct$industry) |>
#   mutate(industry=factor(industry, levels=sic_var$industry)) |>
#   pivot_longer(cols = 2:4, names_to = "Exposure", values_to = "value") |>
#   mutate(Variable = recode(Exposure,
#                            opexpo = "Opportunity",
#                            phexpo = "Physical",
#                            rgexpo = "Regulatory"),
#          round = round(value,digits = 2)) |>
#   ggplot(aes(y=value,x=industry)) +
#   facet_wrap(vars(Variable), nrow=1, scales = "free_x") +
#   # geom_violin() +
#   geom_boxplot(fill="darkgreen", outlier.alpha = .075) +
#   coord_flip() + 
#   # viridis::scale_color_viridis(discrete = T,end = .9) +
#   # scale_shape_manual(values=c(15:18)) + 
#   theme_bw() +
#   labs(y="Variance",x = "") +
#   theme(legend.position = "bottom",
#         axis.text.x=element_text(angle = 35, vjust=0.95,hjust=1))
# 
# # save
# ggsave("../results/Figures/descriptives/within_industry_variances.pdf", within_industry_variances, width=11, height=8.5)
# 
# ###
# 
# # top 10 only
# 
# # Plot
# within_industry_variances <- exposure_var |>
#   filter(industry %in% sic_ct$industry) |>
#   mutate(industry=factor(industry, levels=sic_var$industry)) |>
#   pivot_longer(cols = 2:6, names_to = "Variable", values_to = "value") |>
#   mutate(Variable = recode(Variable,
#                            opexpo = "Opportunity",
#                            phexpo = "Physical",
#                            rgexpo = "Regulatory"),
#          round = round(value,digits = 2)) |>
#   filter(!Variable %in% c("avg_var", "ccexp")) |>
#   group_by(industry) |>
#   mutate(sumv=sum(value)) |>
#   ungroup() |>
#   arrange(desc(sumv)) |>
#   head(30) |>
#   ggplot(aes(y=value,x=industry)) +
#   facet_wrap(vars(Variable), nrow=1, scales = "free_x") +
#   geom_point(size=3) +
#   coord_flip() + 
#   viridis::scale_color_viridis(discrete = T,end = .9) +
#   scale_shape_manual(values=c(15:18)) + 
#   theme_bw() +
#   labs(y="Variance",x = "") +
#   theme(legend.position = "bottom",
#         axis.text.x=element_text(angle = 35, vjust=0.95,hjust=1))
# 
# ggsave("../results/Figures/descriptives/within_industry_variances_TOP10.pdf", within_industry_variances, width=11, height=8.5)

# End