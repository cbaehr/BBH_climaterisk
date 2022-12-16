### Within industry variation in climate change exposure


rm(list=ls())

# load packages
library(data.table)
library(tidyverse)

# set working directory
setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")
setwd("~/Dropbox (Princeton)/BBH/BBH1/data/")

# load data
df <- fread("df_year_fb.csv")

# Within industry variation in exposure -----------------------------------

# read sic 2 digit names
sic_desc <- fread("Misc/sic_2_digit_codes.csv")

# calculate average climate change exposure by firm
firm_mean <- df |>
  group_by(isin) |>
  summarise(ccexp = mean(ccexp, na.rm=T)) |>
  left_join(df |> select(isin,sic),
            by = "isin") |>
  left_join(sic_desc, by = "sic") |>
  filter(!is.na(industry))

# Calculate variance within each industry
sic_var <- firm_mean |>
  # group by industry
  group_by(industry) |>
  # get variance for each industry
  summarise(sic_var=var(ccexp,na.rm=T)) |>
  # delete all industries with NAs
  filter(!is.na(industry)) |>
  # round
  mutate(sic_var = round(sic_var, digits = 2)) |>
  arrange(sic_var)

# Only those industries with more than 50 firms
sic_ct <- firm_mean |>
  count(sic) |>
  filter(n>50)

# plot
within_industry_variance <- firm_mean |>
  filter(sic %in% sic_ct$sic) |>
  mutate(industry=factor(industry, levels=sic_var$industry)) |>
  ggplot(aes(x=industry, y=ccexp)) +
  geom_jitter(alpha=0.1) +
  geom_boxplot(fill="red", outlier.shape = NA) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  labs(x = "", y = "Climate Change Attention") + 
  coord_flip()

# save
ggsave("../Figures/within_industry_variance.pdf", within_industry_variance, width=11, height=8.5)





# Variances over all exposure measures ------------------------------------

# get averages for all four exposure variables for each firm
firm_means <- df |>
  group_by(isin) |>
  summarise(across(c(ccexp,opexpo,rgexpo,phexpo), ~ mean(.x, na.rm=T))) |>
  left_join(df |> select(isin,sic),
            by = "isin") |>
  left_join(sic_desc, by = "sic") |>
  filter(!is.na(industry))

# calculate variance for each variable within industries
exposure_var <-firm_means |> 
  group_by(industry) |>
  summarise(across(ccexp:phexpo, ~ var(.x, na.rm = TRUE))) |>
  mutate(avg_var = rowMeans(across(ccexp:phexpo,na.rm=T))) |>
  arrange(-avg_var)

# Only those industries with more than 50 firms
sic_ct <- firm_mean |>
  count(industry) |>
  filter(n>50)

# Plot
within_industry_variances <- exposure_var |>
  filter(industry %in% sic_ct$industry) |>
  mutate(industry=factor(industry, levels=sic_var$industry)) |>
  pivot_longer(cols = 2:6, names_to = "Variable", values_to = "value") |>
  mutate(Variable = recode(Variable,
                           ccexp = "Attention",
                           opexpo = "Opportunity",
                           phexpo = "Physical",
                           rgexpo = "Regulatory"),
         round = round(value,digits = 2)) |>
  filter(Variable != "avg_var") |>
  ggplot(aes(y=value,x=industry,color=Variable, shape = Variable)) +
  geom_pointrange(aes(ymin = (value-.5),
                      ymax = (value+.5))) +
  coord_flip() + 
  viridis::scale_color_viridis(discrete = T,end = .9) +
  scale_shape_manual(values=c(15:18)) + 
  theme_bw() +
  labs(y="Variance",x = "") +
  theme(legend.position = "bottom")

# save
ggsave("../results/Figures/within_industry_variances.pdf", within_industry_variances, width=11, height=8.5)


# End