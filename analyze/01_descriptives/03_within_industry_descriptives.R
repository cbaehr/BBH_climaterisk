### Within industry variation in climate change exposure


rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse)

# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/data/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/data/")}


# load data
df <- fread("02_processed/exposure_year.csv", colClasses = c("sic"="character"))
df$sic <- as.numeric(substr(df$sic, 1, 2))

# Within industry variation in exposure -----------------------------------


# merge
df <- df |> 
  # filter out empty bvd_sector
  filter(bvd_sector != "") |> 
  # select variables we need
  select(isin, year, bvd_sector, opexpo, rgexpo, phexpo) |>
  pivot_longer(cols = opexpo:phexpo, names_to = "Exposure", values_to = "Value") |>
  mutate(Exposure = recode(Exposure,
                           opexpo = "Opportunity",
                           phexpo = "Physical",
                           rgexpo = "Regulatory"),
         round = round(Value,digits = 2))

# calculate number of firms per industry
industry_n_firms <- df |>
  group_by(bvd_sector) |>
  count()

# Calculate variance within each industry
industry_var <- df |>
  # group by industry
  group_by(bvd_sector, Exposure) |>
  # get variance for each industry
  summarise(Variance = var(Value, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(bvd_sector)) |>
  arrange(desc(Variance))

# Calculate mean variance across exposure variables
industry_var_levels <- industry_var |>
  group_by(bvd_sector) |>
  summarise(mean = mean(Variance, na.rm = TRUE)) |>
  ungroup() |>
  filter(!is.na(bvd_sector)) |>
  arrange((mean)) |>
  tail(15)


# Violin Plot
df |>
  filter(bvd_sector %in% industry_var_levels$bvd_sector) |>
  mutate(bvd_sector = factor(bvd_sector, levels = industry_var_levels$bvd_sector)) |>
  ggplot(aes(y = Value, x = bvd_sector)) +
  facet_wrap(vars(Exposure), nrow = 1, scales = "free_x") +
  geom_violin(trim = T,
              fill = "darkgrey",
              scale = "width") +
  coord_flip() +
  theme_bw() +
  labs(y = "Distribution of Exposure Variables", x = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 12))


ggsave("../results/Figures/descriptives/within_industry_variances_TOP15_violin.pdf", width=10, height=8)

# BoxPlot

df |>
  filter(bvd_sector %in% industry_var_levels$bvd_sector) |>
  mutate(
    bvd_sector=factor(bvd_sector, levels=industry_var_levels$bvd_sector),
    bvd_sector = fct_relabel(bvd_sector, ~str_wrap(., width = 40))
    ) |>
  ggplot(aes(y=Value,x=bvd_sector)) +
  facet_wrap(vars(Exposure), nrow=1, scales = "free_x") +
  geom_boxplot(fill="darkgrey"
               ,na.rm = TRUE
               , outlier.alpha = .075
               , varwidth = TRUE
               ) +
  coord_flip() + 
  theme_bw() +
  labs(y = "Distribution of Exposure Variables", x = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 15))


ggsave("../results/Figures/descriptives/within_industry_variances_TOP15_boxplot.pdf", width=10, height=8)



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