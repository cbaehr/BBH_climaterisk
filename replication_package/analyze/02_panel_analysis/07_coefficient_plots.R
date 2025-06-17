### Firms & Lobbying
### Analysis

### Quarterly

# load packages
pacman::p_load(tidyverse, dplyr, ggplot2, broom)

# set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}

## Coefficient Plot for Main Occurrence and Amount Effects --------------------

load("data/03_final/climate_ols_qrt_bycomponent_MODELS_REVISION_NEW.RData")
m1 <- models[[5]]
m3 <- models[[3]]

load("data/03_final/climate_ols_qrt_bycomponent_amount_MODELS_REVISION_NEW.RData")
m2 <- models[[5]]
m4 <- models[[3]]

# Tidy the model output
m1_df <- broom::tidy(m1, conf.int = T)
m1_df <- m1_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m1_df$term <- factor(m1_df$term, levels = rev(m1_df$term))
m1_df$model <- "Occurrence"
m1_df$fes <- "Industry-by-year fixed effects"

m3_df <- broom::tidy(m3, conf.int = T)
m3_df <- m3_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m3_df$term <- factor(m3_df$term, levels = rev(m3_df$term))
m3_df$model <- "Occurrence"
m3_df$fes <- "Year fixed effects"

m2_df <- broom::tidy(m2, conf.int = T)
m2_df <- m2_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m2_df$term <- factor(m2_df$term, levels = rev(m2_df$term))
m2_df$model <- "Amount"
m2_df$fes <- "Industry-by-year fixed effects"

m4_df <- broom::tidy(m4, conf.int = T)
m4_df <- m4_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m4_df$term <- factor(m4_df$term, levels = rev(m4_df$term))
m4_df$model <- "Amount"
m4_df$fes <- "Year fixed effects"


mods_df <- rbind(m3_df, m1_df, m4_df, m2_df)
mods_df$model <- factor(mods_df$model, levels = c("Occurrence", "Amount"))

mods_df <- mods_df %>%
  mutate(p.value = 2 * (1-pnorm(abs(estimate / std.error))),
         #color = ifelse(p.value < 0.05, "black", "darkgrey"),
         color = ifelse(p.value < 0.05, "black", "black"),
         conf.low95 = estimate - 1.96 * std.error,
         conf.high95 = estimate + 1.96 * std.error,
         conf.low90 = estimate - 1.645 * std.error,
         conf.high90 = estimate + 1.645 * std.error)


# Create the coefficient plot
ggplot(mods_df %>% mutate(fes = factor(fes, levels=c("Year fixed effects", "Industry-by-year fixed effects"))), 
       aes(y = estimate, x = term, color=fes)) +
  facet_wrap(~model, scales="free_x") +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), 
                position = position_dodge(width = 0.4), width = 0, linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90), 
                position = position_dodge(width = 0.4), width = 0, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) +
  geom_point(position = position_dodge(width = 0.4), shape = 21, fill = "white", size = 2) +
  scale_color_manual(values = c("Industry-by-year fixed effects" = "black", "Year fixed effects" = "gray70"),
                     breaks = c("Industry-by-year fixed effects", "Year fixed effects")) +
  labs(y = "Coefficient", x="Exposure") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 15),
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.title.y = element_blank()
  ) + 
  coord_flip()

ggsave("results/figures/regressions/coefplot_byexposure_qrt_REVISE.pdf", width = 7, height = 3.5)


## Coefficient Plot for Main Target Effects -----------------------------------

load("data/03_final/climate_ols_qrt_bycomponent_target_MODELS_REVISION_NEW.RData")
m1 <- models$EPA
m1_df <- broom::tidy(m1, conf.int = T)
m1_df <- m1_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m1_df$agency <- "Environmental Protection Agency"
m1_df$model <- "Occurrence"

m2 <- models$DOE
m2_df <- broom::tidy(m2, conf.int = T)
m2_df <- m2_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m2_df$agency <- "Department of Energy"
m2_df$model <- "Occurrence"

# m3 <- models$CONG
# m3_df <- broom::tidy(m3, conf.int = T)
# m3_df <- m3_df %>%
#   filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
#   mutate(term = c("Opportunity", "Regulatory", "Physical"))
# m3_df$agency <- "CONG"
# m3_df$model <- "Occurrence"

load("data/03_final/climate_ols_qrt_bycomponent_target_amount_MODELS_REVISION_NEW.RData")
m4 <- models$EPA    
m4_df <- broom::tidy(m4, conf.int = T)
m4_df <- m4_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m4_df$agency <- "Environmental Protection Agency"
m4_df$model <- "Amount"

m5 <- models$DOE
m5_df <- broom::tidy(m5, conf.int = T)
m5_df <- m5_df %>%
  filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
  mutate(term = c("Opportunity", "Regulatory", "Physical"))
m5_df$agency <- "Department of Energy"
m5_df$model <- "Amount"

# m6 <- models$CONG
# m6_df <- broom::tidy(m6, conf.int = T)
# m6_df <- m6_df %>%
#   filter(term %in% c("op_expo_ew", "rg_expo_ew", "ph_expo_ew")) %>%
#   mutate(term = c("Opportunity", "Regulatory", "Physical"))
# m6_df$agency <- "CONG"
# m6_df$model <- "Amount"

#mods_df <- rbind(m1_df, m2_df, m3_df, m4_df, m5_df, m6_df)
mods_df <- rbind(m1_df, m2_df, m4_df, m5_df)
mods_df$term <- factor(mods_df$term, levels = c("Physical", "Regulatory", "Opportunity"))
mods_df$model <- factor(mods_df$model, levels = c("Occurrence", "Amount"))

mods_df <- mods_df %>%
  mutate(p.value = 2 * (1-pnorm(abs(estimate / std.error))),
         conf.low95 = estimate - 1.96 * std.error,
         conf.high95 = estimate + 1.96 * std.error,
         conf.low90 = estimate - 1.645 * std.error,
         conf.high90 = estimate + 1.645 * std.error)

# Create the coefficient plot
ggplot(mods_df, aes(y = estimate, x = term, color=agency)) +
  facet_wrap(~model, scales="free_x") +
  geom_errorbar(aes(ymin = conf.low95, ymax = conf.high95), 
                position = position_dodge(width = 0.4), width = 0, linewidth = .5) +
  geom_errorbar(aes(ymin = conf.low90, ymax = conf.high90), 
                position = position_dodge(width = 0.4), width = 0, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) +
  geom_point(position = position_dodge(width = 0.4), shape = 21, fill = "white", size = 2) +
  scale_color_manual(values = c("Environmental Protection Agency" = "black", "Department of Energy" = "gray70"),
                     labels = c("Environmental Protection Agency"="Environmental Protection Agency", "Department of Energy"="Department of Energy"),
                     breaks = c("Environmental Protection Agency", "Department of Energy")) +
  labs(y = "Coefficient", x="Exposure") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(size = 15),
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank()
  ) + 
  coord_flip()

ggsave("results/figures/regressions/coefplot_byexposure_qrt_target_REVISE.pdf", width = 7, height = 3.5)


