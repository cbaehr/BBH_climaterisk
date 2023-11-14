### Firms & Lobbying over time
### Descriptive

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor, cowplot)


# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}



# load data
df <- fread("data/03_final/lobbying_df_quarterly_REVISE_normal.csv")


## Reports over time -------------------------------------------------------

p1 <- df |>
  count(yearqtr, CLI_quarter) |>
  ggplot(aes(x = factor(yearqtr), y = n, group = factor(CLI_quarter))) +
  geom_line(aes(linetype = factor(CLI_quarter))) +
  theme_bw() +
  labs(x = "Year", y = "Number of Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_linetype_manual(name = "", 
                        labels = c("Other Issues", "CLI_quartermate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

df |> tabyl(yearqtr, CLI_quarter) |>
  mutate(share = `1` / (`1` + `0`) * 100)

## Save this
ggsave(plot = p1, "results/figures/descriptives/CLI_quartermate_lobbying_overtime.pdf", width = 9, height = 5.5)
# ggsave(plot = p1, "report/images/CLI_quartermate_lobbying_overtime.png", width = 9, height = 5.5)


# By issues
df |>
  count(yearqtr, ENV) |> filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
  bind_rows(
    df |> count(yearqtr, CAW) |> filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
    df |> count(yearqtr, ENG) |> filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
    df |> count(yearqtr, FUE) |> filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
    ) |>
  ggplot(aes(x = factor(yearqtr), y = n, group = factor(issue))) +
  geom_line(aes(color = factor(issue))) +
  theme_bw() +
  labs(x = "Year", y = "Number of Lobbying Reports") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_color_brewer(type =  "qual", name = "", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_lobbying_overtime_issues.pdf", width = 9, height = 5.5)
# ggsave("report/images/CLI_quartermate_lobbying_overtime_issues.png", width = 9, height = 5.5)





## Money over time ---------------------------------------------------------

p2 <- df |>
  group_by(yearqtr, CLI_quarter) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  ggplot(aes(x = factor(yearqtr), y = money, group = factor(CLI_quarter))) +
  geom_line(aes(linetype = factor(CLI_quarter))) +
  theme_bw() +
  labs(x = "Year", y = "Money Spent (Bio USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_linetype_manual(name = "", 
                        labels = c("Other Issues", "CLI_quartermate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

# Look at share
print(n= 100, df |>
  group_by(yearqtr, CLI_quarter) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  pivot_wider(names_from =  "CLI_quarter", values_from = "money") |>
  mutate(share = `1` / (`1` + `0`) * 100))

## Save this
ggsave(plot = p2, "results/figures/descriptives/CLI_quartermate_spending_overtime.pdf", width = 9, height = 5.5)
# ggsave(plot = p2, "report/images/CLI_quartermate_spending_overtime.png", width = 9, height = 5.5)


# By issues
df |>
  group_by(yearqtr, ENV) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  mutate(money = money / 10^9) |>
  filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
  bind_rows(
    df |>
      group_by(yearqtr, CAW) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^9) |>
      filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
    df |>
      group_by(yearqtr, ENG) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^9) |>
      filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
    df |>
      group_by(yearqtr, FUE) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^9) |>
      filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
    ) |>
  ggplot(aes(x = factor(yearqtr), y = money, group = factor(issue))) +
  geom_line(aes(color = factor(issue))) +
  theme_bw() +
  labs(x = "Year", y = "Money Spent (Bio USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", name = "", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_spending_overtime_issues.pdf", width = 9, height = 5.5)
# ggsave("report/images/CLI_quartermate_spending_overtime_issues.png", width = 9, height = 5.5)


# Combine
pcomb <- plot_grid(p1, p2, labels = "AUTO", nrow = 2)
ggsave2(plot = pcomb, "results/figures/descriptives/CLI_quartermate_lobbying_overtime_comb.pdf", width = 9, height = 9)



## Firms over time ---------------------------------------------------------

df |>
  group_by(yearqtr, CLI_quarter) |>
  summarise(firms = n_distinct(gvkey)) |>
  ggplot(aes(x = factor(yearqtr), y = firms, group = factor(CLI_quarter))) +
  geom_line(aes(linetype = factor(CLI_quarter))) +
  theme_bw() +
  labs(x = "Year", y = "No. of firms lobbying") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_linetype_manual(name = "", 
                        labels = c("Other Issues", "CLI_quartermate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_firms_overtime.pdf", width = 9, height = 5.5)
# ggsave("report/images/CLI_quartermate_firms_overtime.png", width = 9, height = 5.5)


# By issues
df |>
  group_by(yearqtr, ENV) |>
  summarise(firms = n_distinct(gvkey)) |>
  filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
  bind_rows(
    df |>
      group_by(yearqtr, CAW) |>
      summarise(firms = n_distinct(gvkey)) |>
      filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
    df |>
      group_by(yearqtr, ENG) |>
      summarise(firms = n_distinct(gvkey)) |>
      filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
    df |>
      group_by(yearqtr, FUE) |>
      summarise(firms = n_distinct(gvkey)) |>
      filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
  ) |>
  ggplot(aes(x = factor(yearqtr), y = firms, group = factor(issue))) +
  geom_line(aes(color = factor(issue))) +
  theme_bw() +
  labs(x = "Year", y = "No. of firms lobbying") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_color_brewer(type =  "qual", name = "", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_firms_overtime_issues.pdf", width = 9, height = 5.5)
# ggsave("report/images/CLI_quartermate_firms_overtime_issues.png", width = 9, height = 5.5)




# Differentiate regarding CLI_quartermate attention -------------------------------


# Get within industry-year categorical & binary variables indicating how much 
# CLI_quartermate change attention firms experienced in comparison to other firms
# in the same industry
define_CLI_quartermate_attention <- function(df, variables) {
  for (variable in variables) {
    df <- df %>%
      filter(!is.na(industry)) %>%
      group_by(yearqtr, industry) %>%
      mutate(
        !!paste0(variable, "_yqindustry_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_median") := median(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_yqindustry_mean") := mean(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_yqindustry_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_var") := var(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_disc") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_yqindustry_q1")) ~ "q1",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q1")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_median")) ~ "q2",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_median")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_q3")) ~ "q3",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q3")) ~ "q4"
        ),
        !!paste0(variable, "_yqindustry_above_mean") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yqindustry_mean")), 1, 0),
        !!paste0(variable, "_yqindustry_above_75") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q3")), 1, 0)
      ) %>%
      ungroup()
  }
  
  return(df)
}

# Create variables
df2 <- define_CLI_quartermate_attention(df, c("ccexp_q", "opexpo_q", "rgexpo_q", "phexpo_q"))


# inspect <- df |>
#   select(gvkey, sic, yearqtr, ccexp, ccexp_yqsic_q1:ccexp_above_75)


df_c <- df2 |> filter(CLI_quarter == 1)

## Reports over time -------------------------------------------------------

df_c |>
  count(yearqtr, opexpo_q_disc) |> rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_c |> count(yearqtr, rgexpo_q_disc) |> rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df_c |> count(yearqtr, phexpo_q_disc) |> rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
  ) |>
  ggplot(aes(x = factor(yearqtr), y = n, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_wrap(~measure, ncol = 3) +
  labs(x = "Year", y = "Number of Lobbying Reports", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  # scale_linetype_discrete(name = "") +
  theme(legend.position = "bottom", text = element_text(size = 15))

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_lobbying_overtime_variation.pdf", width = 10, height = 5.5)
# ggsave("report/images/CLI_quartermate_lobbying_overtime_variation.png", width = 10, height = 5.5)



## Money over time ---------------------------------------------------------

df_c |>
  group_by(yearqtr, opexpo_q_disc) |>
  summarise(money = sum(CLI_quarter_dollars, na.rm = TRUE)) |>
  rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_c |> group_by(yearqtr, rgexpo_q_disc) |>
      summarise(money = sum(CLI_quarter_dollars, na.rm = TRUE)) |>
      rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df_c |> group_by(yearqtr, phexpo_q_disc) |>
      summarise(money = sum(CLI_quarter_dollars, na.rm = TRUE)) |>
      rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
    ) |>
  ungroup() |>
  mutate(
    quantile = case_when(
      quantile == "q1" ~ 1,
      quantile == "q2" ~ 2,
      quantile == "q3" ~ 3,
      quantile == "q4" ~ 4
    )
  ) |>
  ggplot(aes(x = factor(yearqtr), y = money, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile), linetype = factor(quantile)), linewidth = 1.25) +
  theme_bw() +
  facet_wrap(~measure, ncol = 3) +
  labs(x = "Year", y = "Money Spent (Mio USD)", color = "Firm Quantile", linetype = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 8)], labels = function(x) str_sub(x, end = -3)) +
  # scale_color_viridis_d(option = "D", end = 0.9) +
  scale_linetype_manual(values = c("dotted", "twodash", "dashed", "solid")) +
  # scale_color_brewer(type = "qual", palette = "Set1") +
  scale_color_manual(values = c("darkgrey", "blue", "red", "black")) +
  theme(legend.position = "bottom", 
        text = element_text(size = 17),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_spending_overtime_variation.pdf", width = 10, height = 7)
# ggsave("report/images/CLI_quartermate_spending_overtime_variation.png", width = 10, height = 5.5)


## Firms over time ---------------------------------------------------------

df_c |>
  group_by(yearqtr, opexpo_q_disc) |>
  summarise(firms = n_distinct(gvkey)) |>
  rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_c |> group_by(yearqtr, rgexpo_q_disc) |>
      summarise(firms = n_distinct(gvkey)) |>
      rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df_c |> group_by(yearqtr, phexpo_q_disc) |>
      summarise(firms = n_distinct(gvkey)) |>
      rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  ggplot(aes(x = factor(yearqtr), y = firms, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_wrap(~measure, ncol = 3) +
  labs(x = "Year", y = "No. of firms lobbying", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  theme(legend.position = "bottom", text = element_text(size = 15))

## Save this
ggsave("results/figures/descriptives/CLI_quartermate_firms_overtime_variation.pdf", width = 10, height = 5.5)
# ggsave("report/images/CLI_quartermate_firms_overtime_variation.png", width = 10, height = 5.5)


### END