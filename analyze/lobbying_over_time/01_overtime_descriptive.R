### Firms & Lobbying over time
### Descriptive

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor)


# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}



# load data
df <- fread("data/lobbying_df_wide.csv")



# Climate change lobbying over time ---------------------------------------

# Binary variable: climate issues
df <- df |>
  mutate(CLI = ifelse(ENV == 1 |
                        CAW == 1 |
                        ENG == 1 |
                        FUE == 1,
                      1, 0))

df <- df |>
  mutate(year_quarter = paste0(year,"_",report_quarter_code))

df |> tabyl(CLI)
df |> tabyl(CLI, year_quarter)



## Reports over time -------------------------------------------------------

df |>
  count(year_quarter, CLI) |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(CLI))) +
  geom_line(aes(linetype = factor(CLI))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "Number of Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_lobbying_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_lobbying_overtime.png", width = 9, height = 5.5)


# By issues
df |>
  count(year_quarter, ENV) |> filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
  bind_rows(
    df |> count(year_quarter, CAW) |> filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
    df |> count(year_quarter, ENG) |> filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
    df |> count(year_quarter, FUE) |> filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
    ) |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(issue))) +
  geom_line(aes(color = factor(issue))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "Number of Lobbying Reports") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", name = "", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_lobbying_overtime_issues.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_lobbying_overtime_issues.png", width = 9, height = 5.5)





## Money over time ---------------------------------------------------------

df |>
  group_by(year_quarter, CLI) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  mutate(money = money / 10^9) |>
  ggplot(aes(x = factor(year_quarter), y = money, group = factor(CLI))) +
  geom_line(aes(linetype = factor(CLI))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "Money Spent (Bio USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_spending_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_spending_overtime.png", width = 9, height = 5.5)


# By issues
df |>
  group_by(year_quarter, ENV) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  mutate(money = money / 10^9) |>
  filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
  bind_rows(
    df |>
      group_by(year_quarter, CAW) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^9) |>
      filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
    df |>
      group_by(year_quarter, ENG) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^9) |>
      filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
    df |>
      group_by(year_quarter, FUE) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^9) |>
      filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
    ) |>
  ggplot(aes(x = factor(year_quarter), y = money, group = factor(issue))) +
  geom_line(aes(color = factor(issue))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "Money Spent (Bio USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", name = "", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_spending_overtime_issues.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_spending_overtime_issues.png", width = 9, height = 5.5)




## Firms over time ---------------------------------------------------------

df |>
  group_by(year_quarter, CLI) |>
  summarise(firms = n_distinct(gvkey)) |>
  ggplot(aes(x = factor(year_quarter), y = firms, group = factor(CLI))) +
  geom_line(aes(linetype = factor(CLI))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "No. of firms lobbying") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_firms_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_firms_overtime.png", width = 9, height = 5.5)


# By issues
df |>
  group_by(year_quarter, ENV) |>
  summarise(firms = n_distinct(gvkey)) |>
  filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
  bind_rows(
    df |>
      group_by(year_quarter, CAW) |>
      summarise(firms = n_distinct(gvkey)) |>
      filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
    df |>
      group_by(year_quarter, ENG) |>
      summarise(firms = n_distinct(gvkey)) |>
      filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
    df |>
      group_by(year_quarter, FUE) |>
      summarise(firms = n_distinct(gvkey)) |>
      filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
  ) |>
  ggplot(aes(x = factor(year_quarter), y = firms, group = factor(issue))) +
  geom_line(aes(color = factor(issue))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "No. of firms lobbying") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", name = "", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_firms_overtime_issues.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_firms_overtime_issues.png", width = 9, height = 5.5)




# Differentiate regarding climate attention -------------------------------


# Get within industry-year categorical & binary variables indicating how much 
# climate change attention firms experienced in comparison to other firms
# in the same industry
define_climate_attention <- function(df, variables) {
  for (variable in variables) {
    df <- df %>%
      filter(!is.na(sic)) %>%
      group_by(year_quarter, sic) %>%
      mutate(
        !!paste0(variable, "_yqsic_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
        !!paste0(variable, "_yqsic_median") := median(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_yqsic_mean") := mean(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_yqsic_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
        !!paste0(variable, "_yqsic_var") := var(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_disc") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_yqsic_q1")) ~ "q1",
          .data[[variable]] >= !!sym(paste0(variable, "_yqsic_q1")) & .data[[variable]] < !!sym(paste0(variable, "_yqsic_median")) ~ "q2",
          .data[[variable]] >= !!sym(paste0(variable, "_yqsic_median")) & .data[[variable]] < !!sym(paste0(variable, "_yqsic_q3")) ~ "q3",
          .data[[variable]] >= !!sym(paste0(variable, "_yqsic_q3")) ~ "q4"
        ),
        !!paste0(variable, "_yqsic_above_mean") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yqsic_mean")), 1, 0),
        !!paste0(variable, "_yqsic_above_75") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yqsic_q3")), 1, 0)
      ) %>%
      ungroup()
  }
  
  return(df)
}

# Create variables
df <- define_climate_attention(df, c("ccexp", "opexpo_q", "rgexpo_q", "phexpo_q"))


# inspect <- df |>
#   select(gvkey, sic, year_quarter, ccexp, ccexp_yqsic_q1:ccexp_above_75)


df_c <- df |> filter(CLI == 1)

## Reports over time -------------------------------------------------------

df_c |>
  count(year_quarter, ccexp_disc) |> rename(quantile = ccexp_disc) |> mutate(measure = "Attention") |>
  bind_rows(
    df_c |> count(year_quarter, opexpo_q_disc) |> rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity"),
    df_c |> count(year_quarter, rgexpo_q_disc) |> rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df_c |> count(year_quarter, phexpo_q_disc) |> rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
  ) |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_wrap(~measure, ncol = 2) +
  labs(x = "Year Quarter", y = "Number of Lobbying Reports", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  # scale_linetype_discrete(name = "") +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_lobbying_overtime_variation.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_lobbying_overtime_variation.png", width = 9, height = 5.5)



## Money over time ---------------------------------------------------------

df_c |>
  group_by(year_quarter, ccexp_disc) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  mutate(money = money / 10^7) |>
  rename(quantile = ccexp_disc) |> mutate(measure = "Attention") |>
  bind_rows(
    df_c |> group_by(year_quarter, opexpo_q_disc) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity"),
    df_c |> group_by(year_quarter, rgexpo_q_disc) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df_c |> group_by(year_quarter, phexpo_q_disc) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
    ) |>
  ungroup() |>
  ggplot(aes(x = factor(year_quarter), y = money, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_wrap(~measure, ncol = 2) +
  labs(x = "Year Quarter", y = "Money Spent (Mio USD)", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_spending_overtime_variation.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_spending_overtime_variation.png", width = 9, height = 5.5)


## Firms over time ---------------------------------------------------------

df_c |>
  group_by(year_quarter, ccexp_disc) |>
  summarise(firms = n_distinct(gvkey)) |>
  rename(quantile = ccexp_disc) |> mutate(measure = "Attention") |>
  bind_rows(
    df_c |> group_by(year_quarter, opexpo_q_disc) |>
      summarise(firms = n_distinct(gvkey)) |>
      rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity"),
    df_c |> group_by(year_quarter, rgexpo_q_disc) |>
      summarise(firms = n_distinct(gvkey)) |>
      rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df_c |> group_by(year_quarter, phexpo_q_disc) |>
      summarise(firms = n_distinct(gvkey)) |>
      rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  ggplot(aes(x = factor(year_quarter), y = firms, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_wrap(~measure, ncol = 2) +
  labs(x = "Year Quarter", y = "No. of firms lobbying", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_firms_overtime_variation.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_firms_overtime_variation.png", width = 9, height = 5.5)



### END