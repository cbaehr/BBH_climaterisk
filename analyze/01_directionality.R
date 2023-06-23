### Firms & Climate Lobbying
### Directionality



rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor, cowplot)


# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}



df <- fread("data/lobbying_df_w_directionality.csv") 

df <- df |> 
  mutate(
    year_quarter = paste0(year, "_", report_quarter_code),
    amount_num = as.numeric(amount_num)
    )


# Plot climate lobbying directionality over time --------------------------

# By quarter
p1 <- df |>
  group_by(year_quarter) |>
  summarize(n_pro_climate = sum(pro_CLI, na.rm = TRUE),
            n_contra_climate = sum(contra_CLI, na.rm = TRUE)) |>
  pivot_longer(cols = starts_with("n_"),
               names_to = "type",
               names_prefix = "n_",
               values_to = "n") |>
  mutate(year = substr(year_quarter,1,4)) |>
  filter(year < 2020) |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year", y = "Number of Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_linetype_manual(name = "", 
                        labels = c("Contra", "Pro"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")


## Save this
ggsave(plot = p1, "results/Figures/descriptives/climate_lobbying_directionality.pdf", width = 9, height = 5.5)
ggsave(plot = p1, "report/images/climate_lobbying_directionality.png", width = 9, height = 5.5)




# Plot money with climate lobbying directionality over time --------------------------

# By quarter
p2 <- df |>
  group_by(year_quarter, pro_CLI) |>
  summarize(n = sum(amount_num, na.rm = TRUE)) |>
  mutate(n = n / 10^7) |>
  filter(pro_CLI==1) |> select(-pro_CLI) |> mutate(type = "Pro") |>
  bind_rows(
    df |>
      group_by(year_quarter, contra_CLI) |>
      summarize(n = sum(amount_num, na.rm = TRUE)) |>
      mutate(n = n / 10^7) |>
      filter(contra_CLI==1) |> select(-contra_CLI) |> mutate(type = "Contra")
  ) |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year", y = "Money Spent (Mio USD)", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_linetype_manual(name = "", 
                        labels = c("Contra", "Pro"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")


## Save this
ggsave(plot = p2, "results/Figures/descriptives/climate_lobbying_money_directionality.pdf", width = 9, height = 5.5)
ggsave(plot = p2, "report/images/climate_lobbying_money_directionality.png", width = 9, height = 5.5)




## Next to each other ------------------------------------------------------

pcomb <- plot_grid(p1, p2, labels = "AUTO", nrow = 2)

ggsave2(plot = pcomb, "results/Figures/descriptives/climate_lobbying_directionality_comb.pdf", width = 9, height = 9)


# Quarter: Differentiate by climate attention --------------------------------------

# Get within industry-year categorical & binary variables indicating how much 
# climate change attention firms experienced in comparison to other firms
# in the same industry
define_climate_attention_q <- function(df, variables) {
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
df2 <- define_climate_attention_q(df, c("ccexp_q", "opexpo_q", "rgexpo_q", "phexpo_q"))


## Money over time ---------------------------------------------------------


df2 |>
  group_by(year_quarter, ccexp_q_disc, direction_CLI) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  mutate(money = money / 10^7) |>
  rename(quantile = ccexp_q_disc) |> mutate(measure = "Attention") |>
  bind_rows(
    df2 |> group_by(year_quarter, opexpo_q_disc, direction_CLI) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity"),
    df2 |> group_by(year_quarter, rgexpo_q_disc, direction_CLI) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
    df2 |> group_by(year_quarter, phexpo_q_disc, direction_CLI) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  filter(!direction_CLI %in% c("None", "", "Both")) |>
  ggplot(aes(x = factor(year_quarter), y = money, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_grid(measure ~direction_CLI) +
  labs(x = "Year", y = "Money Spent (Mio USD)", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_spending_direction_overtime_variation.pdf", width = 9, height = 9)
ggsave("report/images/climate_spending_direction_overtime_variation.png", width = 9, height = 9)



# Year: Differentiate by climate attention --------------------------------------

# Get within industry-year categorical & binary variables indicating how much 
# climate change attention firms experienced in comparison to other firms
# in the same industry
define_climate_attention_y <- function(df, variables) {
  for (variable in variables) {
    df <- df %>%
      filter(!is.na(sic)) %>%
      group_by(year, sic) %>%
      mutate(
        !!paste0(variable, "_ysic_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
        !!paste0(variable, "_ysic_median") := median(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_ysic_mean") := mean(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_ysic_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
        !!paste0(variable, "_ysic_var") := var(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_disc") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_ysic_q1")) ~ "q1",
          .data[[variable]] >= !!sym(paste0(variable, "_ysic_q1")) & .data[[variable]] < !!sym(paste0(variable, "_ysic_median")) ~ "q2",
          .data[[variable]] >= !!sym(paste0(variable, "_ysic_median")) & .data[[variable]] < !!sym(paste0(variable, "_ysic_q3")) ~ "q3",
          .data[[variable]] >= !!sym(paste0(variable, "_ysic_q3")) ~ "q4"
        ),
        !!paste0(variable, "_ysic_above_mean") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_ysic_mean")), 1, 0),
        !!paste0(variable, "_ysic_above_75") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_ysic_q3")), 1, 0)
      ) %>%
      ungroup()
  }
  
  return(df)
}

# Create variables
df3 <- define_climate_attention_y(df2, c("ccexp_y", "opexpo_y", "rgexpo_y", "phexpo_y"))


## Money over time ---------------------------------------------------------


df3 |>
  group_by(year, ccexp_y_disc, direction_CLI) |>
  summarise(money = sum(amount_num, na.rm = TRUE)) |>
  mutate(money = money / 10^7) |>
  rename(quantile = ccexp_y_disc) |> mutate(measure = "Attention") |>
  bind_rows(
    df3 |> group_by(year, opexpo_y_disc, direction_CLI) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = opexpo_y_disc) |> mutate(measure = "Opportunity"),
    df3 |> group_by(year, rgexpo_y_disc, direction_CLI) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = rgexpo_y_disc) |> mutate(measure = "Regulatory"),
    df3 |> group_by(year, phexpo_y_disc, direction_CLI) |>
      summarise(money = sum(amount_num, na.rm = TRUE)) |>
      mutate(money = money / 10^7) |>
      rename(quantile = phexpo_y_disc) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  filter(!direction_CLI %in% c("None", "", "Both")) |>
  ggplot(aes(x = factor(year), y = money, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_bw() +
  facet_grid(measure ~direction_CLI) +
  labs(x = "Year", y = "Money Spent (Mio USD)", color = "Firm Quantile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  theme(legend.position = "bottom")

# I like the quarterly plot better

### END