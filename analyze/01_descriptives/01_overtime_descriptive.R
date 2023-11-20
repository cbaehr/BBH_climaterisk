### Firms & Lobbying over time
### Descriptive

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor, cowplot, haschaR)


# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}



# load data
df <- read_rds("data/03_final/lobbying_df_quarterly_REVISE_normal.rds")

names(df)

glimpse(df)


## Reports over time -------------------------------------------------------

df |>
  count(yearqtr, CLI_quarter) |>
  filter(CLI_quarter == 1) |>
  select(yearqtr, CLI = n) |>
  left_join(
    df |>
      count(yearqtr, nonCLI_quarter)  |>
      filter(nonCLI_quarter == 1) |>
      select(yearqtr, Other = n),
    by = "yearqtr"
    ) |>
  pivot_longer(CLI:Other, names_to = "Issue", values_to = "n") |>
  ggplot(aes(x = factor(yearqtr), y = n, group = factor(Issue))) +
  geom_line(aes(linetype = factor(Issue))) +
  theme_hanno() +
  labs(x = "Year", y = "Number of Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_linetype_manual(name = "", 
                        labels = c("Climate Issues", "Other Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/figures/descriptives/climate_lobbying_overtime.pdf", width = 9, height = 5.5)
# ggsave(plot = p1, "report/images/CLI_quartermate_lobbying_overtime.png", width = 9, height = 5.5)


# # By issues
# df |>
#   count(yearqtr, ENV) |> filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
#   bind_rows(
#     df |> count(yearqtr, CAW) |> filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
#     df |> count(yearqtr, ENG) |> filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
#     df |> count(yearqtr, FUE) |> filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
#     ) |>
#   ggplot(aes(x = factor(yearqtr), y = n, group = factor(issue))) +
#   geom_line(aes(color = factor(issue))) +
#   theme_hanno() +
#   labs(x = "Year", y = "Number of Lobbying Reports") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
#                    labels = function(x) str_sub(x, end = -3)) +
#   expand_limits(y = 0) +
#   scale_color_brewer(type =  "qual", name = "", palette = 2) +
#   theme(legend.position = "bottom")
# 
# ## Save this
# ggsave("results/figures/descriptives/CLI_quartermate_lobbying_overtime_issues.pdf", width = 9, height = 5.5)
# # ggsave("report/images/CLI_quartermate_lobbying_overtime_issues.png", width = 9, height = 5.5)


## Money over time ---------------------------------------------------------


### By quarter --------------------------------------------------------------
p2 <- df |>
  group_by(yearqtr) |>
  summarise(CLI = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
  left_join(
    df |>
      group_by(yearqtr) |>
      summarise(Total = sum(total_lobby_quarter, na.rm = TRUE) / 10^6),
    by = "yearqtr"
  ) |>
  mutate(
    Other = Total - CLI,
    Share = CLI / Total
    )

p2 |>
  select(-c(Total, Share)) |>
  pivot_longer(CLI:Other, names_to = "Issue", values_to = "money") |>
  filter(money != 0) |>
  ggplot(aes(x = factor(yearqtr), y = money, group = factor(Issue))) +
  geom_line(aes(linetype = factor(Issue))) +
  theme_hanno() +
  labs(x = "Year", y = "Money Spent (Mio USD)") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  expand_limits(y = 0) +
  scale_linetype_manual(name = "", 
                        labels = c("Climate Issues", "Other Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")


## Save this
ggsave("results/figures/descriptives/climate_spending_overtime.pdf", width = 9, height = 5.5)
# ggsave(plot = p2, "report/images/CLI_quartermate_spending_overtime.png", width = 9, height = 5.5)



# ### By year -----------------------------------------------------------------
# 
# p3 <- df |>
#   group_by(year) |>
#   summarise(CLI = sum(CLI_amount_annual, na.rm = TRUE) / 10^9) |>
#   left_join(
#     df |>
#       group_by(year) |>
#       summarise(Total = sum(total_lobby_annual, na.rm = TRUE) / 10^9),
#     by = "year"
#   ) |>
#   mutate(
#     Other = Total - CLI,
#     Share = CLI / Total
#   )
# 
# p3 |>
#   select(-c(Total, Share)) |>
#   pivot_longer(CLI:Other, names_to = "Issue", values_to = "money") |>
#   filter(year < 2020) |>
#   ggplot(aes(x = factor(year), y = money, group = factor(Issue))) +
#   geom_line(aes(linetype = factor(Issue))) +
#   theme_hanno() +
#   labs(x = "Year", y = "Money Spent (Bio USD)") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   expand_limits(y = 0) +
#   scale_linetype_manual(name = "", 
#                         labels = c("Climate Issues", "Other Issues"),
#                         values = c("solid", "dashed")) +
#   theme(legend.position = "bottom")
# 
# 
# ## Save this
# ggsave("results/figures/descriptives/climate_spending_overtime_annual.pdf", width = 9, height = 5.5)
# # ggsave(plot = p2, "report/images/CLI_quartermate_spending_overtime.png", width = 9, height = 5.5)


# ## By issue -----------------------------------------------------------------
# 
# df |>
#   group_by(year, CLI_) |>
#   summarise(money = sum(amount_num, na.rm = TRUE)) |>
#   mutate(money = money / 10^9) |>
#   filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
#   bind_rows(
#     df |>
#       group_by(yearqtr, CAW) |>
#       summarise(money = sum(amount_num, na.rm = TRUE)) |>
#       mutate(money = money / 10^9) |>
#       filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
#     df |>
#       group_by(yearqtr, ENG) |>
#       summarise(money = sum(amount_num, na.rm = TRUE)) |>
#       mutate(money = money / 10^9) |>
#       filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
#     df |>
#       group_by(yearqtr, FUE) |>
#       summarise(money = sum(amount_num, na.rm = TRUE)) |>
#       mutate(money = money / 10^9) |>
#       filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
#     ) |>
#   ggplot(aes(x = factor(yearqtr), y = money, group = factor(issue))) +
#   geom_line(aes(color = factor(issue))) +
#   theme_hanno() +
#   labs(x = "Year", y = "Money Spent (Bio USD)") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   expand_limits(y = 0) +
#   scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
#                    labels = function(x) str_sub(x, end = -3)) +
#   scale_color_brewer(type =  "qual", name = "", palette = 2) +
#   theme(legend.position = "bottom")
# 
## Save this
# ggsave("results/figures/descriptives/CLI_quartermate_spending_overtime_issues.pdf", width = 9, height = 5.5)
# # ggsave("report/images/CLI_quartermate_spending_overtime_issues.png", width = 9, height = 5.5)
# 
# 
# # Combine
# pcomb <- plot_grid(p1, p2, labels = "AUTO", nrow = 2)
# ggsave2(plot = pcomb, "results/figures/descriptives/CLI_quartermate_lobbying_overtime_comb.pdf", width = 9, height = 9)



# ## Firms over time ---------------------------------------------------------
# 
# df |>
#   group_by(yearqtr, CLI_quarter) |>
#   summarise(firms = n_distinct(gvkey)) |>
#   ggplot(aes(x = factor(yearqtr), y = firms, group = factor(CLI_quarter))) +
#   geom_line(aes(linetype = factor(CLI_quarter))) +
#   theme_hanno() +
#   labs(x = "Year", y = "No. of firms lobbying") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
#                    labels = function(x) str_sub(x, end = -3)) +
#   expand_limits(y = 0) +
#   scale_linetype_manual(name = "", 
#                         labels = c("Other Issues", "CLI_quartermate Issues"),
#                         values = c("solid", "dashed")) +
#   theme(legend.position = "bottom")
# 
# ## Save this
# ggsave("results/figures/descriptives/CLI_quartermate_firms_overtime.pdf", width = 9, height = 5.5)
# # ggsave("report/images/CLI_quartermate_firms_overtime.png", width = 9, height = 5.5)
# 
# 
# # By issues
# df |>
#   group_by(yearqtr, ENV) |>
#   summarise(firms = n_distinct(gvkey)) |>
#   filter(ENV==1) |> select(-ENV) |> mutate(issue = "Environmental") |>
#   bind_rows(
#     df |>
#       group_by(yearqtr, CAW) |>
#       summarise(firms = n_distinct(gvkey)) |>
#       filter(CAW==1) |> select(-CAW) |> mutate(issue = "Clean Air & Water (Quality)"),
#     df |>
#       group_by(yearqtr, ENG) |>
#       summarise(firms = n_distinct(gvkey)) |>
#       filter(ENG==1) |> select(-ENG) |> mutate(issue = "Energy/Nuclear"),
#     df |>
#       group_by(yearqtr, FUE) |>
#       summarise(firms = n_distinct(gvkey)) |>
#       filter(FUE==1) |> select(-FUE) |> mutate(issue = "Fuel/Gas/Oil")
#   ) |>
#   ggplot(aes(x = factor(yearqtr), y = firms, group = factor(issue))) +
#   geom_line(aes(color = factor(issue))) +
#   theme_hanno() +
#   labs(x = "Year", y = "No. of firms lobbying") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   expand_limits(y = 0) +
#   scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
#                    labels = function(x) str_sub(x, end = -3)) +
#   expand_limits(y = 0) +
#   scale_color_brewer(type =  "qual", name = "", palette = 2) +
#   theme(legend.position = "bottom")
# 
# ## Save this
# ggsave("results/figures/descriptives/CLI_quartermate_firms_overtime_issues.pdf", width = 9, height = 5.5)
# # ggsave("report/images/CLI_quartermate_firms_overtime_issues.png", width = 9, height = 5.5)




# Differentiate regarding CLI_quartermate attention -------------------------------


# Get within industry-year categorical & binary variables indicating how much 
# CLI_quartermate change attention firms experienced in comparison to other firms
# in the same industry
define_CLI_exposure <- function(dataset, variables) {
  for (variable in variables) {
    dataset <- dataset %>%
      group_by(yearqtr, industry) %>%
      mutate(
        # Decile calculations for year-quarter-industry
        !!paste0(variable, "_yqindustry_d1") := quantile(.data[[variable]], 0.1, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d2") := quantile(.data[[variable]], 0.2, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d3") := quantile(.data[[variable]], 0.3, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d4") := quantile(.data[[variable]], 0.4, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d5") := quantile(.data[[variable]], 0.5, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d6") := quantile(.data[[variable]], 0.6, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d7") := quantile(.data[[variable]], 0.7, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d8") := quantile(.data[[variable]], 0.8, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_d9") := quantile(.data[[variable]], 0.9, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_top1pct") := quantile(.data[[variable]], 0.99, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_top5pct") := quantile(.data[[variable]], 0.95, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_mean") := mean(.data[[variable]], na.rm = TRUE),
        
        # Quartiles for year-quarter-industry
        !!paste0(variable, "_yqindustry_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
        !!paste0(variable, "_yqindustry_var") := var(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_yq_disc_quartiles") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_yqindustry_q1")) ~ "Q1",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q1")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d5")) ~ "Q2",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d5")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_q3")) ~ "Q3",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q3")) ~ "Q4"
        ),
        !!paste0(variable, "_yqindustry_above_median") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yqindustry_d5")), 1, 0),
        !!paste0(variable, "_yqindustry_above_mean") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yqindustry_mean")), 1, 0),
        !!paste0(variable, "_yqindustry_above_75") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yqindustry_q3")), 1, 0),
        !!paste0(variable, "_yqindustry_above_90") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yqindustry_d9")), 1, 0),
        !!paste0(variable, "_yqindustry_above_95") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yqindustry_top5pct")), 1, 0),
        !!paste0(variable, "_yqindustry_above_99") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yqindustry_top1pct")), 1, 0),
        
        # Decile categorization for year-quarter-industry
        !!paste0(variable, "_yq_disc_deciles") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d1")) ~ "D1",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d1")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d2")) ~ "D2",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d2")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d3")) ~ "D3",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d3")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d4")) ~ "D4",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d4")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d5")) ~ "D5",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d5")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d6")) ~ "D6",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d6")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d7")) ~ "D7",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d7")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d8")) ~ "D8",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d8")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_d9")) ~ "D9",
          .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_d9")) ~ "D10")
      ) %>%
      ungroup() %>%
      
      # Repeat the same for year-industry
      group_by(year, industry) %>%
      mutate(
        # Decile calculations for year-industry
        !!paste0(variable, "_yindustry_d1") := quantile(.data[[variable]], 0.1, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d2") := quantile(.data[[variable]], 0.2, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d3") := quantile(.data[[variable]], 0.3, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d4") := quantile(.data[[variable]], 0.4, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d5") := quantile(.data[[variable]], 0.5, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d6") := quantile(.data[[variable]], 0.6, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d7") := quantile(.data[[variable]], 0.7, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d8") := quantile(.data[[variable]], 0.8, na.rm = TRUE),
        !!paste0(variable, "_yindustry_d9") := quantile(.data[[variable]], 0.9, na.rm = TRUE),
        !!paste0(variable, "_yindustry_top1pct") := quantile(.data[[variable]], 0.99, na.rm = TRUE),
        !!paste0(variable, "_yindustry_top5pct") := quantile(.data[[variable]], 0.95, na.rm = TRUE),
        !!paste0(variable, "_yindustry_mean") := mean(.data[[variable]], na.rm = TRUE),
        
        # Quartiles for year-industry
        !!paste0(variable, "_yindustry_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
        !!paste0(variable, "_yindustry_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
        !!paste0(variable, "_yindustry_var") := var(.data[[variable]], na.rm = TRUE),
        !!paste0(variable, "_y_disc_quartiles") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_yindustry_q1")) ~ "Q1",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_q1")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d5")) ~ "Q2",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d5")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_q3")) ~ "Q3",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_q3")) ~ "Q4"
        ),
        !!paste0(variable, "_yindustry_above_median") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yindustry_d5")), 1, 0),
        !!paste0(variable, "_yindustry_above_mean") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yindustry_mean")), 1, 0),
        !!paste0(variable, "_yindustry_above_75") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yindustry_q3")), 1, 0),
        !!paste0(variable, "_yindustry_above_90") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yindustry_d9")), 1, 0),
        !!paste0(variable, "_yindustry_above_95") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yindustry_top5pct")), 1, 0),
        !!paste0(variable, "_yindustry_above_99") := ifelse(.data[[variable]] > !!sym(paste0(variable, "_yindustry_top1pct")), 1, 0),
        
        # Decile categorization for year-industry
        !!paste0(variable, "_y_disc_deciles") := case_when(
          .data[[variable]] < !!sym(paste0(variable, "_yindustry_d1")) ~ "D1",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d1")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d2")) ~ "D2",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d2")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d3")) ~ "D3",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d3")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d4")) ~ "D4",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d4")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d5")) ~ "D5",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d5")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d6")) ~ "D6",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d6")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d7")) ~ "D7",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d7")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d8")) ~ "D8",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d8")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_d9")) ~ "D9",
          .data[[variable]] >= !!sym(paste0(variable, "_yindustry_d9")) ~ "D10"
        ),
      ) %>%
      ungroup()
  }
  return(dataset)
}



# define_CLI_exposure <- function(dataset, variables) {
#   for (variable in variables) {
#     dataset <- dataset %>%
#       group_by(yearqtr, industry) %>%
#       mutate(
#         !!paste0(variable, "_yqindustry_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
#         !!paste0(variable, "_yqindustry_median") := median(.data[[variable]], na.rm = TRUE),
#         !!paste0(variable, "_yqindustry_mean") := mean(.data[[variable]], na.rm = TRUE),
#         !!paste0(variable, "_yqindustry_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
#         !!paste0(variable, "_yqindustry_var") := var(.data[[variable]], na.rm = TRUE),
#         !!paste0(variable, "_yq_disc") := case_when(
#           .data[[variable]] < !!sym(paste0(variable, "_yqindustry_q1")) ~ "Q1",
#           .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q1")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_median")) ~ "Q2",
#           .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_median")) & .data[[variable]] < !!sym(paste0(variable, "_yqindustry_q3")) ~ "Q3",
#           .data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q3")) ~ "Q4"
#         ),
#         !!paste0(variable, "_yqindustry_above_mean") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yqindustry_mean")), 1, 0),
#         !!paste0(variable, "_yqindustry_above_75") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yqindustry_q3")), 1, 0)
#       ) %>%
#       ungroup() %>%
#       # and now the same for year
#       group_by(year, industry) %>%
#       mutate(
#         !!paste0(variable, "_yindustry_q1") := quantile(.data[[variable]], 0.25, na.rm = TRUE),
#         !!paste0(variable, "_yindustry_median") := median(.data[[variable]], na.rm = TRUE),
#         !!paste0(variable, "_yindustry_mean") := mean(.data[[variable]], na.rm = TRUE),
#         !!paste0(variable, "_yindustry_q3") := quantile(.data[[variable]], 0.75, na.rm = TRUE),
#         !!paste0(variable, "_yindustry_var") := var(.data[[variable]], na.rm = TRUE),
#         !!paste0(variable, "_y_disc") := case_when(
#           .data[[variable]] < !!sym(paste0(variable, "_yindustry_q1")) ~ "Q1",
#           .data[[variable]] >= !!sym(paste0(variable, "_yindustry_q1")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_median")) ~ "Q2",
#           .data[[variable]] >= !!sym(paste0(variable, "_yindustry_median")) & .data[[variable]] < !!sym(paste0(variable, "_yindustry_q3")) ~ "Q3",
#           .data[[variable]] >= !!sym(paste0(variable, "_yindustry_q3")) ~ "Q4"
#         ),
#         !!paste0(variable, "_yindustry_above_mean") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yindustry_mean")), 1, 0),
#         !!paste0(variable, "_yindustry_above_75") := ifelse(.data[[variable]] >= !!sym(paste0(variable, "_yindustry_q3")), 1, 0)
#       ) %>%
#       ungroup()
#   }
#   return(dataset)
# }

# inspect <- dfx |>
#   select(gvkey, industry, yearqtr, year, op_expo_ew, op_expo_ew_yqindustry_q1:op_expo_ew_yindustry_above_75)


# First, reduce to firms that lobby on climate
df_c <- df |> filter(CLI_quarter == 1)

# Create variables
df_distr <- df_c  |>
  filter(industry != "" & !is.na(industry)) |>
  define_CLI_exposure(c("op_expo_ew", "rg_expo_ew", "ph_expo_ew"))


inspect_op <- df_distr |>
  select(gvkey, industry, yearqtr, op_expo_ew, op_expo_ew_yqindustry_d1:op_expo_ew_yqindustry_above_75)

inspect_ph <- df_distr |>
  select(gvkey, industry, yearqtr, ph_expo_ew, ph_expo_ew_yqindustry_d1:ph_expo_ew_y_disc_deciles)



#### inspect

## opportunity
# quarterly
table(df_distr$op_expo_ew_yq_disc_deciles)
table(df_distr$op_expo_ew_yqindustry_above_75)
table(df_distr$op_expo_ew_yqindustry_above_90)
table(df_distr$op_expo_ew_yqindustry_above_95)
table(df_distr$op_expo_ew_yqindustry_above_99)
table(df_distr$op_expo_ew_yindustry_above_mean)
table(df_distr$op_expo_ew_yindustry_above_median)

# yearly
table(df_distr$op_expo_ew_y_disc_deciles)
table(df_distr$op_expo_ew_yindustry_above_90)
table(df_distr$op_expo_ew_yindustry_above_95)
table(df_distr$op_expo_ew_yindustry_above_99)

## regulatory
# quarterly
table(df_distr$rg_expo_ew_yq_disc_deciles)
table(df_distr$rg_expo_ew_yqindustry_above_90)
table(df_distr$rg_expo_ew_yqindustry_above_95)
table(df_distr$rg_expo_ew_yqindustry_above_99)
table(df_distr$rg_expo_ew_yindustry_above_mean)

# yearly
table(df_distr$rg_expo_ew_y_disc_deciles)
table(df_distr$rg_expo_ew_yindustry_above_90)
table(df_distr$rg_expo_ew_yindustry_above_95)
table(df_distr$rg_expo_ew_yindustry_above_99)

## physical
# quarterly
table(df_distr$ph_expo_ew_yq_disc_deciles)
table(df_distr$ph_expo_ew_yqindustry_above_90)
table(df_distr$ph_expo_ew_yqindustry_above_95)
table(df_distr$ph_expo_ew_yqindustry_above_99)
table(df_distr$ph_expo_ew_yqindustry_above_mean)
table(df_distr$ph_expo_ew_yqindustry_above_median)

# yearly
table(df_distr$ph_expo_ew_y_disc_deciles)
table(df_distr$ph_expo_ew_yindustry_above_90)
table(df_distr$ph_expo_ew_yindustry_above_95)
table(df_distr$ph_expo_ew_yindustry_above_99)



# ## Reports over time -------------------------------------------------------
# 
# df_distr |>
#   count(yearqtr, op_expo_ew_disc) |> rename(quantile = op_expo_ew_disc) |> mutate(measure = "Opportunity") |>
#   bind_rows(
#     df_distr |> count(yearqtr, rg_expo_ew_disc) |> rename(quantile = rg_expo_ew_disc) |> mutate(measure = "Regulatory"),
#     df_distr |> count(yearqtr, ph_expo_ew_disc) |> rename(quantile = ph_expo_ew_disc) |> mutate(measure = "Physical")
#   ) |>
#   filter(!is.na(quantile)) |>
#   ggplot(aes(x = factor(yearqtr), y = n, group = factor(quantile))) +
#   geom_line(aes(color = factor(quantile))) +
#   theme_hanno() +
#   facet_wrap(~measure, ncol = 3) +
#   labs(x = "Year", y = "Number of Lobbying Reports", color = "Firm Quartile") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   expand_limits(y = 0) +
#   scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
#                    labels = function(x) str_sub(x, end = -3)) +
#   scale_color_brewer(type =  "qual", palette = 2) +
#   # scale_linetype_discrete(name = "") +
#   theme(legend.position = "bottom")
# 
# ## Save this
# ggsave("results/figures/descriptives/climate_lobbying_overtime_variation.pdf", width = 10, height = 5.5)
# # ggsave("report/images/CLI_quartermate_lobbying_overtime_variation.png", width = 10, height = 5.5)



## Money over time (above median) ---------------------------------------------------------

plot_df <- df_distr |>
  group_by(yearqtr, op_expo_ew_yqindustry_above_median) |>
  summarise(money = sum(CLI_amount_quarter, na.rm = TRUE)  / 10^6) |>
  rename(quantile = op_expo_ew_yqindustry_above_median) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_distr |> group_by(yearqtr, rg_expo_ew_yqindustry_above_median) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = rg_expo_ew_yqindustry_above_median) |> mutate(measure = "Regulatory"),
    df_distr |> group_by(yearqtr, ph_expo_ew_yqindustry_above_median) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = ph_expo_ew_yqindustry_above_median) |> mutate(measure = "Physical")
    ) |>
  ungroup() |>
  filter(!is.na(quantile))

plot_df |>
  ggplot(aes(x = factor(yearqtr), y = money, group = factor(quantile))) +
  geom_line(aes(color = factor(quantile))) +
  theme_hanno() +
  facet_wrap(~measure, ncol = 1) +
  labs(x = "Year", y = "Lobbying Expenditure (Million USD)", color = "Firm Quartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type =  "qual", palette = 2) +
  # scale_linetype_discrete(name = "") +
  theme(legend.position = "bottom")


plot_df <- df_distr |>
  group_by(yearqtr, op_expo_ew_yqindustry_above_mean) |>
  summarise(money = sum(CLI_amount_quarter, na.rm = TRUE)  / 10^6) |>
  rename(quantile = op_expo_ew_yqindustry_above_mean) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_distr |> group_by(yearqtr, rg_expo_ew_yqindustry_above_mean) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = rg_expo_ew_yqindustry_above_mean) |> mutate(measure = "Regulatory"),
    df_distr |> group_by(yearqtr, ph_expo_ew_yqindustry_above_mean) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = ph_expo_ew_yqindustry_above_mean) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  filter(!is.na(quantile)) |>
  # Transform quartiles into percentage ranges
  mutate(quantile = case_when(
    quantile == "Q1" ~ "0-25%",
    quantile == "Q2" ~ "25-50%",
    quantile == "Q3" ~ "50-75%",
    quantile == "Q4" ~ "75-100%"
  ))

plot_df |>
  ggplot(aes(x = factor(yearqtr), y = money, group = factor(quantile))) +
  geom_line(aes(
    # color = factor(quantile)
    #, linetype = factor(quantile)
    ) #, linewidth = 1.25
    ) +
  geom_point(aes(shape = factor(quantile))) +
  theme_hanno() +
  facet_wrap(~measure, ncol = 1) +
  labs(x = "Year", y = "Lobbying Expenditure (Mio USD)", color = "Firm Quartile", linetype = "Firm Quartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 8)], labels = function(x) str_sub(x, end = -3)) +
  # scale_color_viridis_d(option = "D", end = 0.9) +
  # scale_linetype_manual(values = c("dotted", "twodash", "dashed", "solid")) +
  # scale_color_brewer(type = "qual", palette = "Set1") +
  # scale_color_manual(values = c("darkgrey", "blue", "red", "black")) +
  scale_shape_manual(values = c(0, 17, 2, 19)) +
  theme(legend.position = "bottom", 
        text = element_text(size = 17),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## Save this
ggsave("results/figures/descriptives/climate_spending_overtime_variation.pdf", width = 10, height = 7)
# ggsave("report/images/CLI_quartermate_spending_overtime_variation.png", width = 10, height = 5.5)


## Money over time (quartiles) ---------------------------------------------------------

plot_df <- df_distr |>
  group_by(yearqtr, op_expo_ew_yq_disc_quartiles) |>
  summarise(money = sum(CLI_amount_quarter, na.rm = TRUE)  / 10^6) |>
  rename(quantile = op_expo_ew_yq_disc_quartiles) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_distr |> group_by(yearqtr, rg_expo_ew_yq_disc_quartiles) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = rg_expo_ew_yq_disc_quartiles) |> mutate(measure = "Regulatory"),
    df_distr |> group_by(yearqtr, ph_expo_ew_yq_disc_quartiles) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = ph_expo_ew_yq_disc_quartiles) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  filter(!is.na(quantile)) |>
  # Transform quartiles into percentage ranges
  mutate(quantile = as.factor(case_when(
    quantile == "Q1" ~ "0-25%",
    quantile == "Q2" ~ "25-50%",
    quantile == "Q3" ~ "50-75%",
    quantile == "Q4" ~ "75-100%"
  )))

plot_df |>
  ggplot(aes(x = factor(yearqtr), y = money, group = quantile)) +
  geom_line(aes(
    color = quantile
    #, linetype = factor(quantile)
  ) #, linewidth = 1.25
  ) +
  geom_point(aes(shape = quantile
                 ,color = quantile
                 )) +
  theme_hanno() +
  facet_wrap(~measure, ncol = 1) +
  labs(x = "Year", y = "Lobbying Expenditure (Mio USD)", 
       color = "Firm Exposure Quartile", linetype = "Firm Exposure Quartile", shape = "Firm Exposure Quartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)], labels = function(x) str_sub(x, end = -3)) +
  # scale_color_viridis_d(option = "D", end = 0.9) +
  # scale_linetype_manual(values = c("dotted", "twodash", "dashed", "solid")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  # scale_color_manual(values = c("darkgrey", "blue", "red", "black")) +
  scale_shape_manual(values = c(0, 17, 2, 19)) +
  theme(legend.position = "bottom", 
        text = element_text(size = 17),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## Save this
ggsave("results/figures/descriptives/climate_spending_overtime_variation.pdf", width = 10, height = 10)
# ggsave("report/images/CLI_quartermate_spending_overtime_variation.png", width = 10, height = 5.5)


## Annual Money over time (quartiles) ---------------------------------------------------------

plot_df <- df_distr |>
  group_by(year, op_expo_ew_yq_disc_quartiles) |>
  summarise(money = sum(CLI_amount_quarter, na.rm = TRUE)  / 10^6) |>
  rename(quantile = op_expo_ew_yq_disc_quartiles) |> mutate(measure = "Opportunity") |>
  bind_rows(
    df_distr |> group_by(year, rg_expo_ew_yq_disc_quartiles) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = rg_expo_ew_yq_disc_quartiles) |> mutate(measure = "Regulatory"),
    df_distr |> group_by(year, ph_expo_ew_yq_disc_quartiles) |>
      summarise(money = sum(CLI_amount_quarter, na.rm = TRUE) / 10^6) |>
      rename(quantile = ph_expo_ew_yq_disc_quartiles) |> mutate(measure = "Physical")
  ) |>
  ungroup() |>
  filter(!is.na(quantile)) |>
  # Transform quartiles into percentage ranges
  mutate(quantile = as.factor(case_when(
    quantile == "Q1" ~ "0-25%",
    quantile == "Q2" ~ "25-50%",
    quantile == "Q3" ~ "50-75%",
    quantile == "Q4" ~ "75-100%"
  )))

plot_df |>
  ggplot(aes(x = factor(year), y = money, group = quantile)) +
  geom_line(aes(
    color = quantile
    #, linetype = factor(quantile)
  ) #, linewidth = 1.25
  ) +
  geom_point(aes(shape = quantile
                 ,color = quantile
  )) +
  theme_hanno() +
  facet_wrap(~measure, ncol = 1) +
  labs(x = "Year", y = "Lobbying Expenditure (Mio USD)", 
       color = "Firm Exposure Quartile", linetype = "Firm Exposure Quartile", shape = "Firm Exposure Quartile") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  expand_limits(y = 0) +
  # scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)], labels = function(x) str_sub(x, end = -3)) +
  # scale_color_viridis_d(option = "D", end = 0.9) +
  # scale_linetype_manual(values = c("dotted", "twodash", "dashed", "solid")) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  # scale_color_manual(values = c("darkgrey", "blue", "red", "black")) +
  scale_shape_manual(values = c(0, 17, 2, 19)) +
  theme(legend.position = "bottom", 
        text = element_text(size = 17),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

## Save this
ggsave("results/figures/descriptives/climate_spending_overtime_variation_annual.pdf", width = 10, height = 10)
# ggsave("report/images/CLI_quartermate_spending_overtime_variation.png", width = 10, height = 5.5)



# 
# ## Firms over time ---------------------------------------------------------
# 
# df_distr |>
#   group_by(yearqtr, opexpo_q_disc) |>
#   summarise(firms = n_distinct(gvkey)) |>
#   rename(quantile = opexpo_q_disc) |> mutate(measure = "Opportunity") |>
#   bind_rows(
#     df_distr |> group_by(yearqtr, rgexpo_q_disc) |>
#       summarise(firms = n_distinct(gvkey)) |>
#       rename(quantile = rgexpo_q_disc) |> mutate(measure = "Regulatory"),
#     df_distr |> group_by(yearqtr, phexpo_q_disc) |>
#       summarise(firms = n_distinct(gvkey)) |>
#       rename(quantile = phexpo_q_disc) |> mutate(measure = "Physical")
#   ) |>
#   ungroup() |>
#   ggplot(aes(x = factor(yearqtr), y = firms, group = factor(quantile))) +
#   geom_line(aes(color = factor(quantile))) +
#   theme_hanno() +
#   facet_wrap(~measure, ncol = 3) +
#   labs(x = "Year", y = "No. of firms lobbying", color = "Firm Quantile") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   expand_limits(y = 0) +
#   scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
#                    labels = function(x) str_sub(x, end = -3)) +
#   scale_color_brewer(type =  "qual", palette = 2) +
#   theme(legend.position = "bottom", text = element_text(size = 15))
# 
# ## Save this
# ggsave("results/figures/descriptives/CLI_quartermate_firms_overtime_variation.pdf", width = 10, height = 5.5)
# # ggsave("report/images/CLI_quartermate_firms_overtime_variation.png", width = 10, height = 5.5)


### END