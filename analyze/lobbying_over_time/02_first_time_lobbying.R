### Firms & Climate Lobbying over time
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
  mutate(
    CLI = ifelse(ENV == 1 |
                   CAW == 1 |
                   ENG == 1 |
                   FUE == 1,
                 1, 0),
    not_CLI = ifelse(CLI == 1, 0, 1),
    year_quarter = paste0(year, "_", report_quarter_code)
  )


# Code first entry --------------------------------------------------------



## Year quarter ------------------------------------------------------------

df2 <- df |>
  # get number of reports by firm in given year quarter
  group_by(year_quarter, gvkey) |>
  mutate(
    n_reports_q = ifelse(!is.na(gvkey), n(), NA),
    n_climate_reports_q = ifelse(!is.na(gvkey), sum(CLI), NA),
    climate_report_q_bin = ifelse(n_climate_reports_q > 0, 1, 0)
  ) |>
  ungroup() |>
  arrange(gvkey, year_quarter)

# insp <- df2 |> select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin) |>
#   distinct()
# # worked

# Next: code first time lobbying
# Get firms that lobby on climate at any point
firms_cl <- df2 |>
  select(gvkey, climate_report_q_bin) |>
  filter(climate_report_q_bin == 1) |>
  distinct(gvkey) |>
  pull()

df2 <- df2 |>
  group_by(gvkey) |>
  mutate(
    firms_climate = ifelse(gvkey %in% firms_cl, 1, 0),
    year_quarter_first_climate = ifelse(firms_climate == 1,
                                        first(year_quarter[climate_report_q_bin == 1]),
                                        "999999"),
    first_climate_report_q_bin = ifelse(year_quarter == year_quarter_first_climate, 1, 0),
    first_climate_report_q_bin = ifelse(
      is.na(first_climate_report_q_bin),
      0,
      first_climate_report_q_bin
    )
  )

insp <- df2 |>
  select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
                     firms_climate, year_quarter_first_climate, first_climate_report_q_bin) |>
  distinct()
# worked



## Year --------------------------------------------------------------------

df2 <- df2 |>
  # get number of reports by firm in given year
  group_by(year, gvkey) |>
  mutate(
    n_reports_y = ifelse(!is.na(gvkey), n(), NA),
    n_climate_reports_y = ifelse(!is.na(gvkey), sum(CLI), NA),
    climate_report_y_bin = ifelse(n_climate_reports_y > 0, 1, 0)
  ) |>
  ungroup() |>
  arrange(gvkey, year_quarter)

insp <- df2 |> select(gvkey, year_quarter, n_reports_y, n_climate_reports_y, climate_report_y_bin) |>
  distinct()
# worked

# Next: code first time lobbying
# Get firms that lobby on climate at any point
firms_cl_y <- df2 |>
  select(gvkey, climate_report_y_bin) |>
  filter(climate_report_y_bin == 1) |>
  distinct(gvkey) |>
  pull()

df2 <- df2 |>
  group_by(gvkey) |>
  mutate(
    firms_climate = ifelse(gvkey %in% firms_cl_y, 1, 0),
    year_first_climate = ifelse(firms_climate == 1,
                                first(year[climate_report_y_bin == 1]),
                                9999),
    first_climate_report_y_bin = ifelse(year == year_first_climate, 1, 0),
    first_climate_report_y_bin = ifelse(
      is.na(first_climate_report_y_bin),
      0,
      first_climate_report_y_bin
    )
  )

insp <- df2 |>
  select(gvkey, year_quarter, n_reports_y, n_climate_reports_y, climate_report_y_bin,
         firms_climate, year_first_climate, first_climate_report_y_bin) |>
  distinct()
# worked



# Code climate lobbying after not having lobbied during the previo --------


## Year quarter ------------------------------------------------------------

# We need a df with only one observation per firm per year_quarter for this
df_red <- df2 |>
  select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
         firms_climate, year_quarter_first_climate, first_climate_report_q_bin) |>
  distinct() |>
  arrange(gvkey, year_quarter) |>
  group_by(gvkey) |>
  mutate(climate_report_afterpause_q_bin = ifelse(
    climate_report_q_bin == 1 &
      lag(climate_report_q_bin, default = 0) == 0 &
      first_climate_report_q_bin == 0,
    1,
    0
  )) |>
  ungroup() |>
  select(gvkey, year_quarter, climate_report_afterpause_q_bin)

# Merge with df2
df2 <- df2 |>
  left_join(df_red, by = c("gvkey", "year_quarter"))

insp <- df2 |>
  filter(firms_climate == 1) |>
  select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
                     firms_climate, first_climate_report_q_bin, climate_report_afterpause_q_bin) |>
  arrange(gvkey, year_quarter)
# worked


## Year ------------------------------------------------------------

# We need a df with only one observation per firm per year for this
df_red <- df2 |>
  select(gvkey, year, n_reports_y, n_climate_reports_y, climate_report_y_bin,
         firms_climate, year_first_climate, first_climate_report_y_bin) |>
  distinct() |>
  arrange(gvkey, year) |>
  group_by(gvkey) |>
  mutate(climate_report_afterpause_y_bin = ifelse(
    climate_report_y_bin == 1 &
      lag(climate_report_y_bin, default = 0) == 0 &
      first_climate_report_y_bin == 0,
    1,
    0
  )) |>
  ungroup() |>
  select(gvkey, year, climate_report_afterpause_y_bin)

# Merge with df2
df2 <- df2 |>
  left_join(df_red, by = c("gvkey", "year"))

insp <- df2 |>
  filter(firms_climate == 1) |>
  select(gvkey, year_quarter, n_reports_y, n_climate_reports_y, climate_report_y_bin,
         firms_climate, first_climate_report_y_bin, climate_report_afterpause_y_bin) |>
  arrange(gvkey, year_quarter)
# worked




# Code first entry non-climate --------------------------------------------------------



## Year quarter ------------------------------------------------------------

df2 <- df |>
  # get number of reports by firm in given year quarter
  group_by(year_quarter, gvkey) |>
  mutate(
    n_reports_q = ifelse(!is.na(gvkey), n(), NA),
    n_climate_reports_q = ifelse(!is.na(gvkey), sum(CLI), NA),
    climate_report_q_bin = ifelse(n_climate_reports_q > 0, 1, 0)
  ) |>
  ungroup() |>
  arrange(gvkey, year_quarter)

# insp <- df2 |> select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin) |>
#   distinct()
# # worked

# Next: code first time lobbying
# Get firms that lobby on climate at any point
firms_cl <- df2 |>
  select(gvkey, climate_report_q_bin) |>
  filter(climate_report_q_bin == 1) |>
  distinct(gvkey) |>
  pull()

df2 <- df2 |>
  group_by(gvkey) |>
  mutate(
    firms_climate = ifelse(gvkey %in% firms_cl, 1, 0),
    year_quarter_first_climate = ifelse(firms_climate == 1,
                                        first(year_quarter[climate_report_q_bin == 1]),
                                        "999999"),
    first_climate_report_q_bin = ifelse(year_quarter == year_quarter_first_climate, 1, 0),
    first_climate_report_q_bin = ifelse(
      is.na(first_climate_report_q_bin),
      0,
      first_climate_report_q_bin
    )
  )

insp <- df2 |>
  select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
         firms_climate, year_quarter_first_climate, first_climate_report_q_bin) |>
  distinct()
# worked



## Year --------------------------------------------------------------------

df2 <- df2 |>
  # get number of reports by firm in given year
  group_by(year, gvkey) |>
  mutate(
    n_reports_y = ifelse(!is.na(gvkey), n(), NA),
    n_climate_reports_y = ifelse(!is.na(gvkey), sum(CLI), NA),
    climate_report_y_bin = ifelse(n_climate_reports_y > 0, 1, 0)
  ) |>
  ungroup() |>
  arrange(gvkey, year_quarter)

insp <- df2 |> select(gvkey, year_quarter, n_reports_y, n_climate_reports_y, climate_report_y_bin) |>
  distinct()
# worked

# Next: code first time lobbying
# Get firms that lobby on climate at any point
firms_cl_y <- df2 |>
  select(gvkey, climate_report_y_bin) |>
  filter(climate_report_y_bin == 1) |>
  distinct(gvkey) |>
  pull()

df2 <- df2 |>
  group_by(gvkey) |>
  mutate(
    firms_climate = ifelse(gvkey %in% firms_cl_y, 1, 0),
    year_first_climate = ifelse(firms_climate == 1,
                                first(year[climate_report_y_bin == 1]),
                                9999),
    first_climate_report_y_bin = ifelse(year == year_first_climate, 1, 0),
    first_climate_report_y_bin = ifelse(
      is.na(first_climate_report_y_bin),
      0,
      first_climate_report_y_bin
    )
  )

insp <- df2 |>
  select(gvkey, year_quarter, n_reports_y, n_climate_reports_y, climate_report_y_bin,
         firms_climate, year_first_climate, first_climate_report_y_bin) |>
  distinct()
# worked



# Code non-climate lobbying after not having lobbied during the previo --------


## Year quarter ------------------------------------------------------------

# We need a df with only one observation per firm per year_quarter for this
df_red <- df2 |>
  select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
         firms_climate, year_quarter_first_climate, first_climate_report_q_bin) |>
  distinct() |>
  arrange(gvkey, year_quarter) |>
  group_by(gvkey) |>
  mutate(climate_report_afterpause_q_bin = ifelse(
    climate_report_q_bin == 1 &
      lag(climate_report_q_bin, default = 0) == 0 &
      first_climate_report_q_bin == 0,
    1,
    0
  )) |>
  ungroup() |>
  select(gvkey, year_quarter, climate_report_afterpause_q_bin)

# Merge with df2
df2 <- df2 |>
  left_join(df_red, by = c("gvkey", "year_quarter"))

insp <- df2 |>
  filter(firms_climate == 1) |>
  select(gvkey, year_quarter, n_reports_q, n_climate_reports_q, climate_report_q_bin,
         firms_climate, first_climate_report_q_bin, climate_report_afterpause_q_bin) |>
  arrange(gvkey, year_quarter)
# worked


## Year ------------------------------------------------------------

# We need a df with only one observation per firm per year for this
df_red <- df2 |>
  select(gvkey, year, n_reports_y, n_climate_reports_y, climate_report_y_bin,
         firms_climate, year_first_climate, first_climate_report_y_bin) |>
  distinct() |>
  arrange(gvkey, year) |>
  group_by(gvkey) |>
  mutate(climate_report_afterpause_y_bin = ifelse(
    climate_report_y_bin == 1 &
      lag(climate_report_y_bin, default = 0) == 0 &
      first_climate_report_y_bin == 0,
    1,
    0
  )) |>
  ungroup() |>
  select(gvkey, year, climate_report_afterpause_y_bin)

# Merge with df2
df2 <- df2 |>
  left_join(df_red, by = c("gvkey", "year"))

insp <- df2 |>
  filter(firms_climate == 1) |>
  select(gvkey, year_quarter, n_reports_y, n_climate_reports_y, climate_report_y_bin,
         firms_climate, first_climate_report_y_bin, climate_report_afterpause_y_bin) |>
  arrange(gvkey, year_quarter)
# worked






# Code first appearance overall -------------------------------------------


## Year quarter ------------------------------------------------------------
df2 <- df2 |>
  group_by(gvkey) |>
  arrange(year_quarter) |>
  mutate(first_year_quarter = min(year_quarter),
         first_lobbying_yq = ifelse(year_quarter == first_year_quarter, 1, 0)) |>
  ungroup()

insp <- df2 |>
  select(gvkey, year_quarter, first_year_quarter, first_lobbying_yq) |>
  arrange(gvkey, year_quarter)
# worked



## Year  ------------------------------------------------------------
df2 <- df2 |>
  group_by(gvkey) |>
  arrange(year_quarter) |>
  mutate(first_year = min(year),
         first_lobbying_y = ifelse(year == first_year, 1, 0)) |>
  ungroup()

insp <- df2 |>
  select(gvkey, year_quarter, first_year, first_lobbying_y) |>
  arrange(gvkey, year_quarter)
# worked




# Code pause overall ------------------------------------------------------



## Year quarter ------------------------------------------------------------

# We have to create reduced dataframe with only one year_quarter observation per firm
df_red <- df2 |>
  select(gvkey, year_quarter) |>
  distinct() |>
  arrange(gvkey, year_quarter) |>
  group_by(gvkey) |>
  mutate(
    prev_year_quarter = case_when(
      substr(year_quarter, 6, 6) == "1" ~ paste0(as.integer(substr(year_quarter, 1, 4)) - 1, "_4"),
      TRUE ~ paste0(substr(year_quarter, 1, 4), "_", as.integer(substr(year_quarter, 6, 6)) - 1)
    ),
    lobbying_pause_any_q_bin = if_else(
      prev_year_quarter != lag(year_quarter),
      1,
      0
    )
  ) |>
  ungroup() |>
  select(-prev_year_quarter)

# Merge
df2 <- df2 |>
  left_join(df_red, by = c("gvkey", "year_quarter"))


insp <- df2 |>
  select(gvkey, year_quarter, first_lobbying_yq, lobbying_pause_any_q_bin) |>
  arrange(gvkey, year_quarter)
# worked



## Year  ------------------------------------------------------------

# We have to create reduced dataframe with only one year observation per firm
df_red <- df2 |>
  select(gvkey, year) |>
  distinct() |>
  arrange(gvkey, year) |>
  group_by(gvkey) |>
  mutate(
    prev_year = lag(year),
    lobbying_pause_any_y_bin = if_else(
      year - prev_year > 1,
      1,
      0
    ),
    magnitude_of_pause_any_y = abs(year - prev_year)
  ) |>
  ungroup() |>
  select(-prev_year)

# Merge
df2 <- df2 |>
  left_join(df_red, by = c("gvkey", "year"))


insp <- df2 |>
  select(gvkey, year_quarter, first_lobbying_y, lobbying_pause_any_y_bin, magnitude_of_pause_any_y) |>
  arrange(gvkey, year_quarter)
# worked




# Number of reports -------------------------------------------------------

## First time climate lobbying by firms over time --------------------------

insp <- df2 |>
  select(gvkey, year_quarter, first_climate_report_q_bin, first_lobbying_yq)

# By quarter
df2 |>
  group_by(year_quarter) |>
  summarize(n_first_climate = sum(first_climate_report_q_bin, na.rm = TRUE),
            n_first_any = sum(first_lobbying_yq, na.rm = TRUE)) |>
  pivot_longer(cols = starts_with("n_"),
               names_to = "type",
               names_prefix = "n_",
               values_to = "n") |>
  filter(year_quarter != "1999_1") |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "Number of First Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")
# looks ugly


# By year
df2 |>
  group_by(year) |>
  summarize(n_first_climate = sum(first_climate_report_y_bin, na.rm = TRUE),
            n_first_any = sum(first_lobbying_y, na.rm = TRUE)) |>
  pivot_longer(cols = starts_with("n_"),
               names_to = "type",
               names_prefix = "n_",
               values_to = "n") |>
  filter(year != "1999") |>
  ungroup() |>
  ggplot(aes(x = factor(year), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year", y = "Number of First Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/first_time_climate_lobbying_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/first_time_climate_lobbying_overtime.png", width = 9, height = 5.5)




## Climate Lobbying After Pause --------------------------------------------


insp <- df2 |>
  select(gvkey, year_quarter, climate_report_afterpause_q_bin, lobbying_pause_any_q_bin)

# By quarter
df2 |>
  group_by(year_quarter) |>
  summarize(n_pause_climate = sum(climate_report_afterpause_q_bin, na.rm = TRUE),
            n_pause_any = sum(lobbying_pause_any_q_bin, na.rm = TRUE)) |>
  pivot_longer(cols = starts_with("n_"),
               names_to = "type",
               names_prefix = "n_",
               values_to = "n") |>
  ggplot(aes(x = factor(year_quarter), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year Quarter", y = "Number of Lobbying Reports After Pause", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")
# looks ugly


# By year
df2 |>
  group_by(year) |>
  summarize(n_pause_climate = sum(climate_report_afterpause_y_bin, na.rm = TRUE),
            n_pause_any = sum(lobbying_pause_any_y_bin, na.rm = TRUE)) |>
  pivot_longer(cols = starts_with("n_"),
               names_to = "type",
               names_prefix = "n_",
               values_to = "n") |>
  ungroup() |>
  ggplot(aes(x = factor(year), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year", y = "Number of Lobbying Reports After Pause", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_lobbying_afterpause_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_lobbying_afterpause_overtime.png", width = 9, height = 5.5)




# Number of firms -------------------------------------------------------

## First time climate lobbying by firms over time --------------------------

insp <- df2 |>
  select(gvkey, year_quarter, first_climate_report_q_bin, first_lobbying_yq)

# By year
df2 |>
  group_by(year, first_climate_report_y_bin) |>
  summarize(n_first_climate = n_distinct(gvkey)) |>
  filter(first_climate_report_y_bin == 1) |>
  select(-c(first_climate_report_y_bin)) |>
  rename(n = n_first_climate) |>
  mutate(type = "climate") |>
  bind_rows(
    df2 |>
      group_by(year, first_lobbying_y) |>
      summarize(n_first_any = n_distinct(gvkey)) |>
      filter(first_lobbying_y == 1) |>
      select(-c(first_lobbying_y)) |>
      rename(n = n_first_any) |>
      mutate(type = "any")
  ) |>
  filter(year != 1999) |>
  ggplot(aes(x = factor(year), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year", y = "Number of firms that lobby for the first time", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/first_time_climate_lobbying_firms_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/first_time_climate_lobbying_firms_overtime.png", width = 9, height = 5.5)




## Climate Lobbying After Pause --------------------------------------------


insp <- df2 |>
  select(gvkey, year_quarter, climate_report_afterpause_q_bin, lobbying_pause_any_q_bin)

# By year
df2 |>
  group_by(year, climate_report_afterpause_y_bin) |>
  summarize(n_pause_climate = n_distinct(gvkey)) |>
  filter(climate_report_afterpause_y_bin == 1) |>
  select(-c(climate_report_afterpause_y_bin)) |>
  rename(n = n_pause_climate) |>
  mutate(type = "climate") |>
  bind_rows(
    df2 |>
      group_by(year, lobbying_pause_any_y_bin) |>
      summarize(n_pause_any = n_distinct(gvkey)) |>
      filter(lobbying_pause_any_y_bin == 1) |>
      select(-c(lobbying_pause_any_y_bin)) |>
      rename(n = n_pause_any) |>
      mutate(type = "any")
  ) |>
  ggplot(aes(x = factor(year), y = n, group = factor(type))) +
  geom_line(aes(linetype = factor(type))) +
  theme_bw() +
  labs(x = "Year", y = "Number of firms that lobby after a pause", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_linetype_manual(name = "", 
                        labels = c("All Issues", "Climate Issues"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom")

## Save this
ggsave("results/Figures/descriptives/climate_lobbying_afterpause_firms_overtime.pdf", width = 9, height = 5.5)
ggsave("report/images/climate_lobbying_afterpause_firms_overtime.png", width = 9, height = 5.5)




### END