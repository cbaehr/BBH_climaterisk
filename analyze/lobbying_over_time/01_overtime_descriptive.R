### Firms & Lobbying over time
### Descriptive

# load packages
pacman::p_load(data.table, tidyverse, janitor)

# load data
df <- fread("~/Dropbox (Princeton)/BBH/BBH1/data/lobbying_df_wide.csv")



# Climate change lobbying over time ---------------------------------------

# Binary variable: climate issues
df <- df |>
  mutate(CLI = ifelse(ENV == 1 |
                        CAW == 1 |
                        ENG == 1 |
                        FUE == 1 |
                        ENV == 1,
                      1, 0))


## Plot over time ----------------------------------------------------------

df <- df |>
  mutate(year_quarter = paste0(year,"_",report_quarter_code))

df |> tabyl(CLI)
df |> tabyl(CLI, year_quarter)


df |>
  count(year_quarter, CLI) |>
  ggplot(aes(x = as.factor(year_quarter), y = n, group = as.factor(CLI), color = as.factor(CLI))) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Year Quarter", y = "Number of Lobbying Reports", color = "Climate Report") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)],
                   labels = function(x) str_sub(x, end = -3)) +
  scale_color_brewer(type = 'qual', palette = 2) + # Adjust colours as needed
  theme(legend.position = "bottom")



#   Climate lobbying changes over time
#     # of firms
#     Total $
#   Attention to climate change
#     Change over time
#     Variation within industry

