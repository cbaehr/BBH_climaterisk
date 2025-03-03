### Firms & Lobbying
### Get Directionality with Coalition data

rm(list=ls())

# load packages
pacman::p_load(data.table, tidyverse, janitor, kableExtra)


# set working directory
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}



# Load data ---------------------------------------------------------------

df <- read_rds("data/03_final/lobbying_df_annual_REVISE_normal.rds")

coal <- fread("data/01_raw/coalitions/Lerner and Osgood 2022 replication/analysis_data_no_proprietary.csv")


# Edit coalition data ---------------------------------------------------

coal <- coal[ , c("gvkey", "year", "numsupcoal", "numoppcoal")]

coal |> tabyl(numsupcoal)
coal |> tabyl(numoppcoal)
coal |> tabyl(numsupcoal, numoppcoal) # not much overlap

# Create dummy
coal <- coal |>
  mutate(
    sup_climate_action = ifelse(numsupcoal > 0, 1, 0),
    opp_climate_action = ifelse(numoppcoal > 0, 1, 0),
    gvkey = as.character(gvkey)
  )

# How many not part at any point?
# support
coal |>
  group_by(gvkey) %>%
  summarise(support_count = sum(sup_climate_action),
            opposition_count = sum(opp_climate_action)) %>%
  filter(support_count == 0 & opposition_count == 0) |>
  nrow()
  

coal |> tabyl(sup_climate_action, opp_climate_action) # not much overlap: 274 firm-years / 45 firms


# Merge with df -----------------------------------------------------------

df$year <- as.numeric(df$year)
coal$gvkey <- as.integer(coal$gvkey)
df2 <- df |>
  left_join(coal, by = c("gvkey", "year"))

df2 |> tabyl(sup_climate_action)
df2 |> tabyl(opp_climate_action)

# Code directionality -----------------------------------------------------


## Long df --------------------------------------------------------------

df3 <- df2 |>
  mutate(CLI = ifelse(grepl("ENV|CAW|ENG|FUE", issue_code), 1, 0)) |>
  mutate(pro_CLI = ifelse(CLI == 1 & sup_climate_action == 1 & opp_climate_action == 0, 1, 0), 
         contra_CLI = ifelse(CLI == 1 & opp_climate_action == 1 & sup_climate_action == 0, 1, 0)) |>
  mutate(
    direction_CLI = case_when(
      pro_CLI == 1 ~ "Pro", 
      contra_CLI == 1 ~ "Contra",
      CLI == 1 & sup_climate_action == 1 & opp_climate_action == 1 ~ "Both", 
      CLI == 1 & sup_climate_action != 1 & opp_climate_action != 1 ~ "None")
  )

#insp <- df3 |> select(gvkey, year, issue_code, CLI, pro_CLI, direction_CLI, sup_climate_action, contra_CLI, opp_climate_action)
insp <- df3[ , c("gvkey", "year", "issue_code", "CLI", "pro_CLI", "direction_CLI", 
                 "sup_climate_action", "contra_CLI", "opp_climate_action")]

df3 |> tabyl(pro_CLI)
df3 |> tabyl(contra_CLI)


desc <- df3 |>
  filter(CLI == 1)
desc <- desc[ , c("gvkey", "year", "direction_CLI")]

# desc |> tabyl(direction_CLI) |>
#   adorn_pct_formatting() |>
#   select(-valid_percent) |>
#   rename(`Lobbying Direction` = direction_CLI,
#          N = n,
#          Percent = percent) |>
#   mutate(order = case_when(
#     `Lobbying Direction` == "Pro" ~ 1,
#     `Lobbying Direction` == "Contra" ~ 2,
#     `Lobbying Direction` == "Both" ~ 3,
#     `Lobbying Direction` == "None" ~ 4,
#     `Lobbying Direction` == "<NA>" ~ 5
#   )) |>
#   arrange(order) |>
#   select(-order) |>
#   kbl(booktabs = TRUE, "latex", caption = "Direction of Lobbying on Climate Issues",
#       label = "lobbying_direction_desc") |>
#   kable_styling(
#     latex_options = "hold_position"
#   )
  


## Wide df_reduced ---------------------------------------------------------

# df_wide3 <- df_wide2 |>
#   mutate(
#     # lobbying on climate
#     CLI = ifelse(ENV == 1 |
#                    CAW == 1 |
#                    ENG == 1 |
#                    FUE == 1,
#                  1, 0),
#     pro_CLI = ifelse(CLI == 1 &
#                        sup_climate_action == 1 &
#                        opp_climate_action == 0,
#                      1,
#                      0),
#     contra_CLI = ifelse(CLI == 1 &
#                           opp_climate_action == 1 &
#                           sup_climate_action == 0,
#                         1,
#                         0),
#     direction_CLI = case_when(
#       pro_CLI == 1 ~ "Pro", 
#       contra_CLI == 1 ~ "Contra",
#       CLI == 1 & sup_climate_action == 1 & opp_climate_action == 1 ~ "Both", 
#       CLI == 1 & sup_climate_action != 1 & opp_climate_action != 1 ~ "None")
#   )





# Write -------------------------------------------------------------------

fwrite(df3, "data/03_final/lobbying_df_w_directionality_REVISE.csv")
#fwrite(df_wide3, "data/lobbying_df_wide__red_w_directionality.csv")

### END