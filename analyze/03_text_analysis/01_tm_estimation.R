### Firms & Lobbying
### Text Analysis

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, stm, tm)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="vh4264" ) {setwd("/home/vh4264/bbh1/")}


# Load data
df <- fread("orig/lobbying_df_reduced_fb.csv") |>
  # Remove observations with no text
  filter(issue_text != "") |>
  data.frame() |>
  mutate(quarter = paste0(year, "_", report_quarter_code))

# Transform Variables -----------------------------------------------------

df <- df |>
  mutate(
    # Dummy variable: climate issues
    CLI = ifelse(issue_code %in% c("ENV", "CAW", "ENG", "FUE"), 1, 0),
    # US headquarter
    us_dummy = ifelse(hqcountrycode == "US", 1, 0)) |>
  # Total annual lobbying (total dollars)
  group_by(gvkey, year) |>
  mutate(total_lobby = sum(amount_num)) |>
  ungroup()

# Code industry variable
df$industry <- df$bvd_sector
df <- df[which(df$industry!=""), ]
df$industry_year <- paste(df$industry, df$year)

df$ebit_at <- df$ebit / df$assets


# remove all observations that have NA for any of these variables
df <- df %>%
  filter(
    complete.cases(
      op_expo_ew_y, rg_expo_ew_y, ph_expo_ew_y, ebit, I(ebit/assets), 
      us_dummy, total_lobby, factor(year), factor(industry), factor(industry_year)
    )
  )



# Only Climate Lobbying Reports -------------------------------------------

df <- df |> 
  filter(CLI == 1)

# Preprocessing -----------------------------------------------------------

# set seed
seed <- 1234
seed
set.seed(seed)

# set number of topics
k <- 5
k

# set formula: firms + quarter fixed effects
formula.t <- as.formula("~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/assets) + us_dummy + total_lobby + factor(year) + factor(industry) + factor(industry_year)")
formula.file.string <- "-firm-quarter"

formula.t
formula.file.string

formula.est <- update(formula.t, 1:k ~ .)
formula.est

# set min threshold
low.thres <- 100
low.thres

# Process text for STM
processed <- textProcessor(
  df$issue_text,
  metadata = df,
  removenumbers = FALSE,
  removepunctuation = TRUE,
  removestopwords = TRUE
)

# Plot removed words
# plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

# Prepare documents for STM
out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh = low.thres)


# Top Five Topics in U.S. Climate Lobbying --------------------------------

# Fit 5-topic model
# Start clock
ptm <- proc.time()

# fit model
prev.fit <- stm(documents = out$documents,
                vocab = out$vocab,
                K = k,
                prevalence = formula.t,
                max.em.its = 1000,
                data = out$meta,
                seed = seed,
                init.type = "Spectral")

# stop clock
proc.time() - ptm

# save
save(prev.fit, file = "output/topicmodels.RData")
save(out, file = "output/prepDocuments.RData")

### END