### Firms & Lobbying
### Text Analysis

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, data.table, stm, tm)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}


# Load data
df <- fread("data/lobbying_df_reduced_fb.csv") |>
  # Remove observations with no text
  filter(issue_text != "") |>
  data.frame() |>
  mutate(quarter = paste0(year, "_", report_quarter_code))


# Preprocessing -----------------------------------------------------------

# set seed
seed <- 1234
seed
set.seed(seed)

# set number of topics
k <- 5
k

# set formula: firms + quarter fixed effects
formula.t <- as.formula("~ gvkey + quarter")
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
  removepunctuation = TRUE
)

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
save(prev.fit, file = paste(MAIN_DIR, "/prev-fit-k", k,
                            years.string, exclude.years.string,
                            formula.file.string,
                            "-low-thres-", low.thres,
                            ".RData", sep = ""))


