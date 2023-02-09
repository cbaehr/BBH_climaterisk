# # Unconventional Monetary Policy and Parliament Speeches


setwd('/scratch/network/vh4264/UMP_Speech')

library(tidyverse)
library(stm)
library(tm)

# Topic Model evaluation

load("speech_df.RData")

# preprocess
stm_df <- speech_df %>%
  filter(ecb_ump_kw_bin == 1 &
           !is.na(party) &
           party != "independent" &
           year >= 2013) %>%
  # transform and get tokens
  mutate(
    text_clean_sw = sapply(text_clean_sw, toString),
    text_clean_sw = str_remove_all(text_clean_sw, ","),
    party = factor(party)
  ) %>%
  select(-c(text, text_clean, ecb_kw, ump_kw, ecb_ump_kw))

# stm processing
processed <- textProcessor(stm_df$text_clean_sw,
                           metadata = stm_df,
                           language = "de")
out_ecb_ump <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)

# Run topic models --------------------------------------------------------

tm9 <- stm(
  out_ecb_ump$documents,
  out_ecb_ump$vocab,
  K = 9,
  prevalence = ~ party + s(year),
  data = out_ecb_ump$meta,
  init.type = "Spectral"
)

# Only UMP & ECB together ----------------------------------------------------------------

# preprocess
stm_df <- speech_df %>%
  filter(ecb_kw_bin==1 & ump_kw_bin==1 &
           !is.na(party) &
           party != "independent" &
           year >= 2013) %>%
  # transform and get tokens
  mutate(
    text_clean_sw = sapply(text_clean_sw, toString),
    text_clean_sw = str_remove_all(text_clean_sw, ","),
    party = factor(party)
  ) %>%
  select(-c(text, text_clean, ecb_kw, ump_kw, ecb_ump_kw))

# stm processing
processed <- textProcessor(stm_df$text_clean_sw,
                           metadata = stm_df,
                           language = "de")
out_ecb_ump_together <- prepDocuments(processed$documents,
                             processed$vocab,
                             processed$meta)

# Run topic models --------------------------------------------------------

tm_together9 <- stm(
  out_ecb_ump_together$documents,
  out_ecb_ump_together$vocab,
  K = 9,
  prevalence = ~ party + s(year),
  data = out_ecb_ump_together$meta,
  init.type = "Spectral"
)


# Only UMP  ----------------------------------------------------------------

# preprocess
stm_df <- speech_df %>%
  filter(ump_kw_bin==1 &
           !is.na(party) &
           party != "independent" &
           year >= 2013) %>%
  # transform and get tokens
  mutate(
    text_clean_sw = sapply(text_clean_sw, toString),
    text_clean_sw = str_remove_all(text_clean_sw, ","),
    party = factor(party)
  ) %>%
  select(-c(text, text_clean, ecb_kw, ump_kw, ecb_ump_kw))

# stm processing
processed <- textProcessor(stm_df$text_clean_sw,
                           metadata = stm_df,
                           language = "de")
out_ump <- prepDocuments(processed$documents,
                                      processed$vocab,
                                      processed$meta)

# Run topic models --------------------------------------------------------

tm_ump9 <- stm(
  out_ump$documents,
  out_ump$vocab,
  K = 9,
  prevalence = ~ party + s(year),
  data = out_ump$meta,
  init.type = "Spectral"
)

save(out_ecb_ump,tm9,out_ecb_ump_together,tm_together9,out_ump,tm_ump9, file = "tm9.RData")



