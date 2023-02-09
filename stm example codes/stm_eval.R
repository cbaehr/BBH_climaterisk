# Unconventional Monetary Policy and Parliament Speeches

setwd('/scratch/network/vh4264/UMP_Speech')

library(tidyverse)
library(stm)
library(tm)
library(furrr)



# Topic Model evaluation

load("speech_df.RData")

# preprocess
stm_df <- speech_df %>%
  filter(ecb_ump_kw_bin==1 &
           !is.na(party) & 
           party != "independent" & 
           year >= 2010) %>%
  # transform and get tokens
  mutate(text_clean_sw = sapply(text_clean_sw, toString),
         text_clean_sw = str_remove_all(text_clean_sw,","),
         party = factor(party)) %>%
  select(-c(text,text_clean,ecb_kw,ump_kw,ecb_ump_kw))

# stm processing
processed <- textProcessor(stm_df$text_clean_sw, metadata = stm_df,
                           language = "de")
out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# create numeric time variable that captures half_year
time <- meta %>%
  select(half_year) %>%
  unlist() %>%
  tibble() %>%
  unique() %>%
  mutate(time = row_number(),) %>%
  rename("half_year" =".")
meta <- meta %>%
  left_join(time)
rm(time)



# Select best topic number ------------------------------------------------

# 
# # Try different values for K (number of topics)
# storage <- searchK(out$documents, out$vocab, 
#                    K = c(3:30),
#                    prevalence = ~ party + s(time),
#                    data = meta)
# eval <- storage$results
# eval <- eval %>%
#   rename('Exclusivity'='exclus',
#          'Semantic Coherence'='semcoh',
#          'Residual'='residual',
#          'Held-Out Likelihood'='heldout',
#          'Lower Bound'='lbound') %>%
#   tibble()
# 
# 
# # save
# # setwd("data/")
# save(eval, file="stm_modelselect_df.RData")
# save(storage, file = "stm.RData")
# 


# Run topic models --------------------------------------------------------

plan(multiprocess)

topic_models <- data_frame(K = c(3:12)) %>%
  mutate(topic_model = future_map(K, ~stm(out$documents, out$vocab, 
                                          K = .,
                                          prevalence = ~ party + s(time),
                                          data = meta)))

save(topic_models,file = "topic_models.RData")

