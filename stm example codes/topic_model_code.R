# Topic models code

# load
load("stm_modelselect_df.RData")
load("stm.RData")

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
  mutate(t = row_number(),) %>%
  rename("half_year" =".")
meta <- meta %>%
  left_join(time,by="half_year")
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




# Evaluation --------------------------------------------------------------



# Plot
eval %>%
  select(1, 3:5, 7) %>%
  unnest() %>%
  gather(Metric, Value,-K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_point(show.legend = F) +
  geom_line(size = 1.5,
            show.legend = FALSE) +
  facet_wrap( ~ Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics") +
  theme_bw()+
  scale_color_brewer(palette = "Dark2")


# Semantic coherence & exclusivity
eval %>%
  select(1:3) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(
    x = `Semantic Coherence`,
    y = Exclusivity,
    label = K
  )) +
  geom_point(size = 2, show.legend = F) +
  ggrepel::geom_text_repel(show.legend = F) +
  labs(title = "Comparing exclusivity and semantic coherence") +
  theme_bw()

tidy_tm <- tidy(topic_model)

tidy_tm %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")





# Final topic models ------------------------------------------------------

setwd("/Users/vincentheddesheimer/Documents/Princeton/2_Spring 2022/585 IPE/UMP Speech Paper/UMP ParlSpeech/data")
load("tm9.RData")


# label topics
labelTopics(tm9)
labelTopics(tm_together9)
labelTopics(tm_ump9)

# Estimate topic relationship
out_ecb_ump_together$meta$party <- as.factor(out_ecb_ump_together$meta$party)
prep <- estimateEffect(1:9 ~ party + s(year), tm_together9,
                       meta = out_ecb_ump_together$meta, uncertainty = "Global")

# tidy
# devtools::install_github("mikajoh/tidystm", dependencies = TRUE)
library(tidystm)

stm <- extract.estimateEffect(prep, "party", model = tm_together9, method = "pointestimate")
stm %>%
  ggplot(aes(x=estimate,y=factor(topic),color=covariate.value,xmin=ci.lower,xmax=ci.upper))+
  geom_pointrange(position=position_dodge(.7)) +
  geom_vline(xintercept = 0, color="red") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Probability",
       y = "Topics") +
  theme(legend.position="bottom")

stm_time <- extract.estimateEffect(prep, "year", model = tm_together9, method = "pointestimate")
stm_time %>%
  ggplot(aes(x=covariate.value,y=estimate,color=factor(topic),ymin=ci.lower,ymax=ci.upper))+
  geom_pointrange() +
  facet_wrap(~factor(topic),scales = "free") +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Year",
       y = "Probability") +
  theme(legend.position="bottom")


