### Firms & Lobbying
### Topic Models Regression

rm(list=ls())

# tidystm
devtools::install_github("mikajoh/tidystm", dependencies = TRUE)
# Load packages
pacman::p_load(tidyverse, data.table, stm, tm, tidystm)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="vh4264" ) {setwd("/home/vh4264/bbh1/")}



# Load topic model output -------------------------------------------------


# save
load("data/03_final/topicmodels.RData")


# label topics
labelTopics(prev.fit)

# Estimate topic relationship
prep <- estimateEffect(1:5 ~ opexpo_q + rgexpo_q + phexpo_q, prev.fit, uncertainty = "Global")


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

