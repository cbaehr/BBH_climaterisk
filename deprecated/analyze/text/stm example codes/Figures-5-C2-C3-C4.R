## R Script Outputs ------------------------------------------------------------
# Figure 5: Top Five Topics in U.S. Immigration Lobbying, 2008--2017
# Appendix Figure C.2: Selecting the Number of Topics
# Appendix Figure C.3: Top Four Topics in Immigration Lobbying, 2008--2017
# Appendix Figure C.4: Quarterly Changes in Topic Prevalence, 2008--2017


## Instructions ----------------------------------------------------------------
# Step 1: Adjust MAIN_DIR to where README.txt is located
# Step 2: Run entire script


## IMPORTANT NOTE --------------------------------------------------------------
# This script uses Orbis' proprietary data on firms' NAICS industry. To protect 
# Orbis' proprietary data, I exclude all Orbis data and only load resulting  
# anonymous saved outputs for replication purposes. All code for preparing the 
# input data and implementing STM are documented below.


## setup -----------------------------------------------------------------------
# clean slate
rm(list = ls())
date()

# load packages
pkg <- c("tidyverse", 
         "stm", 
         "RColorBrewer", 
         "viridis", 
         "gridExtra",
         "mvtnorm")

lapply(pkg, require, character.only = TRUE)

# set main directory
MAIN_DIR <- "~/Dropbox/Research/JOP-h1b-replication"


## define functions ------------------------------------------------------------
simBetas <- function(parameters, nsims = 100){
  simbetas <- list()
  for(i in 1:length(parameters)) {
    simbetas[[i]] <- do.call(rbind, lapply(parameters[[i]],
                                           function(x) mvtnorm::rmvnorm(n = nsims, mean = x$est, sigma = x$vcov)))
  }
  return(simbetas)
}


## set parameters --------------------------------------------------------------
# set years of lobbying data
years <- seq(2008, 2017, by = 1)
years

years.string <- paste("-", first(years), "-", last(years), sep = "")
years.string

# set quarters
quarters <- c(
  "2008-Q1", "2008-Q2", "2008-Q3", "2008-Q4",
  "2009-Q1", "2009-Q2", "2009-Q3", "2009-Q4",
  "2010-Q1", "2010-Q2", "2010-Q3", "2010-Q4",
  "2011-Q1", "2011-Q2", "2011-Q3", "2011-Q4",
  "2012-Q1", "2012-Q2", "2012-Q3", "2012-Q4",
  "2013-Q1", "2013-Q2", "2013-Q3", "2013-Q4",
  "2014-Q1", "2014-Q2", "2014-Q3", "2014-Q4",
  "2015-Q1", "2015-Q2", "2015-Q3", "2015-Q4",
  "2016-Q1", "2016-Q2", "2016-Q3", "2016-Q4",
  "2017-Q1", "2017-Q2", "2017-Q3", "2017-Q4"
)
quarters

# remove years from vocab?
exclude.years <- FALSE

if(exclude.years == TRUE) {
  exclude.years.string <- "-exclude-years"  
  
} else {
  exclude.years.string <- ""
  
}

exclude.years.string

# set seed
seed <- 1234
seed
set.seed(seed)

# set number of topics
k <- 5
k

# set formula
formula.t <- as.formula("~ client_bvdid + quarter")
formula.file.string <- "-firm-quarter"

formula.t
formula.file.string

formula.est <- update(formula.t, 1:k ~ .)
formula.est

# set min threshold
low.thres <- 100
low.thres


## Prepare Data for STM --------------------------------------------------------
## NOTE: The code below show exactly how I constructed the input dataset 
## for STM using manually checked data of lobbying reports. To protect Orbis' 
## proprietary data, I am prohibited from uploading the entire data set. However, 
## lv-imm-fr-1999-2007-checked-orbis-excluded.RData and 
## lv-imm-fr-2008-2017-checked-orbis-excluded.RData in the replication files show
## what the input data looks like without Orbis variables.
#
# # load full checked data (not publicly shared)
# imm.reports.c.pre.2008 <- read_csv(paste(MAIN_DIR, "/lv-imm-fr-1999-2007-checked.csv", sep = ""))
# 
# imm.reports.c.post.2008 <- read_csv(paste(MAIN_DIR, "/lv-imm-fr-2008-2017-checked.csv", sep = ""))
# #
# # # save dataset excluding Orbis data (publicly shared)
# imm.reports.c.pre.2008.orbis.excluded <- imm.reports.c.pre.2008 %>%
#   select(-client_naics, -naics_core, -naics_core_descrip, -naics_2d) %>%
# 
# save(imm.reports.c.pre.2008.orbis.excluded, file = paste(MAIN_DIR, "lv-imm-fr-1999-2007-checked-orbis-excluded.RData ", sep = "/"))
# 
# imm.reports.c.post.2008.orbis.excluded <- imm.reports.c.post.2008 %>%
#   select(-client_naics, -naics_core, -naics_core_descrip, -naics_2d) %>%
# 
# save(imm.reports.c.post.2008.orbis.excluded, file = paste(MAIN_DIR, "lv-imm-fr-2008-2017-checked-orbis-excluded.RData", sep = "/"))
# 
# # check missing text
# pre.2008.na.text <- imm.reports.c.pre.2008 %>% 
#   filter(is.na(text) | text == "")
# nrow(pre.2008.na.text) / nrow(imm.reports.c.pre.2008) * 100
# 
# post.2008.na.text <- imm.reports.c.post.2008 %>% 
#   filter(is.na(text) | text == "")
# nrow(post.2008.na.text) / nrow(imm.reports.c.post.2008) * 100
# 
# # check missing venue
# pre.2008.na.venue <- imm.reports.c.pre.2008 %>% 
#   filter(is.na(venue) | venue == "")
# nrow(pre.2008.na.venue) / nrow(imm.reports.c.pre.2008) * 100
# 
# post.2008.na.venue <- imm.reports.c.post.2008 %>% 
#   filter(is.na(venue) | venue == "")
# nrow(post.2008.na.venue) / nrow(imm.reports.c.post.2008) * 100
# 
# # clean industry variable at the 2-digit level
# imm.reports.c <- imm.reports.c.post.2008 %>%
#   filter(year %in% years) %>%
#   mutate(naics_2d = ifelse(naics_2d == "31" | naics_2d == "32" | naics_2d == "33", "31-33", naics_2d), # Manufacturing
#          naics_2d = ifelse(naics_2d == "44" | naics_2d == "45", "44-45", naics_2d), # Retail Trade
#          naics_2d = ifelse(naics_2d == "48" | naics_2d == "49", "48-49", naics_2d), # Transportation and Warehousing
#          
#          quarter = paste(year, "-Q", str_sub(period, 1, 1), sep = ""),
#          quarter = ifelse(year < 2008 & str_detect(period, "Mid-Year"), paste(year, "-S1", sep = ""), quarter),
#          quarter = ifelse(year < 2008 & str_detect(period, "Year-End"), paste(year, "-S2", sep = ""), quarter),
#          quarter = factor(quarter, 
#                           level = quarters)) %>%
#   filter(!(is.na(naics_2d))) %>% # drop firms without 2-digit NAICS
#   mutate(year = as.factor(year),
#          client_bvdid = as.factor(client_bvdid),
#          naics_2d = as.factor(naics_2d)) %>%
#   arrange(client_bvdid, year, period)
# 
# # process text for STM
# processed <- textProcessor(imm.reports.c$text, 
#                            metadata = imm.reports.c,
#                            removenumbers = FALSE,
#                            removepunctuation = TRUE)
# 
# 
# # prepare documents for STM
# out <- prepDocuments(processed$documents, 
#                      processed$vocab, 
#                      processed$meta, 
#                      lower.thresh = low.thres)



## Figure 5: Top Five Topics in U.S. Immigration Lobbying, 2008 – 2017 ---------
## NOTE: The code below show exactly how I fit the model using the input 
## data from above.
# 
# # fit 5-topic model
# # start clock
# ptm <- proc.time()
#
# # fit model
# prev.fit <- stm(documents = out$documents, 
#                 vocab = out$vocab,
#                 K = k,
#                 prevalence = formula.t,
#                 max.em.its = 1000,
#                 data = out$meta,
#                 seed = seed,
#                 init.type = "Spectral")
# 
# # stop clock
# proc.time() - ptm 
# 
# # save
# save(prev.fit, file = paste(MAIN_DIR, "/prev-fit-k", k, 
#                             years.string, exclude.years.string, 
#                             formula.file.string,
#                             "-low-thres-", low.thres, 
#                             ".RData", sep = ""))
# 

# load anonymous resulting output data for replication
load(paste(MAIN_DIR, "/prev-fit-k", k, 
           years.string, exclude.years.string, 
           formula.file.string,
           "-low-thres-", low.thres, 
           ".RData", sep = ""))

# fix cluster order: from highest expected topic proportions to lowest
cluster.order.df <- tibble(cluster = 1:k,
                           mu = colMeans(prev.fit$theta)) %>%
  arrange(desc(mu)) %>%
  mutate(cluster_fixed = row_number())

cluster.order.vec <- cluster.order.df %>% 
  pull(cluster)

cluster.label.vec <- cluster.order.df %>% 
  pull(cluster_fixed)

cluster.mu.vec <- cluster.order.df %>% 
  pull(mu)

# set max words in each cloud
max.words.set <- 100
one.row <- TRUE
wordcloud.colors <- "black"

# plot and save
pdf(paste(MAIN_DIR, "Figure-5.pdf", sep = "/"),
    width = 9, height = 3)

layout(matrix(seq(1, k, 1), nrow = 1, byrow = TRUE),
       heights = c(1, 1))

par(mar = rep(0, 4))

set.seed(seed)

walk(cluster.order.vec, function(x){
  cloud(prev.fit, topic = x, 
        scale = c(3.6, 0.2), 
        max.words = max.words.set,
        random.order = FALSE,
        colors = wordcloud.colors)  
})

mtext(paste("Topic ", cluster.label.vec[1:k], sep = ""),
      side = 3, line = -2, outer = TRUE, cex = 1.5, font = 3,
      at = seq(1/(2 * k), 1, by = 1 / k))
mtext(paste("E(p) = ", round(cluster.mu.vec[1:k], 2), sep = ""),
      side = 3, line = -3.5, outer = TRUE, cex = 0.8, font = 3, col = "#E41A1C",
      at = seq(1/(2 * k), 1, by = 1 / k))
  
dev.off()



## Figure C.4: Quarterly Changes in Topic Prevalence, 2008--2017 ---------------
## NOTE: The code below show exactly how I compute quarterly changes using the input 
## data and fitted model results from above.
# 
# # estimate effect on topics
# # set parameters
# key.var <- paste("quarter", quarters, sep = "")[-1]
# 
# # estimate
# estimate.out <- estimateEffect(formula.est, 
#                                prev.fit, 
#                                meta = out$meta, 
#                                uncertainty = "Global")
# 
# # extract estimates into a data frame
# estimate.sum <- summary(estimate.out)
# 
# coef.df <- map_df(1:k, function(x){
#   map_df(key.var, function(y){
#     out <- tibble(topic = x,
#                   topic_fixed = cluster.order.df[match(x, cluster.order.df$cluster), "cluster_fixed"]$cluster_fixed,
#                   var = y,
#                   coef = round(estimate.sum$tables[[x]][y, 1], 3),
#                   se = round(estimate.sum$tables[[x]][y, 2], 3),
#                   t = round(estimate.sum$tables[[x]][y, 3], 3),
#                   p = round(estimate.sum$tables[[x]][y, 4], 3))
#   })
#   
# }) %>%
#   arrange(topic_fixed)
# 
# # simulate proportions over time
# # set covariate of focus
# covariate.focus <- "quarter"  
# first.period <- paste("quarter", quarters[[1]], sep = "")
# periods <- quarters
# 
# # create dummies for FEs
# model.df.full <- imm.reports.c %>%
#   dplyr::select(client_bvdid, !!as.name(covariate.focus)) %>%
#   mutate(client_bvdid = paste("client_bvdid", client_bvdid, sep = ""),
#          !!covariate.focus := paste(covariate.focus, !!as.name(covariate.focus), sep = ""),
#          new = 1,
#          temp = row_number()) %>%
#   spread(client_bvdid, new, fill = 0) %>%
#   dplyr::select(-temp) %>%
#   mutate(new = 1,
#          temp = row_number()) %>%
#   spread(!!as.name(covariate.focus), new, fill = 0) %>%
#   dplyr::select(-temp) 
# 
# # remove base period and add intercept
# model.df.full <- model.df.full %>%
#   dplyr::select(-!!as.name(first.period))
# 
# model.df.full[,1] <- 1
# 
# # convert to matrix
# cmat <- as.matrix(model.df.full)
# 
# # set seed
# set.seed(seed)
# 
# # simulate coefficients
# simbetas <- simBetas(estimate.out$parameters, nsims = 1000)
# ci.level <- 0.95
# offset <- (1 - ci.level) / 2
# 
# # start clock
# ptm <- proc.time()
# 
# # simulate across each period
# sim.sum.point.df <- map_df(periods, function(x){
#   
#   # and each topic
#   out <- map_df(1:k, function(y) {
#     
#     # print
#     cat(paste("Simulating predicted values for period ", x, " and Topic ", y,
#               "\n", sep = ""))
#     
#     # create temp data
#     newd <- cmat
#     
#     if (paste(covariate.focus, x, sep = "") == first.period){
#       
#       # set all other periods as 0
#       leftover <- paste(covariate.focus, setdiff(periods, x), sep = "")
#       leftover <- leftover[leftover != first.period]
#       
#       newd[,leftover] <- 0
#       
#     } else {
#       # set year of focus as 1
#       newd[,paste(covariate.focus, x, sep = "")] <- 1
#       
#       # set all other years as 0
#       leftover <- paste(covariate.focus, setdiff(periods, x), sep = "")
#       leftover <- leftover[leftover != first.period]
#       
#       newd[,leftover] <- 0
#     }
#     
#     # matrix multiplication
#     sims <- newd %*% t(simbetas[[which(estimate.out$topics == y)]])
#     
#     # average over observations for each draw
#     sims.ave <- colMeans(sims)
#     
#     sims.sum <- tibble(period = x,
#                        topic = y,
#                        ev = sims.ave)
#   })
# })
# 
# # stop clock
# proc.time() - ptm 
# 
# # reorder cluster number to be consistent
# sim.df <- sim.sum.point.df %>%
#   mutate(topic_fixed = cluster.order.df[match(topic, cluster.order.df$cluster), "cluster_fixed"]$cluster_fixed) %>%
#   arrange(topic_fixed) 
# 
# # save
# save(sim.df, file = paste(OUT_DIR, "sim-df-k", k, 
#                           years.string, exclude.years.string, 
#                           formula.file.string,
#                           "-low-thres-", low.thres, 
#                           ".RData", sep = ""))

# load anonymous resulting output data for replication  
load(file = paste(MAIN_DIR, "/sim-df-k", k, 
                  years.string, exclude.years.string, 
                  formula.file.string,
                  "-low-thres-", low.thres, 
                  ".RData", sep = ""))
  
# summarize estimates by period and topic
# set parameters
ci.level <- 0.95
offset <- (1 - ci.level) / 2

sim.level <- sim.df %>%
  group_by(period, topic_fixed) %>%
  summarize(mean = mean(ev), 
            lwr = quantile(ev, offset),
            upr = quantile(ev, 1 - offset)) %>%
  mutate(topic_fixed = paste("Topic ", topic_fixed, sep = ""),
         period_label = if_else(str_detect(period, "Q1"), period, str_sub(period, 6, 7))) %>%
  ungroup()

# plot Topic 2: H-1B visas 
# set parameter
axis.title.size <- 16

p.levels.visa <- ggplot(data = sim.level %>% filter(topic_fixed == "Topic 2"),
                        aes(x = period, 
                            y = mean,
                            ymin = lwr, 
                            ymax = upr,
                            group = factor(topic_fixed))) +
  geom_rect(xmin = length(quarters) - 3.5, xmax = Inf, 
            ymin = -Inf, ymax = Inf, 
            fill = "grey90", color = "grey90",
            alpha = 0.1) +
  geom_pointrange(size = 0.7) +
  geom_path() +
  geom_hline(yintercept = 0,
             color = I(hsv(0/12, 7/12, 7/12)), 
             linetype = "dashed") +
  scale_x_discrete("", 
                   labels = sim.level %>% filter(topic_fixed == "Topic 2") %>% pull(period_label)) +
  scale_y_continuous("Simulated Topic Proportions", limits = c(0, 0.4)) +
  theme_bw() +
  ggtitle("Topic 2: H-1B Visas") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = axis.title.size + 4,
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(0, 0, 20, 0)),
    plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"),
    axis.title.y = element_text(size = axis.title.size,
                                margin = margin(0, 20, 0, 0)),
    axis.text.y = element_text(size = axis.title.size),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = axis.title.size + 4,
                              margin = margin(20, 0, 20, 0)),
    panel.grid = element_blank()) 

# add annotation
p.levels.visa <- p.levels.visa + 
  annotate("text", 
           x = length(quarters) - 3.5, 
           y = 0.37,
           label = "Trump Admin.", 
           color = "black",
           hjust = 0,
           size = 3) + 
  annotate("segment", 
           x = length(quarters) - 3.5, xend = length(quarters), 
           y = 0.39, yend = 0.39, 
           colour = "black",
           arrow = arrow(length = unit(0.2,"cm"), 
                         ends = "last", type = "closed"))

# plot Topic 4: High-Skilled Immigration Acts
p.levels.acts <- ggplot(data = sim.level %>% filter(topic_fixed == "Topic 4"),
                        aes(x = period, 
                            y = mean,
                            ymin = lwr, 
                            ymax = upr,
                            group = factor(topic_fixed))) +
  geom_rect(xmin = length(quarters) - 3.5, xmax = Inf, 
            ymin = -Inf, ymax = Inf, 
            fill = "grey90", color = "grey90",
            alpha = 0.1) +
  geom_pointrange(size = 0.7) +
  geom_path() +
  geom_hline(yintercept = 0,
             color = I(hsv(0/12, 7/12, 7/12)), 
             linetype = "dashed") +
  scale_x_discrete("", 
                   labels = sim.level %>% filter(topic_fixed == "Topic 4") %>% pull(period_label)) +
  scale_y_continuous("Simulated Topic Proportions", limits = c(0, 0.4)) +
  theme_bw() +
  ggtitle("Topic 4: High-Skilled Immigration Acts") +
  theme(
    legend.position = "none",
    plot.title = element_text(size = axis.title.size + 4,
                              face = "bold",
                              hjust = 0.5,
                              margin = margin(0, 0, 20, 0)),
    plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"),
    axis.title.y = element_text(size = axis.title.size,
                                margin = margin(0, 20, 0, 0)),
    axis.text.y = element_text(size = axis.title.size),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(size = axis.title.size + 4,
                              margin = margin(20, 0, 20, 0)),
    panel.grid = element_blank()) 

# add annotations
p.levels.acts <- p.levels.acts + 
  annotate("text", 
           x = length(quarters) - 3.5, 
           y = 0.37,
           label = "Trump Admin.", 
           color = "black",
           hjust = 0,
           size = 3) + 
  annotate("segment", 
           x = length(quarters) - 3.5, xend = length(quarters), 
           y = 0.39, yend = 0.39, 
           colour = "black",
           arrow = arrow(length = unit(0.2,"cm"), 
                         ends = "last", type = "closed"))

# combine plots and save
pdf(file = paste(MAIN_DIR, "Appendix-Figure-C.4.pdf", sep = "/"),
    width = 10, height = 8.2)
grid.arrange(p.levels.visa, p.levels.acts, ncol = 1)
dev.off()



## Figure C.2: Selecting the Number of Topics ----------------------------------
## NOTE: The code below show exactly how I select topic numbers using the input 
## data from above.
# 
# # search optimal number of clusters
# # start clock
# ptm <- proc.time()
# 
# # search K
# storage <- searchK(out$documents, out$vocab,
#                    K = c(3:15),
#                    max.em.its = 1000,
#                    prevalence = formula.t,
#                    init.type = "Spectral",
#                    data = out$meta,
#                    seed = seed)
# 
# # stop clock
# proc.time() - ptm  
# 
# # save output
# save(storage, file = paste(MAIN_DIR, "/searchK-storage",
#                            years.string, exclude.years.string,
#                            formula.file.string,
#                            "-low-thres-", low.thres,
#                            ".RData", sep = ""))  

# load anonymous resulting output data for replication 
load(file = paste(MAIN_DIR, "/searchK-storage",
                  years.string, exclude.years.string,
                  formula.file.string,
                  "-low-thres-", low.thres,
                  ".RData", sep = ""))    
  
# set parameter
axis.title.size <- 12

# plot
f.search.k <- ggplot(data.frame(storage$results),
                     aes(x = unlist(semcoh), y = unlist(exclus))) +
  geom_text(label = storage$results$K, size = 3) +
  scale_x_continuous("Semantic Coherence") +
  scale_y_continuous("Exclusivity") +
  theme_bw() +
  theme(plot.title = element_text(size = axis.title.size,
                                  face = "bold",
                                  margin = margin(0, 0, 20, 0),
                                  hjust = 0.5),
        axis.title.y = element_text(size = axis.title.size,
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = axis.title.size,
                                    margin = margin(20, 0, 0, 0)),
        axis.text = element_text(size = axis.title.size - 2),
        plot.margin = margin(0.1, 0.3, 0.1, 0.1, "cm"),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# save
pdf(paste(MAIN_DIR, "Appendix-Figure-C.2.pdf", sep = "/"),
    width = 4, height = 3)
f.search.k
dev.off()



## Figure C.3: Top Four Topics in U.S. Immigration Lobbying, 2008-2017 ---------
## NOTE: The code below show exactly how I fit the model using the input 
## data from above.
# reset number of topics
k <- 4
#   
# # start clock
# ptm <- proc.time()
# 
# # fit model
# prev.fit <- stm(documents = out$documents, 
#                 vocab = out$vocab,
#                 K = k,
#                 prevalence = formula.t,
#                 max.em.its = 1000,
#                 data = out$meta,
#                 seed = seed,
#                 init.type = "Spectral")
# 
# # stop clock
# proc.time() - ptm 
# 
# # save
# save(prev.fit, file = paste(MAIN_DIR, "/prev-fit-k", k, 
#                             years.string, exclude.years.string, 
#                             formula.file.string,
#                             "-low-thres-", low.thres, 
#                             ".RData", sep = ""))

# load anonymous resulting output data for replication 
load(paste(MAIN_DIR, "/prev-fit-k", k, 
           years.string, exclude.years.string, 
           formula.file.string,
           "-low-thres-", low.thres, 
           ".RData", sep = ""))

# fix cluster order: from highest expected topic proportions to lowest
cluster.order.df <- tibble(cluster = 1:k,
                           mu = colMeans(prev.fit$theta)) %>%
  arrange(desc(mu)) %>%
  mutate(cluster_fixed = row_number())

cluster.order.vec <- cluster.order.df %>% 
  pull(cluster)

cluster.label.vec <- cluster.order.df %>% 
  pull(cluster_fixed)

cluster.mu.vec <- cluster.order.df %>% 
  pull(mu)

# set max words in each cloud
max.words.set <- 100
one.row <- TRUE
wordcloud.colors <- "black"

# plot and save
pdf(paste(MAIN_DIR, "Appendix-Figure-C.3.pdf", sep = "/"),
    width = 9, height = 3)

layout(matrix(seq(1, k, 1), nrow = 1, byrow = TRUE),
       heights = c(1, 1))

par(mar = rep(0, 4))

set.seed(seed)

walk(cluster.order.vec, function(x){
  cloud(prev.fit, topic = x, 
        scale = c(3.6, 0.2), 
        max.words = max.words.set,
        random.order = FALSE,
        colors = wordcloud.colors)  
})

mtext(paste("Topic ", cluster.label.vec[1:k], sep = ""),
      side = 3, line = -2, outer = TRUE, cex = 1.5, font = 3,
      at = seq(1/(2 * k), 1, by = 1 / k))
mtext(paste("E(p) = ", round(cluster.mu.vec[1:k], 2), sep = ""),
      side = 3, line = -3.5, outer = TRUE, cex = 0.8, font = 3, col = "#E41A1C",
      at = seq(1/(2 * k), 1, by = 1 / k))

dev.off()

