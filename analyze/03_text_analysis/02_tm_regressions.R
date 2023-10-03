### Firms & Lobbying
### Topic Models Regression

rm(list=ls())

# tidystm
# devtools::install_github("mikajoh/tidystm", dependencies = TRUE)
# Load packages
pacman::p_load(tidyverse, data.table, stm, tm, tidystm)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr" ) {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="vh4264" ) {setwd("/home/vh4264/bbh1/")}



# Load topic model output -------------------------------------------------

if(Sys.info()["user"]=="vincentheddesheimer" ) {
  load("data/03_final/topicmodels.RData")
  load("data/03_final/prepDocuments.RData")
}
if(Sys.info()["user"]=="vh4264" ) {
  load("output/topicmodels.RData")
  load("output/prepDocuments.RData")
  }

# label topics
labelTopics(prev.fit)

# topic salience
plot(prev.fit, type = "summary", xlim = c(0, 1.2), n = 6)


# Estimate effect ---------------------------------------------------------

prep <- estimateEffect(1:5 ~ op_expo_ew_y + rg_expo_ew_y + ph_expo_ew_y + ebit + I(ebit/assets) + us_dummy + total_lobby + factor(year) + factor(industry) + factor(industry_year),
                       prev.fit, metadata = out$meta, uncertainty = "Global")



# Summarize ---------------------------------------------------------------

summary(prep, topics = 1)



# Functions for tidy resultstable -----------------------------------------

rmvnorm<-function(n,mu,Sigma,chol.Sigma=chol(Sigma)) {
  E<-matrix(rnorm(n*length(mu)),n,length(mu))
  t(  t(E%*%chol.Sigma) +c(mu))
}

tidy.estimateEffect <- function(object, topics=NULL, nsim=500, ...) {
  if(is.null(topics)) topics <- object$topics
  if(any(!(topics %in% object$topics))) {
    stop("Some topics specified with the topics argument are not available in this estimateEffect object.")
  }
  
  tidy_df <- data.frame(
    Topic = character(),
    Parameter = character(),
    Estimate = numeric(),
    Std_Error = numeric(),
    t_value = numeric(),
    Pr_t = numeric(),
    CI_90_low = numeric(),
    CI_90_high = numeric(),
    CI_95_low = numeric(),
    CI_95_high = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:length(topics)) {
    topic <- topics[i]
    sims <- lapply(object$parameters[[which(object$topics==topic)]], function(x) rmvnorm(nsim, x$est, x$vcov))
    sims <- do.call(rbind,sims)
    est <- colMeans(sims)
    se <- sqrt(apply(sims,2, stats::var))
    tval <- est/se
    rdf <- nrow(object$data) - length(est)
    p <- 2 * stats::pt(abs(tval), rdf, lower.tail = FALSE)
    
    # Calculate confidence intervals
    q_90 <- qt(0.95, rdf)  # For 90% CI
    q_95 <- qt(0.975, rdf) # For 95% CI
    
    ci_90_low <- est - q_90 * se
    ci_90_high <- est + q_90 * se
    
    ci_95_low <- est - q_95 * se
    ci_95_high <- est + q_95 * se
    
    # Create a temporary dataframe for this topic
    tmp_df <- data.frame(
      Topic = topic,
      Parameter = attr(object$parameters[[1]][[1]]$est, "names"),
      Estimate = est,
      Std_Error = se,
      t_value = tval,
      Pr_t = p,
      CI_90_low = ci_90_low,
      CI_90_high = ci_90_high,
      CI_95_low = ci_95_low,
      CI_95_high = ci_95_high
    )
    
    # Binding the temporary dataframe to the accumulated results
    tidy_df <- rbind(tidy_df, tmp_df)
  }
  
  return(tidy_df)
}



# Create tidy results -----------------------------------------------------

tidy1 <- tidy.estimateEffect(prep, topics = 1)
tidy2 <- tidy.estimateEffect(prep, topics = 2)
tidy3 <- tidy.estimateEffect(prep, topics = 3)
tidy4 <- tidy.estimateEffect(prep, topics = 4)
tidy5 <- tidy.estimateEffect(prep, topics = 5)

tidy_plot <- rbind(tidy1, tidy2, tidy3, tidy4, tidy5)


# Plot --------------------------------------------------------------------

tidy_plot |>
  filter(Parameter %in% c("op_expo_ew_y", "rg_expo_ew_y", "ph_expo_ew_y")) |>
  mutate(
    Parameter = case_match(
      Parameter,
      "op_expo_ew_y" ~ "Opportunity",
      "rg_expo_ew_y" ~ "Regulatory",
      "ph_expo_ew_y" ~ "Physical"
    ),
    Topic = case_match(
      Topic,
      4 ~ "Topic 4:
      (issu, relat, energi,
      legisl, appropri, water)",
      2 ~ "Topic 2:
      (act, regul, epa,
      air, water, rule)",
      3 ~ "Topic 3:
      (act, energi, clean,
      climat, chang, american)",
      1 ~ "Topic 1:
      (fuel, gas, act,
      relat, natur, oil)",
      5 ~ "Topic 5:
      (energi, act, bill,
      effici, electr, nuclear)"
    )
  ) |>
  ggplot(aes(y=Topic, x=Estimate))+
  geom_vline(xintercept = 0, color="red", linetype = "dotted") +
  geom_errorbar(aes(xmin = CI_95_low, xmax = CI_95_high), width = 0, linewidth = .5) +
  geom_errorbar(aes(xmin = CI_90_low, xmax = CI_90_low), width = 0, linewidth = 1) +
  geom_point(shape = 21, fill = "white", size = 2) +
  theme_bw() +
  labs(x = "Estimate",
       y = "Topic") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 15)) +
  facet_wrap(~ Parameter, scales = "free_x")

ggsave("results/figures/text_analysis/topicmodels.pdf", width=8, height = 5.5)


### END