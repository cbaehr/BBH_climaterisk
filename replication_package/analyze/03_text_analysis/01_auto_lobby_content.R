### Firms & Lobbying
### Text Analysis
### Transport manufacturing

rm(list=ls())

# Load packages
pacman::p_load(tidyverse, readxl, quanteda, quanteda.textstats, stm, wordcloud, quanteda.textplots, ggplot2, viridis)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="vincentheddesheimer" ) {setwd("~/Dropbox (Princeton)/BBH/BBH1/")}
if(Sys.info()["user"]=="christianbaehr") {setwd("/Users/christianbaehr/Dropbox/BBH/BBH1/")}

####Data preparation -----

#Load data
auto <- read_excel("data/03_final/issues_texts/auto.xlsx")

# #Create climate binary variable
# auto$CLI <- grepl("ENV|CAW|ENG|FUE", auto$issue_code)

#Filter data for climate lobbying only 
auto <- auto %>% 
  filter(CLI_quarter == 1)

# auto <- auto %>%
#   filter(
#     complete.cases(
#       op_expo_ew, rg_expo_ew, ph_expo_ew
#     )
#   )

###Create overall corpus ------
auto_corpus <-corpus(auto$issue_text, docnames = seq_len(nrow(auto)))

# Add document-level information
docvars(auto_corpus) <- auto[, c("gvkey", "conm", "year", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")]

####Create sub-corpus for each exposure measure -------
# Get the document variables from the corpus
opp <- docvars(auto_corpus, "op_expo_ew")
reg <- docvars(auto_corpus, "rg_expo_ew")
phy <- docvars(auto_corpus, "ph_expo_ew")

# Opportunity -----
# Calculate the median of the document variable
med_opp <- median(opp, na.rm = TRUE)

# Create a logical condition for grouping
condition_opp <- opp > med_opp

# Split the corpus into two groups based on the condition
above_oppmed_corpus <- subset(auto_corpus, condition_opp)
below_oppmed_corpus <- subset(auto_corpus, !condition_opp)

#Make opp dfm 
# Convert the corpus to tokens
tokens_opp_above <- tokens(above_oppmed_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_opp_above <- tokens_select(tokens_opp_above, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_opp_above <- dfm(tokens_opp_above)

tokens_opp_below <- tokens(below_oppmed_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_opp_below <- tokens_select(tokens_opp_below, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_opp_below <- dfm(tokens_opp_below)

## Opp Keywords  
#Set opp keywords
keywords_oppv2 <- c("alternative", "tax", "charging", "technology", "electric", "fuel", "credit", "plug", "infrastructure", "hybrid")

# First check which keywords exist in the dfm
available_keywords_below <- intersect(keywords_oppv2, colnames(dfm_opp_below))
available_keywords_above <- intersect(keywords_oppv2, colnames(dfm_opp_above))

# Use only available keywords when calculating frequencies
keyword_opp_below <- colSums(dfm_opp_below[, available_keywords_below])
keyword_opp_above <- colSums(dfm_opp_above[, available_keywords_above])

# Create a complete vector with zeros for missing keywords
keyword_opp_below_complete <- numeric(length(keywords_oppv2))
names(keyword_opp_below_complete) <- keywords_oppv2
keyword_opp_below_complete[available_keywords_below] <- keyword_opp_below

keyword_opp_above_complete <- numeric(length(keywords_oppv2))
names(keyword_opp_above_complete) <- keywords_oppv2
keyword_opp_above_complete[available_keywords_above] <- keyword_opp_above

# Use the complete vectors for further calculations
keyword_opp_below <- keyword_opp_below_complete
keyword_opp_above <- keyword_opp_above_complete

# Calculate total words in the corpus
total_opp_above <- sum(dfm_opp_above)

total_opp_below <- sum(dfm_opp_below)

#Calculate ratio of each keyword to total words
ratio_opp_above <- (keyword_opp_above / total_opp_above)

ratio_opp_below <- (keyword_opp_below / total_opp_below)

##Plot the results 
#Create df to plot
opp_df <- data.frame(keywords = keywords_oppv2, above = ratio_opp_above, below = ratio_opp_below)
plot_opp_df <- reshape2::melt(opp_df, id.vars = "keywords")

# Order by the ratio of the above median to below median
plot_opp_df <- plot_opp_df |>
  mutate(
    variable = ifelse(variable == "above", "Above Median", "At/Below Median"),
    variable = factor(variable, levels=c("At/Below Median", "Above Median")),
    keywords = reorder(keywords, value),
    exposure = "Opportunity"
    )

#Plot
plot_opp_df |>
  ggplot(aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("black", "darkgrey"), breaks = c("Above Median", "At/Below Median")) +
  labs(x = "Keywords", y = "Ratio", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 12),
        legend.position = "bottom") +
  coord_flip()

ggsave("results/figures/text_analysis/median_opportunity.pdf", width = 8, height = 4)

# Regulatory -----
# Calculate the median of the document variable
med_reg <- median(reg, na.rm = TRUE)

# Create a logical condition for grouping
condition_reg <- reg > med_reg  # or < if you want the opposite comparison

# Split the corpus into two groups based on the condition
above_regmed_corpus <- subset(auto_corpus, condition_reg)
below_regmed_corpus <- subset(auto_corpus, !condition_reg)

#Make reg dfm
# Convert the corpus to tokens
tokens_reg_above <- tokens(above_regmed_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_reg_above <- tokens_select(tokens_reg_above, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_reg_above <- dfm(tokens_reg_above)

tokens_reg_below <- tokens(below_regmed_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_reg_below <- tokens_select(tokens_reg_below, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_reg_below <- dfm(tokens_reg_below)

##Keywords
#Set reg keywords
keywords_reg <- c("implementation", "rule", "efficiency", "cafe", "emissions", "harmonization", "standards", "regulation", "monitor", "price")

# First check which keywords exist in the dfm
available_keywords_reg_below <- intersect(keywords_reg, colnames(dfm_reg_below))
available_keywords_reg_above <- intersect(keywords_reg, colnames(dfm_reg_above))

# Use only available keywords when calculating frequencies
keyword_reg_below <- colSums(dfm_reg_below[, available_keywords_reg_below])
keyword_reg_above <- colSums(dfm_reg_above[, available_keywords_reg_above])

# Create a complete vector with zeros for missing keywords
keyword_reg_below_complete <- numeric(length(keywords_reg))
names(keyword_reg_below_complete) <- keywords_reg
keyword_reg_below_complete[available_keywords_reg_below] <- keyword_reg_below

keyword_reg_above_complete <- numeric(length(keywords_reg))
names(keyword_reg_above_complete) <- keywords_reg
keyword_reg_above_complete[available_keywords_reg_above] <- keyword_reg_above

# Use the complete vectors for further calculations
keyword_reg_below <- keyword_reg_below_complete
keyword_reg_above <- keyword_reg_above_complete

# Calculate total words in the corpus
total_reg_above <- sum(dfm_reg_above)

total_reg_below <- sum(dfm_reg_below)

#Calculate ratio of each keyword to total words
ratio_reg_above <- (keyword_reg_above / total_reg_above)

ratio_reg_below <- (keyword_reg_below / total_reg_below)

##Plot the results 
#Create df to plot
reg_df <- data.frame(keywords = keywords_reg, above = ratio_reg_above, below = ratio_reg_below)
plot_reg_df <- reshape2::melt(reg_df, id.vars = "keywords")

# Order by the ratio of the above median to below median
plot_reg_df <- plot_reg_df |>
  mutate(
    variable = ifelse(variable == "above", "Above Median", "At/Below Median"),
    variable = factor(variable, levels=c("At/Below Median", "Above Median")),
    keywords = reorder(keywords, value),
    exposure = "Regulatory"
  )

#Plot
plot_reg_df |>
  ggplot(aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("black", "darkgrey"), breaks = c("Above Median", "At/Below Median")) +
  labs(x = "Keywords", y = "Ratio", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 12),
        legend.position = "bottom") +
  coord_flip()

ggsave("results/figures/text_analysis/median_regulatory.pdf", width = 8, height = 4)


# Physical  -----
# Calculate the median of the document variable
med_phy <- median(phy, na.rm = TRUE)

# Create a logical condition for grouping
condition_phy <- phy > med_phy  # or < if you want the opposite comparison

# Split the corpus into two groups based on the condition
above_phymed_corpus <- subset(auto_corpus, condition_phy)
below_phymed_corpus <- subset(auto_corpus, !condition_phy)

#Make phy dfm
# Convert the corpus to tokens
tokens_phy_above <- tokens(above_phymed_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_phy_above <- tokens_select(tokens_phy_above, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_phy_above <- dfm(tokens_phy_above)

tokens_phy_below <- tokens(below_phymed_corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_phy_below <- tokens_select(tokens_phy_below, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_phy_below <- dfm(tokens_phy_below)

##Keywords
#Set phy keywords
keywords_phy <- c("gas", "clean", "climate", "greenhouse", "warming", "environmental", "global", "air", "pollution", "fossil")

# First check which keywords exist in the dfm
available_keywords_phy_below <- intersect(keywords_phy, colnames(dfm_phy_below))
available_keywords_phy_above <- intersect(keywords_phy, colnames(dfm_phy_above))

# Use only available keywords when calculating frequencies
keyword_phy_below <- colSums(dfm_phy_below[, available_keywords_phy_below])
keyword_phy_above <- colSums(dfm_phy_above[, available_keywords_phy_above])

# Create a complete vector with zeros for missing keywords
keyword_phy_below_complete <- numeric(length(keywords_phy))
names(keyword_phy_below_complete) <- keywords_phy
keyword_phy_below_complete[available_keywords_phy_below] <- keyword_phy_below

keyword_phy_above_complete <- numeric(length(keywords_phy))
names(keyword_phy_above_complete) <- keywords_phy
keyword_phy_above_complete[available_keywords_phy_above] <- keyword_phy_above

# Use the complete vectors for further calculations
keyword_phy_below <- keyword_phy_below_complete
keyword_phy_above <- keyword_phy_above_complete

# Calculate total words in the corpus
total_phy_above <- sum(dfm_phy_above)

total_phy_below <- sum(dfm_phy_below)

#Calculate ratio of each keyword to total words
ratio_phy_above <- (keyword_phy_above / total_phy_above)

ratio_phy_below <- (keyword_phy_below / total_phy_below)

##Plot the results 
#Create df to plot
phy_df <- data.frame(keywords = keywords_phy, above = ratio_phy_above, below = ratio_phy_below)
plot_phy_df <- reshape2::melt(phy_df, id.vars = "keywords")

# Order by the ratio of the above median to below median
plot_phy_df <- plot_phy_df |>
  mutate(
    variable = ifelse(variable == "above", "Above Median", "At/Below Median"),
    variable = factor(variable, levels=c("At/Below Median", "Above Median")),
    keywords = reorder(keywords, value),
    exposure = "Physical"
  )

#Plot
plot_phy_df |>
  ggplot(aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("black", "darkgrey"), breaks = c("Above Median", "At/Below Median")) +
  labs(x = "Keywords", y = "Ratio", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 12),
        legend.position = "bottom") +
  coord_flip()

ggsave("results/figures/text_analysis/median_physical.pdf", width = 8, height = 4)



# Combine into one stacked plot -------------------------------------------

#Combine into one df
plot_df <- rbind(plot_opp_df, plot_reg_df, plot_phy_df)

#Plot
plot_df |>
  mutate(exposure = factor(exposure, levels = c("Opportunity", "Regulatory", "Physical"))) |>
  ggplot(aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("black", "darkgrey"), breaks = c("Above Median", "At/Below Median")) +
  labs(x = "Keywords", y = "Ratio", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~exposure, ncol = 1, scales = "free_y") +
  coord_flip()

ggsave("results/figures/text_analysis/median_combined.pdf", width = 8, height = 10)

#Plot
plot_df |>
  mutate(exposure = factor(exposure, levels = c("Opportunity", "Regulatory", "Physical"))) |>
  ggplot(aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("black", "gray70"), breaks = c("Above Median", "At/Below Median")) +
  labs(x = "Keywords", y = "Relative Frequency within Climate Lobbying Report", fill = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~exposure, ncol = 3, scales = "free") +
  coord_flip() +
  # x axis labels turn by 45 degrees
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("results/figures/text_analysis/median_combined_wide.pdf", width = 8, height = 5)



###################OLD CODE
# 
# #Calculate quartiles for opportunity 
# q_opp <- quantile(opp, probs = c(0.25, 0.5, 0.75))
# 
# #Create logical condition for grouping based on quartiles
# q1_opp <- auto_corpus[opp <= q_opp[1]]
# q2_opp <- auto_corpus[opp > q_opp[1] & opp <=q_opp[2]]
# q3_opp <- auto_corpus[opp > q_opp[2] & opp <=q_opp[3]]
# q4_opp <- auto_corpus[opp > q_opp[3]]
# 
# 
# #Make opp dfm
# # Convert the corpus to tokens
# tokens_q1_opp <- tokens(q1_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q1_opp <- tokens_select(tokens_q1_opp, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q1_opp <- dfm(tokens_q1_opp)
# 
# 
# tokens_q2_opp <- tokens(q2_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q2_opp <- tokens_select(tokens_q2_opp, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q2_opp <- dfm(tokens_q2_opp)
# 
# 
# tokens_q3_opp <- tokens(q3_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q3_opp <- tokens_select(tokens_q3_opp, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q3_opp <- dfm(tokens_q3_opp)
# 
# 
# tokens_q4_opp <- tokens(q4_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q4_opp <- tokens_select(tokens_q4_opp, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q4_opp <- dfm(tokens_q4_opp)
# 
# 
# ##Keywords
# #Set opp keywords_version 1
# keywords_oppv1 <- c("battery", "charging", "development", "electric", "fund", "hybrid", "incentive", "research", "tax", "technology")
# 
# #Calculate frequency for each quartile
# keyword_q1_oppv1 <- colSums(dfm_q1_opp[, keywords_oppv1])
# 
# keyword_q2_oppv1 <- colSums(dfm_q2_opp[, keywords_oppv1])
# 
# keyword_q3_oppv1 <- colSums(dfm_q3_opp[, keywords_oppv1])
# 
# keyword_q4_oppv1 <- colSums(dfm_q4_opp[, keywords_oppv1])
# 
# ##Plot the results 
# #Create df
# opp_dfv1 <- data.frame(
#   Keyword = rep(keywords_opp, 4),
#   Quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = length(keywords_opp)),
#   Count = c(keyword_q1_oppv1, keyword_q2_oppv1, keyword_q3_oppv1, keyword_q4_oppv1)
# )
# 
# ggplot(opp_dfv1, aes(x = Keyword, y = Count, fill = Quartile)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black") +
#   labs(title = "Opportunity Keyword Counts by Quartiles V1", x = "Keyword", y = "Count", fill = "Quartile") +
#   scale_fill_viridis_d(option = "viridis") +  
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# #Set opp keywords_version 2
# keywords_oppv2 <- c("alternative", "tax", "charging", "technology", "electric", "fuel", "credit", "plug", "infrastructure", "hybrid")
# 
# #Calculate frequency for each quartile
# keyword_q1_oppv2 <- colSums(dfm_q1_opp[, keywords_oppv2])
# 
# keyword_q2_oppv2 <- colSums(dfm_q2_opp[, keywords_oppv2])
# 
# keyword_q3_oppv2 <- colSums(dfm_q3_opp[, keywords_oppv2])
# 
# keyword_q4_oppv2 <- colSums(dfm_q4_opp[, keywords_oppv2])
# 
# # Calculate total words in the corpus for each quartile
# total_opp_q1 <- sum(dfm_q1_opp)
# 
# total_opp_q2 <- sum(dfm_q2_opp)
# 
# total_opp_q3 <- sum(dfm_q3_opp)
# 
# total_opp_q4 <- sum(dfm_q4_opp)
# 
# 
# #Calculate ratio of each keyword to total words
# ratio_opp_q1 <- (keyword_q1_oppv2 / total_opp_q1)
# 
# ratio_opp_q2 <- (keyword_q2_oppv2 / total_opp_q2)
# 
# ratio_opp_q3 <- (keyword_q3_oppv2 / total_opp_q3)
# 
# ratio_opp_q4 <- (keyword_q4_oppv2 / total_opp_q4)
# 
# 
# ##Plot the results 
# #Create df
# opp_dfv2 <- data.frame(
#   Keyword = rep(keywords_oppv2, 4),
#   Quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = length(keywords_oppv2)),
#   Ratio = c(ratio_opp_q1, ratio_opp_q2, ratio_opp_q3, ratio_opp_q4)
# )
# 
# ggplot(opp_dfv2, aes(x = Keyword, y = Ratio, fill = Quartile)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black") +
#   labs(title = "Opportunity Keyword Ratio by Quartiles V2", x = "Keyword", y = "Ratio of Keywords to Total Words", fill = "Quartile") +
#   scale_fill_viridis_d(option = "viridis") +  
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 
# #Calculate quartiles for regulatory 
# q_reg <- quantile(reg, probs = c(0.25, 0.5, 0.75))
# 
# #Create logical condition for grouping based on quartiles
# q1_reg <- auto_corpus[reg <= q_reg[1]]
# q2_reg <- auto_corpus[reg > q_reg[1] & reg <=q_reg[2]]
# q3_reg <- auto_corpus[reg > q_reg[2] & reg <=q_reg[3]]
# q4_reg <- auto_corpus[reg > q_reg[3]]
# 
# 
# #Make reg dfm
# # Convert the corpus to tokens
# tokens_q1_reg <- tokens(q1_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q1_reg <- tokens_select(tokens_q1_reg, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q1_reg <- dfm(tokens_q1_reg)
# 
# 
# tokens_q2_reg <- tokens(q2_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q2_reg <- tokens_select(tokens_q2_reg, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q2_reg <- dfm(tokens_q2_reg)
# 
# 
# tokens_q3_reg <- tokens(q3_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q3_reg <- tokens_select(tokens_q3_reg, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q3_reg <- dfm(tokens_q3_reg)
# 
# 
# tokens_q4_reg <- tokens(q4_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)
# 
# tokens_q4_reg <- tokens_select(tokens_q4_reg, 
#                                pattern = stopwords("english"), 
#                                selection = "remove")
# 
# dfm_q4_reg <- dfm(tokens_q4_reg)
# 
# 
# ##Keywords
# #Set reg keywords
# keywords_reg <- c("standard", "rule", "efficiency", "cafe", "fuel", "harmonization", "implement", "regulation", "monitor", "price")
# 
# #Calculate frequency for each quartile
# keyword_q1_reg <- colSums(dfm_q1_reg[, keywords_reg])
# 
# keyword_q2_reg <- colSums(dfm_q2_reg[, keywords_reg])
# 
# keyword_q3_reg <- colSums(dfm_q3_reg[, keywords_reg])
# 
# keyword_q4_reg <- colSums(dfm_q4_reg[, keywords_reg])
# 
# ##Plot the results 
# #Create df
# reg_df <- data.frame(
#   Keyword = rep(keywords_reg, 4),
#   Quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = length(keywords_reg)),
#   Count = c(keyword_q1_reg, keyword_q2_reg, keyword_q3_reg, keyword_q4_reg)
# )
# 
# ggplot(reg_df, aes(x = Keyword, y = Count, fill = Quartile)) +
#   geom_bar(stat = "identity", position = "dodge", color = "black") +
#   labs(title = "Keyword Counts by Quartiles", x = "Keyword", y = "Count", fill = "Quartile") +
#   scale_fill_viridis_d(option = "viridis") +  
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


###Explore wordclouds
#set.seed(100)
#cloud_opp_above <- textplot_wordcloud(dfm_opp_above, min_count = 8, random_order = FALSE,
                   #rotation = .25, 
                   #colors = RColorBrewer::brewer.pal(8,"Dark2"))


#cloud_opp_below <- textplot_wordcloud(dfm_opp_below, min_count = 8, random_order = FALSE,
                   #rotation = .25, 
                   #colors = RColorBrewer::brewer.pal(8,"Dark2"))

#cloud_reg_above <- textplot_wordcloud(dfm_reg_above, min_count = 8, random_order = FALSE,
                   #rotation = .25, 
                   #colors = RColorBrewer::brewer.pal(8,"Dark2"))


#cloud_reg_below <- textplot_wordcloud(dfm_reg_below, min_count = 8, random_order = FALSE,
                   #rotation = .25, 
                   #colors = RColorBrewer::brewer.pal(8,"Dark2"))

#cloud_phy_above <- textplot_wordcloud(dfm_phy_above, min_count = 8, random_order = FALSE,
                  # rotation = .25, 
                   #colors = RColorBrewer::brewer.pal(8,"Dark2"))


#cloud_phy_below <-textplot_wordcloud(dfm_phy_below, min_count = 8, random_order = FALSE,
                   #rotation = .25, 
                   #colors = RColorBrewer::brewer.pal(8,"Dark2"))
