### Firms & Lobbying
### Text Analysis
### Transport manufacturing

rm(list=ls())

install.packages("quanteda.textstats", "wordcloud", "quanteda.textplots")

# Load packages
pacman::p_load(tidyverse, readxl, quanteda, quanteda.textstats, stm, wordcloud, quanteda.textplots, ggplot2, viridis)


# Set working directory
if(Sys.info()["user"]=="fiona" ) {setwd("/Users/fiona/Dropbox (Princeton)/BBH/BBH1/")}

####Data preparation

#Load data
auto <- read_excel("data/03_final/issues_texts/auto.xlsx")

#Create climate binary variable
auto$CLI <- grepl("ENV|CAW|ENG|FUE", auto$issue_code)

#Filter data for climate lobbying only 
auto <- auto %>% 
  filter(CLI == "TRUE")

auto <- auto %>%
  filter(
    complete.cases(
      op_expo_ew, rg_expo_ew, ph_expo_ew
    )
  )


###Create overall corpus 
auto_corpus <-corpus(auto$issue_text, docnames = seq_len(nrow(auto)))

# Add document-level information
docvars(auto_corpus) <- auto[, c("gvkey", "conm", "year", "op_expo_ew", "rg_expo_ew", "ph_expo_ew")]

###Create sub-corpus for quartiles for each exposure measure
##Opportunity 
# Get the document variable from the corpus
opp <- docvars(auto_corpus, "op_expo_ew")

#Calculate quartiles for opportunity 
q_opp <- quantile(opp, probs = c(0.25, 0.5, 0.75))

#Create logical condition for grouping based on quartiles
q1_opp <- auto_corpus[opp <= q_opp[1]]
q2_opp <- auto_corpus[opp > q_opp[1] & opp <=q_opp[2]]
q3_opp <- auto_corpus[opp > q_opp[2] & opp <=q_opp[3]]
q4_opp <- auto_corpus[opp > q_opp[3]]


#Make opp dfm
# Convert the corpus to tokens
tokens_q1_opp <- tokens(q1_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q1_opp <- tokens_select(tokens_q1_opp, 
                                  pattern = stopwords("english"), 
                                  selection = "remove")

dfm_q1_opp <- dfm(tokens_q1_opp)


tokens_q2_opp <- tokens(q2_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q2_opp <- tokens_select(tokens_q2_opp, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q2_opp <- dfm(tokens_q2_opp)


tokens_q3_opp <- tokens(q3_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q3_opp <- tokens_select(tokens_q3_opp, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q3_opp <- dfm(tokens_q3_opp)


tokens_q4_opp <- tokens(q4_opp, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q4_opp <- tokens_select(tokens_q4_opp, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q4_opp <- dfm(tokens_q4_opp)


##Keywords
#Set opp keywords_version 1
keywords_oppv1 <- c("battery", "charging", "development", "electric", "fund", "hybrid", "incentive", "research", "tax", "technology")

#Calculate frequency for each quartile
keyword_q1_oppv1 <- colSums(dfm_q1_opp[, keywords_oppv1])

keyword_q2_oppv1 <- colSums(dfm_q2_opp[, keywords_oppv1])

keyword_q3_oppv1 <- colSums(dfm_q3_opp[, keywords_oppv1])

keyword_q4_oppv1 <- colSums(dfm_q4_opp[, keywords_oppv1])

##Plot the results 
#Create df
opp_dfv1 <- data.frame(
  Keyword = rep(keywords_opp, 4),
  Quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = length(keywords_opp)),
  Count = c(keyword_q1_oppv1, keyword_q2_oppv1, keyword_q3_oppv1, keyword_q4_oppv1)
)

ggplot(opp_dfv1, aes(x = Keyword, y = Count, fill = Quartile)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Opportunity Keyword Counts by Quartiles V1", x = "Keyword", y = "Count", fill = "Quartile") +
  scale_fill_viridis_d(option = "viridis") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Set opp keywords_version 2
keywords_oppv2 <- c("alternative", "tax", "charging", "technology", "electric", "fuel", "credit", "plug", "infrastructure", "hybrid")

#Calculate frequency for each quartile
keyword_q1_oppv2 <- colSums(dfm_q1_opp[, keywords_oppv2])

keyword_q2_oppv2 <- colSums(dfm_q2_opp[, keywords_oppv2])

keyword_q3_oppv2 <- colSums(dfm_q3_opp[, keywords_oppv2])

keyword_q4_oppv2 <- colSums(dfm_q4_opp[, keywords_oppv2])

##Plot the results 
#Create df
opp_dfv2 <- data.frame(
  Keyword = rep(keywords_opp, 4),
  Quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = length(keywords_opp)),
                 Count = c(keyword_q1_oppv2, keyword_q2_oppv2, keyword_q3_oppv2, keyword_q4_oppv2)
  )

ggplot(opp_dfv2, aes(x = Keyword, y = Count, fill = Quartile)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Opportunity Keyword Counts by Quartiles V2", x = "Keyword", y = "Count", fill = "Quartile") +
  scale_fill_viridis_d(option = "viridis") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Regulatory
# Get the document variable from the corpus
reg <- docvars(auto_corpus, "rg_expo_ew")

#Calculate quartiles for regulatory 
q_reg <- quantile(reg, probs = c(0.25, 0.5, 0.75))

#Create logical condition for grouping based on quartiles
q1_reg <- auto_corpus[reg <= q_reg[1]]
q2_reg <- auto_corpus[reg > q_reg[1] & reg <=q_reg[2]]
q3_reg <- auto_corpus[reg > q_reg[2] & reg <=q_reg[3]]
q4_reg <- auto_corpus[reg > q_reg[3]]


#Make reg dfm
# Convert the corpus to tokens
tokens_q1_reg <- tokens(q1_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q1_reg <- tokens_select(tokens_q1_reg, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q1_reg <- dfm(tokens_q1_reg)


tokens_q2_reg <- tokens(q2_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q2_reg <- tokens_select(tokens_q2_reg, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q2_reg <- dfm(tokens_q2_reg)


tokens_q3_reg <- tokens(q3_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q3_reg <- tokens_select(tokens_q3_reg, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q3_reg <- dfm(tokens_q3_reg)


tokens_q4_reg <- tokens(q4_reg, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, lowercase = TRUE)

tokens_q4_reg <- tokens_select(tokens_q4_reg, 
                               pattern = stopwords("english"), 
                               selection = "remove")

dfm_q4_reg <- dfm(tokens_q4_reg)


##Keywords
#Set reg keywords
keywords_reg <- c("standard", "rule", "efficiency", "cafe", "fuel", "harmonization", "implement", "regulation", "monitor", "price")

#Calculate frequency for each quartile
keyword_q1_reg <- colSums(dfm_q1_reg[, keywords_reg])

keyword_q2_reg <- colSums(dfm_q2_reg[, keywords_reg])

keyword_q3_reg <- colSums(dfm_q3_reg[, keywords_reg])

keyword_q4_reg <- colSums(dfm_q4_reg[, keywords_reg])

##Plot the results 
#Create df
reg_df <- data.frame(
  Keyword = rep(keywords_reg, 4),
  Quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = length(keywords_reg)),
  Count = c(keyword_q1_reg, keyword_q2_reg, keyword_q3_reg, keyword_q4_reg)
)

ggplot(reg_df, aes(x = Keyword, y = Count, fill = Quartile)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Keyword Counts by Quartiles", x = "Keyword", y = "Count", fill = "Quartile") +
  scale_fill_viridis_d(option = "viridis") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###################################
###Alternative approach using median
# Calculate the mean of the document variable
med_opp <- median(opp)

# Create a logical condition for grouping
condition_opp <- opp > med_opp  # or < if you want the opposite comparison

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

##Keywords
#Set opp keywords
keywords_opp <- c("incentive", "tax", "charging", "technology", "electric", "fund", "research", "development", "battery", "hybrid")

#Calculate frequency for above/below mean
keyword_opp_above <- colSums(dfm_opp_above[, keywords_opp])

keyword_opp_below <- colSums(dfm_opp_below[, keywords_opp])

# Calculate total words in the corpus
total_opp_above <- sum(dfm_opp_above)

total_opp_below <- sum(dfm_opp_below)

#Calculate ratio of each keyword to total words
ratio_opp_above <- (keyword_opp_above / total_opp_above)

ratio_opp_below <- (keyword_opp_below / total_opp_below)

##Plot the results 
#Create df to plot
opp_df <- data.frame(keywords = keywords_opp, above = ratio_opp_above, below = ratio_opp_below)
plot_opp_df <- reshape2::melt(opp_df, id.vars = "keywords")

#Plot
ggplot(plot_df, aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Ratio of Opportunity Keywords Above and Below the Median",
       x = "Keywords", y = "Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

##Regulatory 
# Get the document variable from the corpus
reg <- docvars(auto_corpus, "rg_expo_ew")

# Calculate the mean of the document variable
med_reg <- median(reg)

# Create a logical condition for grouping
condition_reg <- reg > med_reg  # or < if you want the opposite comparison

# Split the corpus into two groups based on the condition
above_regmed_corpus <- subset(auto_corpus, condition_reg)
below_regmed_corpus <- subset(auto_corpus, !condition_reg)

#Make opp dfm
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
#Set opp keywords
keywords_reg <- c("standard", "rule", "efficiency", "cafe", "fuel", "harmonization", "implement", "regulation", "monitor", "price")

#Calculate frequency for above/below mean
keyword_reg_above <- colSums(dfm_reg_above[, keywords_reg])

keyword_reg_below <- colSums(dfm_reg_below[, keywords_reg])


# Calculate total words in the corpus
total_reg_above <- sum(dfm_reg_above)

total_reg_below <- sum(dfm_reg_below)

#Calculate ratio of each keyword to total words
ratio_reg_above <- (keyword_reg_above / total_reg_above)

ratio_reg_below <- (keyword_reg_below / total_reg_below)

#Create df to plot
reg_df <- data.frame(keywords = keywords_reg, above = ratio_reg_above, below = ratio_reg_below)
plot_reg_df <- reshape2::melt(reg_df, id.vars = "keywords")

#Plot
ggplot(plot_reg_df, aes(x = keywords, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Ratio of Regulatory Keywords for Firms Above vs At/Below the Median",
       x = "Keywords", y = "Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


##Physical 
# Get the document variable from the corpus
phy <- docvars(auto_corpus, "ph_expo_ew")

# Calculate the mean of the document variable
med_phy <- median(phy)

# Create a logical condition for grouping
condition_phy <- phy > med_phy  # or < if you want the opposite comparison

# Split the corpus into two groups based on the condition
above_phymed_corpus <- subset(auto_corpus, condition_phy)
below_phymed_corpus <- subset(auto_corpus, !condition_phy)

#Make opp dfm
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
#Set opp keywords
keywords_phy <- c("greenhouse", "water", "climate", "general", "warming", "environment", "land", "air", "pollution", "flood" )
keywords_phy <- c("greenhouse", "water", "climate", "general", "warming", "environment", "land", "air", "pollution")

#Calculate frequency for above/below mean
keyword_phy_above <- colSums(dfm_phy_above[, keywords_phy])

keyword_phy_below <- colSums(dfm_phy_below[, keywords_phy])

matching_keywords_phy <- colnames(dfm_phy_above)[colnames(dfm_phy_above) %in% keywords_phy]

print(matching_keywords_phy) #get same issue as above, no mention of flood in the above median group


# Calculate total words in the corpus
total_phy_above <- sum(dfm_phy_above)

total_phy_below <- sum(dfm_phy_below)

#Calculate ratio of each keyword to total words
ratio_phy_above <- (keyword_phy_above / total_phy_above)*100

ratio_phy_below <- (keyword_phy_below / total_phy_below)*100

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
