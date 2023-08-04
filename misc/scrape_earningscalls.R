
root <- "https://seekingalpha.com/symbol/"

sym <- "F"

site <- paste0(root, sym)

page <- read_html("https://seekingalpha.com/symbol/F/earnings/transcripts")

'//*[@id="content"]/div[2]/div/div[3]/div/div[1]/div[2]/div/section/div/div[2]/div/article[1]'
'//*[@id="content"]/div[2]/div/div[3]/div/div[1]/div[2]/div/section/div/div[2]/div'

library(rvest)
library(tidyverse)

summaries_xpath <- page %>%
  html_elements(xpath = '//*[@id="content"]/div[2]/div/div[3]/div/div[1]/div[2]/div/section/div/div[2]/div')

summaries_xpath <- NYT_page %>%
  html_elements(xpath = "//*[contains(@class, 'summary-class')]")

head(summaries_xpath)