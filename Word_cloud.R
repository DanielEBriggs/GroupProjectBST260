# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(dplyr)
library(tidytext)
library(tidyverse)
data("stop_words")

tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)

#Seperates tweets into one line per word

tokens <- unnest_tokens(tweets, word, text, to_lower = TRUE) %>%
  anti_join(stop_words)

cloud <- tokens %>%
  select(word) %>%
  mutate(word_1 = word) %>%
  group_by(word) %>% 
  mutate(n = n()) %>%
  distinct(word, n)

cloud %>%
  distinct(word, n) %>%
  filter(word == "nazi") %>%
  select(n)
  
  
wordcloud(words = cloud$word, freq = cloud$n, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
