library(dplyr)
library(tidytext)
library(tidyverse)
data("stop_words")

tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)

#Seperates tweets into one line per word

tokens <- unnest_tokens(tweets, word, text, to_lower = TRUE) %>%
  anti_join(stop_words)


# each sentiment library provides a different method of assigning sentiment to a word

#nrc assigns multiple sentiment to each word
nrc_sentiments <- get_sentiments("nrc")

#afinn assigns numeric value form -5 to 5 (-5 being most negative sentiment)
afinn_sentiments <- get_sentiments("afinn")

#bing sentiment assigns binary positive/negative sentiment
bing_sentiments <- get_sentiments("bing")



#Calculates mean sentiment by tweet

tokens_afinn_sentiment <- tokens %>%
  inner_join(afinn_sentiments) %>%
  group_by(id, created) %>%
  mutate(tweet_sentiment = mean(score)) 


