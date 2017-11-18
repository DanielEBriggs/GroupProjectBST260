library(dplyr)
library(tidytext)
library(tidyverse)
data("stop_words")

tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)

tokens <- unnest_tokens(tweets, word, text, to_lower = TRUE) %>%
  anti_join(stop_words)

nrc_sentiments <- get_sentiments("nrc")
afinn_sentiments <- get_sentiments("afinn")
bing_sentiments <- get_sentiments("bing")


tokens_afinn_sentiment <- tokens %>%
  inner_join(afinn_sentiments) %>%
  group_by(id, created) %>%
  mutate(tweet_sentiment = mean(score)) %>%
  ungroup() %>%
  mutate(date = myRound(created))


tokens_afinn_sentiment %>%
  summarize(mean(tweet_sentiment))

tokens_nrc_sentiment <- tokens %>%
  inner_join(nrc_sentiments)

tokens_bing_sentiment <- tokens %>%
  inner_join(bing_sentiments)

