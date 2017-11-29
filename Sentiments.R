library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(chron)

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

tweet_sentiment_time <- tokens_afinn_sentiment %>%
  mutate(day = "", day = replace(day, str_sub(created, start = 9, end = 10) == "29", "Sunday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "28", "Saturday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "27", "Friday"),
         minutes = 60 * 24 * as.numeric(times(str_sub(created, start = -8, end = -1)))) %>%
  select(created, id, screenName, retweetCount, tweet_sentiment, day, minutes)
  
  
tweet_sentiment_time %>%
  ggplot() +
  geom_point(aes(x = minutes, y = tweet_sentiment, col = day))


tweet_sentiment_time %>%
  group_by(day) %>%
  summarise(mean(tweet_sentiment))




