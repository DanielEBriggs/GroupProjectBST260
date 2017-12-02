library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(chron)
library(gridExtra)
library(gganimate)

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

#Calculates mean sentiment by tweet

tweets_afinn_sentiment <- tokens %>%
  inner_join(afinn_sentiments, by = "word") %>%
  group_by(id, created) %>%
  mutate(tweet_sentiment = mean(score),
         day = "", 
         day = replace(day, str_sub(created, start = 9, end = 10) == "29", "Sunday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "28", "Saturday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "27", "Friday"),
         minutes_15 = round(60 * 24 * as.numeric(times(str_sub(created, start = -8, end = -1))), 0),
         minutes_15 = trunc(minutes_15/15)) %>%
  select(created, id, screenName, retweetCount, tweet_sentiment, day, minutes_15, replyToSN)


##Big players for network analysis

network_stars <-  c("BigBoyVol", "BlueRaiderDJ", "Limbaugh2016", "John_Currie", "ChadCaldwell24", 
                    "dennisbrucemor1", "realDonaldTrump", "DeLoachJW", "jdbswim", "ClayTravis",
                    "mikerapp", "diamondhockey", "ColeenC123", "cbrentv3", "BlakeShelton193",
                    "justasking3time")


#filter tweets for networks of big players

network_stars_tweets <- tweets_afinn_sentiment %>%
  filter(replyToSN %in% network_stars | screenName %in% network_stars) %>%
  mutate(network = ifelse(screenName %in% network_stars, screenName, NA),
         network = replace(network, replyToSN %in% network_stars, replyToSN))

#Plots

# All tweets sentiment over time


friday <-  tweets_afinn_sentiment %>%
  group_by(minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n()) %>%
  filter(day == "Friday") %>%
  ggplot(aes(frame = minutes_15)) +
  geom_point(aes(x = minutes_15, y = tweet_sentiment, size = n)) +
  geom_smooth(aes(x = minutes_15, y = tweet_sentiment))

saturday <-  tweets_afinn_sentiment %>%
  group_by(minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n()) %>%
  filter(day == "Saturday") %>%
  ggplot() +
  geom_point(aes(x = minutes_15, y = tweet_sentiment, size = n)) +
  geom_smooth(aes(x = minutes_15, y = tweet_sentiment))

sunday <-  tweets_afinn_sentiment %>%
  group_by(minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n()) %>%
  filter(day == "Sunday") %>%
  ggplot() +
  geom_point(aes(x = minutes_15, y = tweet_sentiment, size = n)) +
  geom_smooth(aes(x = minutes_15, y = tweet_sentiment))

grid.arrange(friday, saturday, sunday)

# Big player networks over time

network_stars_tweets %>%
  group_by(minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n()) %>%
  filter(day == "Saturday") %>%
  ggplot() +
  geom_point(aes(x = minutes_15, y = tweet_sentiment, size = n, colour = network)) +
  geom_smooth(aes(x = minutes_15, y = tweet_sentiment))

# NRC sentiments for networks

tweets_nrc_sentiment <- tokens %>%
  inner_join(nrc_sentiments, by = "word") %>%
  group_by(id, created) %>%
  mutate(day = "", 
         day = replace(day, str_sub(created, start = 9, end = 10) == "29", "Sunday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "28", "Saturday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "27", "Friday"),
         minutes_15 = round(60 * 24 * as.numeric(times(str_sub(created, start = -8, end = -1))), 0),
         minutes_15 = trunc(minutes_15/15)) %>%
  select(created, id, screenName, retweetCount, sentiment, day, minutes_15, replyToSN) %>%
  filter(replyToSN %in% network_stars | screenName %in% network_stars) %>%
  mutate(network = ifelse(screenName %in% network_stars, screenName, NA),
         network = replace(network, replyToSN %in% network_stars, replyToSN))


# Density of sentiments over time

sentiment_density_time <- tweets_nrc_sentiment %>%
  mutate(sentiment = as.factor(sentiment)) %>%
  group_by(minutes_15, sentiment) %>%
  mutate(count = n()) %>%
  distinct(day, minutes_15, sentiment, density, n_time, network)

#Plot of sentiment density

sentiment_density_time %>%
  filter(day == "Saturday", network == "realDonaldTrump") %>%
  ggplot(aes(x = minutes_15, y = ..count.., fill = sentiment)) +
  geom_density(aes(alpha = 0.6), position = "stack")
  
  
  
  
  
  ggplot(diamonds, aes(x=depth, y=..density..)) + 
  geom_density(aes(fill=cut), position="stack")

# Limitations of sentiment packages

nrc_sentiments %>%
  distinct(sentiment)


