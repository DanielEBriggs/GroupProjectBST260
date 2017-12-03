library(dplyr)
library(tidytext)
library(tidyverse)
library(stringr)
library(gridExtra)
library(timevis)
library(lubridate)
library(scales)
library(knitr)

data("stop_words")

tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)

tweet_text <- tweets %>%
  distinct(created, screenName, text) %>%
  mutate(created = ymd_hms(created))

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
  group_by(screenName, created) %>%
  mutate(tweet_sentiment = mean(score),
         day = "", 
         day = replace(day, str_sub(created, start = 9, end = 10) == "29", "Sunday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "28", "Saturday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "27", "Friday"),
         minutes_15 = round(60 * 24 * as.numeric(times(str_sub(created, start = -8, end = -1))), 0),
         minutes_15 = trunc(minutes_15/15)) %>%
  ungroup() %>%
  mutate(created = ymd_hms(created)) %>%
  select(created, id, screenName, retweetCount, tweet_sentiment, day, minutes_15, replyToSN)

tweets_afinn_sentiment$round_qhr <- round_qhr <- as.POSIXct(round(as.double(tweets_afinn_sentiment$created)/(15*60))*(15*60), origin=(as.POSIXct('1970-01-01')))


#sample of most negative tweets

tweets_afinn_sentiment %>%
  distinct(created, screenName, tweet_sentiment) %>%
  inner_join(tweet_text, by = c("created", "screenName")) %>%
  arrange(tweet_sentiment) %>%
  head(20) %>%
  kable()

##Big players for network analysis

network_stars <-  c("BigBoyVol", "BlueRaiderDJ", "Limbaugh2016", "ChadCaldwell24", 
                    "dennisbrucemor1", "realDonaldTrump", "DeLoachJW", "jdbswim",
                    "mikerapp", "ColeenC123", "cbrentv3", "BlakeShelton193",
                    "justasking3time")


#filter tweets for networks of big players

network_stars_tweets <- tweets_afinn_sentiment %>%
  filter(replyToSN %in% network_stars | screenName %in% network_stars) %>%
  mutate(network = ifelse(replyToSN %in% network_stars, replyToSN, NA),
         network = ifelse(is.na(network), screenName, replyToSN))

#Plots

# All tweets sentiment over time (saturday)


saturday <-  tweets_afinn_sentiment %>%
  filter(created < as.POSIXct("2017-10-28 17:30:00") & created > as.POSIXct("2017-10-28 02:00:00")) %>%
  group_by(day, minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n(),
         Positive = (tweet_sentiment > 0)) %>%
  filter(day == "Saturday") %>%
  ggplot(aes(x = round_qhr, y = tweet_sentiment)) +
  geom_point(aes(size = n, color = Positive), alpha = 0.75) +
  geom_smooth(color = "grey", linetype = "dotted") +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
  scale_y_continuous(limits = c(-2, 2)) +
  xlab("Hour") +
  ylab("Average Twitter Sentiment")


# Big player networks over time

saturday_stars <- network_stars_tweets %>%
  group_by(day, minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n(),
         Positive = (tweet_sentiment > 0)) %>%
  filter(day == "Saturday") %>%
  ggplot(aes(x = round_qhr, y = tweet_sentiment)) +
  geom_point(aes(size = n, color = Positive), alpha = 0.75) +
  geom_smooth(color = "grey", linetype = "dotted", se = FALSE) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("4 hour")) +
  scale_y_continuous(limits = c(-2, 2)) +
  xlab("Hour") +
  ylab("Average Twitter Sentiment") +
  facet_wrap(~network, nrow = 3)
  

# NRC sentiments for networks

tweets_nrc_sentiment <- tokens %>%
  inner_join(nrc_sentiments, by = "word") %>%
  mutate(day = "", 
         day = replace(day, str_sub(created, start = 9, end = 10) == "29", "Sunday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "28", "Saturday"),
         day = replace(day, str_sub(created, start = 9, end = 10) == "27", "Friday"),
         minutes_15 = round(60 * 24 * as.numeric(times(str_sub(created, start = -8, end = -1))), 0),
         minutes_15 = trunc(minutes_15/15),
         created = ymd_hms(created)) %>%
  select(created, id, screenName, retweetCount, sentiment, day, minutes_15, replyToSN) %>%
  filter(replyToSN %in% network_stars | screenName %in% network_stars) %>%
  mutate(network = ifelse(screenName %in% network_stars, screenName, NA),
         network = replace(network, replyToSN %in% network_stars, replyToSN))

tweets_nrc_sentiment$round_qhr <- round_qhr <- as.POSIXct(round(as.double(tweets_nrc_sentiment$created)/(15*60))*(15*60), origin=(as.POSIXct('1970-01-01')))

# nrc count of sentiments over time

sentiment_nrc_time <- tweets_nrc_sentiment %>%
  group_by(day, minutes_15) %>%
  mutate(n_time = n()) %>%
  ungroup() %>%
  group_by(day, minutes_15, sentiment) %>%
  mutate(n_sentiment = n(), sentiment_share = n_sentiment/n_time) %>%
  ungroup() %>%
  mutate(sentiment_overall = ifelse(sentiment %in% c("fear", "anticipation", "surprise"), "Suspense", NA),
         sentiment_overall = replace(sentiment_overall, sentiment %in% c("positive", "joy", "trust"), "Positive"),
         sentiment_overall = replace(sentiment_overall, is.na(sentiment_overall), "Negative"))
  distinct(sentiment, day, round_qhr, n_time, n_sentiment, sentiment_share)



#Plot of sentiment over time

sentiment_nrc_time %>%
  filter(day == "Saturday") %>%
  ggplot(aes(x = round_qhr, y = sentiment_share, color = sentiment)) + 
  geom_smooth(se = FALSE) +
  facet_grid(~sentiment_overall)


#timeline of events

"source: http://www.tennessean.com/story/news/2017/10/28/white-lives-matter-rally-murfreesboro-tn-live-updates-shelbyville-tn-stream-video/804380001/"

#afinn

saturday  +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 03:00:00"), y = -2, xend = as.POSIXct("2017-10-28 03:00:00"), yend = -0.9), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 03:00:00"), y = -0.8, label = "Murfreesboro: Police \n close town square"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 09:00:00"), y = 2, xend = as.POSIXct("2017-10-28 09:00:00"), yend = 1.6), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 09:00:00"), y = 1.5, label = "Shelbyville: Law enforcement \n arrives in riot gear"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 10:15:00"), y = -2, xend = as.POSIXct("2017-10-28 10:15:00"), yend = -1.6), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 10:15:00"), y = -1.5, label = "Shelbyville: First white nationalist \n and counter-protestors arrive"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 12:00:00"), y = 2, xend = as.POSIXct("2017-10-28 12:00:00"), yend = -0.65), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 12:00:00"), y = -0.75, label = "Shelbyville: 400 counter-protestors, \n 200 white nationalists on site"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 14:00:00"), y = 2, xend = as.POSIXct("2017-10-28 14:00:00"), yend = -0.2), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 15:30:00"), y = -0.3, label = "Shelbyville: White nationalists elect \n to move to Murfeesboro"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 16:00:00"), y = -2, xend = as.POSIXct("2017-10-28 16:00:00"), yend = -1.25), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 16:30:00"), y = -1.1, label = "Murfreesboro rally fizzles \n as white nationalist numbers dwindle"), size = 4, data = data.frame()) +
  ggtitle("Average Sentiment during Murfreesboro/Shelbyville Protest") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) 

#nrc

sentiment_nrc_time %>%
  mutate(Sentiment = sentiment, n = n_sentiment) %>%
  filter(created < as.POSIXct("2017-10-28 17:30:00") & created > as.POSIXct("2017-10-28 02:00:00")) %>%
  filter(day == "Saturday" & sentiment %in% c("anticipation", "anger")) %>%
  ggplot(aes(x = round_qhr, y = sentiment_share)) + 
  geom_point(aes(size = n, color = Sentiment), alpha = 0.35) +
  geom_smooth(se = FALSE, aes(color = sentiment)) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 03:00:00"), y = 0.4, xend = as.POSIXct("2017-10-28 03:00:00"), yend = 0.24), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 03:00:00"), y = 0.23, label = "Murfreesboro: Police \n close town square"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 09:00:00"), y = 0, xend = as.POSIXct("2017-10-28 09:00:00"), yend = 0.135), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 09:00:00"), y = 0.15, label = "Shelbyville: Law enforcement \n arrives in riot gear"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 10:15:00"), y = 0.4, xend = as.POSIXct("2017-10-28 10:15:00"), yend = 0.24), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 10:15:00"), y = 0.225, label = "Shelbyville: First white nationalist \n and counter-protestors arrive"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 12:00:00"), y = 0, xend = as.POSIXct("2017-10-28 12:00:00"), yend = 0.07), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 12:00:00"), y = 0.085, label = "Shelbyville: 400 counter-protestors, \n 200 white nationalists on site"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 14:00:00"), y = 0.4, xend = as.POSIXct("2017-10-28 14:00:00"), yend = 0.27), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 14:20:00"), y = 0.255, label = "Shelbyville: White nationalists elect \n to move to Murfeesboro"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 16:00:00"), y = 0, xend = as.POSIXct("2017-10-28 16:00:00"), yend = 0.035), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 16:30:00"), y = 0.05, label = "Murfreesboro rally fizzles \n as white nationalist numbers dwindle"), size = 4, data = data.frame()) +
  ggtitle("Sentiment Share during Murfreesboro/Shelbyville Protest") +
  theme(plot.title = element_text(hjust = 0.5, size = 25)) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
  scale_y_continuous(limits = c(0, 0.4)) +
  xlab("Hour") +
  ylab("Share of Total Sentiments Expressed")

table(nrc_sentiments$sentiment)

#limitations

#assignment of afinn sentiments are somewhat arbitrary and may not have the context for specific events

afinn_sentiments %>%
  filter(word %in% c("absentee", "aboard", "apocalyptic", "cheer", "cheat", "charm", "damn", "rejoice", "prick", "thrilled")) %>%
  arrange(score)

afinn_sentiments %>%
  summarise(sum(word %in% c("confederate", "nazi", "kkk")))

# Some tweets cannot be assigned a sentiment (improper spelling, slang terms, etc.)

tweet_text %>%
  anti_join(tweets_afinn_sentiment, by = c("created", "screenName")) %>%
  mutate(key_word = str_detect(text, paste(c("nazi", "kkk", "supremacy"),collapse = '|'))) %>%
  filter(key_word == TRUE) %>%
  select(screenName, text) %>%
  head(10)


# sentiment analysis can't correct spelling

tweet_text %>%
  filter(screenName == "justasking3time") %>%
  select(text) %>%
  head(10)


# nrc sentiments lack context as well

nrc_sentiments %>%
  filter(word %in% c("confederate", "nationalist", "trump", "president")) %>%
  kable()


# Friday average sentiment over time

tweets_afinn_sentiment %>%
  filter(created < as.POSIXct("2017-10-27 17:30:00") & created > as.POSIXct("2017-10-27 02:00:00")) %>%
  group_by(day, minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n(),
         Positive = (tweet_sentiment > 0)) %>%
  filter(day == "Friday") %>%
  ggplot(aes(x = round_qhr, y = tweet_sentiment)) +
  geom_point(aes(size = n, color = Positive), alpha = 0.75) +
  geom_smooth(color = "grey", linetype = "dotted") +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
  scale_y_continuous(limits = c(-2, 2)) +
  xlab("Hour") +
  ylab("Average Twitter Sentiment")

#Sunday average sentiment over time

tweets_afinn_sentiment %>%
  filter(created < as.POSIXct("2017-10-29 17:30:00") & created > as.POSIXct("2017-10-29 02:00:00")) %>%
  group_by(day, minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n(),
         Positive = (tweet_sentiment > 0)) %>%
  filter(day == "Sunday") %>%
  ggplot(aes(x = round_qhr, y = tweet_sentiment)) +
  geom_point(aes(size = n, color = Positive), alpha = 0.75) +
  geom_smooth(color = "grey", linetype = "dotted") +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("2 hour")) +
  scale_y_continuous(limits = c(-2, 2)) +
  xlab("Hour") +
  ylab("Average Twitter Sentiment")



# Trumps network

network_stars_tweets %>%
  filter(created < as.POSIXct("2017-10-28 17:30:00") & created > as.POSIXct("2017-10-28 02:00:00")) %>%
  filter(network == "realDonaldTrump") %>%
  group_by(day, minutes_15) %>%
  mutate(tweet_sentiment = mean(tweet_sentiment),
         n = n(),
         Positive = (tweet_sentiment > 0)) %>%
  filter(day == "Saturday") %>%
  ggplot(aes(x = round_qhr, y = tweet_sentiment)) +
  geom_point(aes(size = n, color = Positive), alpha = 0.75) +
  geom_smooth(color = "orange", linetype = "dotted", se = FALSE) +
  scale_x_datetime(labels = date_format("%H:%M"), breaks = date_breaks("4 hour")) +
  xlab("Hour") +
  ylab("Average Twitter Sentiment") +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 03:00:00"), y = -4, xend = as.POSIXct("2017-10-28 03:00:00"), yend = 0), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 03:00:00"), y = 0.1, label = "Murfreesboro: Police \n close town square"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 09:00:00"), y = 4, xend = as.POSIXct("2017-10-28 09:00:00"), yend = -0.4), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 09:00:00"), y = -0.5, label = "Shelbyville: Law enforcement \n arrives in riot gear"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 10:15:00"), y = -4, xend = as.POSIXct("2017-10-28 10:15:00"), yend = -2), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 10:15:00"), y = -1.9, label = "Shelbyville: First white nationalist \n and counter-protestors arrive"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 12:00:00"), y = 4, xend = as.POSIXct("2017-10-28 12:00:00"), yend = 0.6), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 11:35:00"), y = 0.5, label = "Shelbyville: 400 counter-protestors, \n 200 white nationalists on site"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 14:00:00"), y = 4, xend = as.POSIXct("2017-10-28 14:00:00"), yend = -0.1), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 15:30:00"), y = -0.2, label = "Shelbyville: White nationalists elect \n to move to Murfeesboro"), size = 4, data = data.frame()) +
  geom_segment(mapping = aes(x = as.POSIXct("2017-10-28 16:00:00"), y = -4, xend = as.POSIXct("2017-10-28 16:00:00"), yend = -2.5), size = 0.2,
               linetype = "dashed", data = data.frame()) +
  geom_text(mapping = aes(x = as.POSIXct("2017-10-28 15:30:00"), y = -2.35, label = "Murfreesboro rally fizzles \n as white nationalist numbers dwindle"), size = 4, data = data.frame()) +
  ggtitle("Donald Trump's Network during Murfreesboro/Shelbyville Protest") +
  theme(plot.title = element_text(hjust = 0.5, size = 25))

