library(dplyr)
library(tidytext)
data("stop_words")

tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)




tokens <- unnest_tokens(tweets, word, text, to_lower = TRUE) %>%
  anti_join(stop_words)



