#######
# install.packages("readr")
# install.packages("stringr")
# install.packages("plyr")
# install.packages("tm")
# install.packages("stringi")
# install.packages("stringr")
# install.packages("tm")
#######
#load packages
x <- c("readr", "stringr", "plyr", "tm", "stringi", "stringr", "tm")
lapply(x, require, character.only = T)

#read data and remove unneccesary variables
mbo <- read.csv("C:/Users/debri/Desktop/BST 260/murfreesboro102817.csv", stringsAsFactors = FALSE)
mbo$favorited <- NULL
mbo$statusSource <- NULL
mbo$retweeted <- NULL

filter <- which(mbo$screenName == "Limbaugh2016")
View(mbo[filter,])

#identifies all english stopwords
Stopwords <- stopwords(kind = "en")

#user defined function for easy cleaning
'%!in%' <- function(x,y)!('%in%'(x,y))

#what we will clean
mbo[,1] <- tolower(mbo[1:dim(mbo)[1],1])

#remove stop words
tweets <- unlist(lapply(mbo[,1], function(tweet) {
    text <- unlist(strsplit(tweet, " "))
    text <- text[text %!in% Stopwords]
    tweet <- paste(text, collapse = " ")
}))

unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"

for(i in 1:length(mbo[,1])){
  tweet <-tweets[i]
  tweet <- sub("rt ", "", tweet) #remove retweet 
  tweet <- gsub("@\\w+", "", tweet) # remove at(@)
  tweet <- gsub("&lt;3","",tweet) #removes ASCII hearts <3 
  tweet <- gsub("&lt;|&gt;|&le;|&ge;","",tweet) #removes html <, >, <=, >=
  tweet <- str_replace_all(tweet ,replace_reg, "")  # remove links https 
  tweet <- gsub("[ |\t]{2,}", " ", tweet) # remove tabs 
  tweet <- iconv(tweet, "latin1", "ASCII", sub="") #makes emojis readable 
  tweet <- gsub("<[^>]+>", "", tweet) #removes remaining text from emojis
  tweet <- gsub('[[:punct:] ]+',' ',tweet) #removes punctuation
  tweet <- gsub("[\r|\n|\t|\v|\f]", "", tweet) #removes form feeds tabs etc
  tweet <- gsub("^ ", "", tweet)  # remove blank spaces at the beginning
  tweet <- gsub(" $", "", tweet) # remove blank spaces at the end
  mbo[i,1] <- tweet
}

write.csv(x = mbo[,-1], file = "C:/Users/debri/Desktop/BST 260 Project/GroupProjectBST260/no_text_identities.csv")
write.csv(x = mbo, file = "C:/Users/debri/Desktop/BST 260 Project/GroupProjectBST260/text_identities.csv")
