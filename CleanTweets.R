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

#we will process in a Corpus with the tm package for simplicity
TTCorp <- Corpus(VectorSource(mbo$text))
TTCorp <- tm_map(TTCorp, content_transformer(tolower))
TTCorp <- tm_map(TTCorp, removePunctuation)
TTCorp <- tm_map(TTCorp, removeNumbers)
mbotweets <- data.frame(sapply(TTCorp, identity), stringsAsFactors = F)

#identifies all english stopwords
Stopwords <- stopwords(kind = "en")

#user defined function for easy cleaning
'%!in%' <- function(x,y)!('%in%'(x,y))

#what we will clean
y = mbotweets[1:dim(mbotweets)[1],1]

#remove stop words
slimmed <- lapply(y, function(x) {
    text <- unlist(strsplit(x, " "))
    text <- text[text %!in% Stopwords]
    text <- paste(text, collapse = " ")
})

cslim = list()
for(i in 1:length(y)){
  tweet <- slimmed[[i]]
  tweet <- gsub("rt\\w+", "", tweet) #remove retweet (still needs work)
  tweet <- gsub("@\\w+", "", tweet) # remove at(@)
  tweet <- gsub("https://\\w+", "", tweet)  # remove links https (still needs work)
  tweet <- gsub("[ |\t]{2,}", "", tweet) # remove tabs 
  tweet <- gsub("^ ", " ", tweet)  # remove blank spaces at the beginning
  tweet <- gsub(" $", " ", tweet) # remove blank spaces at the end
  cslim <- c(cslim, tweet)
}

head(cslim) 
