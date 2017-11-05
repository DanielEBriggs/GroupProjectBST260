####
#set up
#install.packages("twitteR")
#install.packages("ROAuth")
#install.packages("httr")
#install.packages("base64enc")
#install.packages("tm")
#install.packages("ggmap")
#install.packages("mapproj")
#install.packages("rworldmap")
####


x <- c('twitteR', 'ggplot2', 'dplyr', 'purrr','ROAuth', 'httr', 'base64enc', 'tm', "ggmap", "mapproj", "rworldmap")
lapply(x, require, character.only = T)

#establish credentials
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
setup_twitter_oauth(consumer_key='p7wg7XdT9PQB7KAr6Z14tGxAB',
                    consumer_secret='6QeIsHzB9UfHj4orCmiootLdFFhblBd3UV7cxKD6D4olnUe8rO',
                    access_token = "767174880739614720-Mkz5nzcE9jVMv0y7qhmiJfPXO7PVQSp",
                    access_secret = "wlhzYEIhpnh78NDa6BN05JXKmavHswFdQDol73LBnaaFf")

murfreesboro = '35.846135,-86.393137,25mi'

tweets <- searchTwitter('', n = 500000, geocode = '35.846135,-86.393137,50mi', since = '2017-10-27', until = '2017-10-29')
tweets_df <- tbl_df(map_df(tweets, as.data.frame))
carly <- searchTwitter('from:carlybbroadwell', n = 1500)
write.csv(tweets_df, file = "murfreesboro102817.csv",row.names=FALSE)

getwd()
