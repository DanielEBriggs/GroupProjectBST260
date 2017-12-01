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

#Credentials have been altered
#Credentials can be obtained from https://developer.twitter.com/
setup_twitter_oauth(consumer_key='"FEDNBJKXZAWLPCTQVGIRHMOUS"',
                    consumer_secret='YRQIIJUCUEWUPZTPWFIDTEPXUUGGFJXZKSEXTXLWTVMIPZYVBI',
                    access_token = "UIRWZFUEAUYRLSBSICBDTQKUZCDTWAYLCWMCDOHGVIBJGSFMKQ",
                    access_secret = "LBSVUGJNJFXRGEUQUENPLWHBDRWRMDMFXXAHBIHGCCSGK")


tweets <- searchTwitter('', n = 500000, geocode = '35.846135,-86.393137,50mi', since = '2017-10-27', until = '2017-10-29')
tweets_df <- tbl_df(map_df(tweets, as.data.frame))
