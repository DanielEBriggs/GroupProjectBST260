#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##GLOBAL
library(shiny)
library(shinyTime)
library(dplyr)
library(tidytext)
library(tidyverse)
library(wordcloud)
data("stop_words")
setwd('C:/Users/debri/Desktop/BST 260 Project/GroupProjectBST260')
tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)
source("DateRoundFunction.R")
tweets$created <- myRound(tweets$created)

tokens <- unnest_tokens(tweets, word, text, to_lower = TRUE) %>%
  anti_join(stop_words)

#afinn assigns numeric value form -5 to 5 (-5 being most negative sentiment)
afinn_sentiments <- get_sentiments("afinn")

tokens_afinn_sentiment <- tokens %>%
  inner_join(afinn_sentiments) %>%
  group_by(id, created) %>%
  mutate(tweet_sentiment = mean(score)) 


cloud <- tokens_afinn_sentiment %>%
  select(word, created, id, score) %>%
  group_by(word, created) %>% 
  mutate(n = n()) %>%
  distinct(word, created, n, score)

cloud <- cloud %>% mutate(day = as.character(as.Date(created)), time = unlist(strsplit(created, " "))[2])
cloud <- cloud %>% mutate(hour = as.numeric(unlist(strsplit(time, ":"))[1]), minute = as.numeric(unlist(strsplit(time, ":"))[2]))
cloud <- cloud %>% ungroup
# Define UI for application that draws a wordcloud
ui <- fluidPage(
   
   # Application title
   titlePanel("Murfreesboro Protest: Tweet Word Frequency by Time"),
   
   # Sidebar with a slider input for time 
   sidebarLayout(
     sidebarPanel(
       radioButtons("day", "Select a Day:",
                    c("2017-10-27" = "2017-10-27",
                      "2017-10-28" = "2017-10-28",
                      "2017-10-29" = "2017-10-29")),
       sliderInput("hour",
                   "Hour:",
                   min = 0,
                   max = 23,
                   value = 0,
                   step = 1,
                   animate = 
                     animationOptions(interval = 3000, loop = F)
     ),      
     sliderInput("minute",
                 "Minute:",
                 min = 0,
                 max = 45,
                 value = 0,
                 step = 15,
                 animate = 
                   animationOptions(interval = 500, loop = T)
     ),

      sliderInput("maxfreq",
                  "Max Frequency:",
                  min = 1,
                  max = 50,
                  value = 50
      )
     ),
     mainPanel(
       plotOutput("wordcloud"), width=12
     )
    )
   )


##SERVER
# Define server logic required to draw a wordcloud
server <- function(input, output){
   

  time <- reactive({
      cloud[which(input$minute == cloud$minute & input$hour == cloud$hour & input$day == cloud$day),]
      })

  wordcloud_rep <- repeatable(wordcloud)
  
  
  output$wordcloud <- renderPlot({

     times <- time()
     # draw the word cloud
     wordcloud_rep(words = times$word, freq = times$n, min.freq = 1,
                   max.words=input$maxfreq, random.order=FALSE, random.color = FALSE,
                   rot.per=0.35, ordered.colors=TRUE,
                   colors=brewer.pal(8, "RdYlGn")[factor(times$score)])
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

