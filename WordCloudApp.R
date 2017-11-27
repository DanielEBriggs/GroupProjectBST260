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
library(dplyr)
library(tidytext)
library(tidyverse)
library(wordcloud)
data("stop_words")
setwd("C:/Users/debri/Desktop/BST 260 Project/GroupProjectBST260")

tweets <- read.csv("text_identities.csv", stringsAsFactors = FALSE)
source("DateRoundFunction.R")
tweets$created <- myRound(tweets$created)

tokens <- unnest_tokens(tweets, word, text, to_lower = TRUE) %>%
  anti_join(stop_words)

cloud <- tokens %>%
  select(word, created) %>%
  group_by(word, created) %>% 
  mutate(n = n()) %>%
  distinct(word, created, n)



# Define UI for application that draws a wordcloud
ui <- fluidPage(
   
   # Application title
   titlePanel("Murfreesboro Protest: Tweet Word Frequency by Time"),
   
   # Sidebar with a slider input for time 
   sidebarLayout(
     sidebarPanel(
       sliderInput("time",
                   "Time:",
                   min = as.POSIXlt("2017-10-27 00:00", tz = 'EST5EDT'),
                   max = as.POSIXlt("2017-10-29 23:30", tz = 'EST5EDT'),
                   value = as.POSIXlt("2017-10-27 00:00", tz = 'EST5EDT'),
                   timeFormat="%T",   
                   step = 900)
     ),
     mainPanel(
       plotOutput("wordcloud")
     )
    )
   )


##SERVER
# Define server logic required to draw a wordcloud
server <- function(input, output){
   

  time <- reactive({
    cloud[which(input$time == cloud$created),]
  })

  wordcloud_rep <- repeatable(wordcloud)
  
  
  output$wordcloud <- renderPlot({
     times <- time()
     # draw the word cloud
     wordcloud_rep(words = times$word, freq = times$n, min.freq = 1,
                   max.words=300, random.order=FALSE, random.color = FALSE,
                   rot.per=0.35, colors=brewer.pal(8, "Dark2"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

