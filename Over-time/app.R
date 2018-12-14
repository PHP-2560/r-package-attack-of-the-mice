#library(tidytext)
#library(ggplot2)
#library(stringr)
#library(stats)

#3. Create an empty shiny app
ui <- fluidPage(
  titlePanel("Sentiment Analysis of Twitter Data"),
  sidebarLayout(
    sidebarPanel(
      helpText("Explore which sentiments are used the most by twitter accounts"),
      
      selectInput("twitter_acc",
                  label = "Choose a twitter account",
                  choices = list("Donald Trump",
                                 "Account 2",
                                 "Account 3")),
      
      radioButtons(inputId = "lexiconChoice", 
                   label = "Choose a sentiment lexicon", 
                   choices = c("afinn", "bing", "nrc"),
                   selected = "afinn")
    ),
    mainPanel(position = "right",
              plotOutput("plot1"),
              br(),
              br(),
              plotOutput("plot2"),
              plotOutput("plot3"))
  )
)

server <- function(input, output) {
  dataset = reactive({
    if(input$twitter_acc == "Donald Trump"){
      readRDS("my_data.rds")
    }
  })
  
  graph1 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("afinn_avg_sentiment_per_day.R", local = TRUE)
      afinn_avg_sentiment_per_day(dataset(), dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "nrc"){
      source("count_tweets_by_date.R", local = TRUE)
      count_tweet_by_date(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "bing"){
      source("count_tweets_by_date.R", local = TRUE)
      count_tweet_by_date(dataset(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  graph2 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("afinn_avg_sentiment_by_tweet.R", local = TRUE)
      afinn_avg_sentiment_per_tweet(dataset(), dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "nrc"){
      source("tweet_percentage_by_date.R", local = TRUE)
      tweet_percentage_by_date(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "bing"){
      source("tweet_percentage_by_date.R", local = TRUE)
      tweet_percentage_by_date(dataset(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  graph3 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("num_tweets_per_day.R", local = TRUE)
      num_tweets_per_day(dataset(), lexicon = 'afinn', dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "nrc"){
      source("num_tweets_per_day.R", local = TRUE)
      num_tweets_per_day(dataset(), lexicon = 'nrc', dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "bing"){
      source("num_tweets_per_day.R", local = TRUE)
      num_tweets_per_day(dataset(), lexicon = 'bing', dropwords = c("trump", "grand"))
    }
  })
  
  output$plot1 = renderPlot({
    graph1()
  }
  )
  output$plot2 = renderPlot({
    graph2()
  })
  output$plot3 = renderPlot({
    graph3()
  })
  
  
}

shinyApp(ui = ui, server = server)
