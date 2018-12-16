#library(tidytext)
#library(ggplot2)
#library(stringr)
#library(stats)
#library(waffle)
#library(scales)

datasets = list(readRDS('tidy_trump.rds'), readRDS('tidy_ariana.rds'), readRDS('tidy_chrissy.rds'))

#3. Create an empty shiny app
ui <- fluidPage(navbarPage("Tabs",
                           
                           ##TAB 1
                           tabPanel("Text analysis",
                                    sidebarPanel(
                                      helpText("Explore which words are used the most by twitter accounts"),
                                      
                                      selectInput("twitter_acc1",
                                                  label = "Choose a twitter account",
                                                  choices = list("Donald Trump" = "1",
                                                                 "Chrissy Teigen" = "2",
                                                                 "Ariana Grande"= "3")
                                      ),
                                      radioButtons("plottype",
                                                   label = "Choose a graph",
                                                   choices = c("10 most common words",
                                                               "Positive word cloud",
                                                               "Negative word cloud")
                                      )
                                    ),
                                    
                                    mainPanel(plotOutput("plot"))
                           ),
                           
                           ##TAB 2
                           tabPanel("Sentiment analysis",
                                    titlePanel(h1("Sentiment Analysis of Twitter Data")),
                                    sidebarPanel(
                                      helpText("Explore which sentiments are used the most by twitter accounts"),
                                      selectInput("twitter_acc2",
                                                  label = "Choose a twitter account",
                                                  choices = list("Donald Trump" = "1",
                                                                 "Chrissy Teigen" = "2",
                                                                 "Ariana Grande"= "3")
                                      ),
                                      radioButtons(inputId = "lexiconChoice", 
                                                   label = "Choose a sentiment lexicon", 
                                                   choices = c("afinn", "bing", "nrc"),
                                                   selected = "afinn")),
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Waffle", plotOutput("plot4", width = "100%", height = 800)),
                                        tabPanel("Scores & Sentiments", plotOutput("plot5", width = "100%", height = 800)),
                                        tabPanel("Word Contribution", plotOutput("plot6", width = "100%", height = 1000)))
                                    )
                           ),
  tabPanel("Over Time Tab",
           titlePanel(h1("Over-Time Analysis of Twitter Data")),
           sidebarPanel(
               helpText("Explore how the sentiments change over time."),
               
               selectInput("twitter_acc3",
                           label = "Choose a twitter account",
                           choices = list("Donald Trump" = "1",
                                          "Chrissy Teigen" = "2",
                                          "Ariana Grande"= "3")
               ),
               
               radioButtons(inputId = "lexiconChoice2", 
                            label = "Choose a sentiment lexicon", 
                            choices = c("afinn", "bing", "nrc"),
                            selected = "afinn"), 
               
               dateRangeInput(inputId = "date", label = "Date Range", start = '2018-12-16', end = '2018-12-15')
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Average & Counts", plotOutput("plot1", width = "100%", height = 500)),
                 tabPanel("Average and Percentages", plotOutput("plot2", width = "100%", height = 800)),
                 tabPanel("Number of Tweets Per Day", plotOutput("plot3", width = "100%", height = 1000)))
             )
           )
))
  
# dataset2 = readRDS("tidy_data.rds")

server <- function(input, output) {
  
  dataset <- reactive ({
    dataset3 <- datasets[[as.numeric(input$twitter_acc3)]]
  })

  dataset2 <- reactive ({
    dataset3 <- datasets[[as.numeric(input$twitter_acc2)]]
     })
  dataset3 <- reactive ({
    dataset3 <- datasets[[as.numeric(input$twitter_acc1)]]
     })
  
  #TAB 1 
  graph = reactive({
    if(input$plottype == "10 most common words"){
      source("most_common_words.R", local = TRUE)
      most_common_words(dataset3(), 10)
    } else if(input$plottype == "Positive word cloud"){
      source("word_cloud.R", local = TRUE)
      word_cloud(dataset3(), sent = "positive")
    } else if(input$plottype == "Negative word cloud"){
      source("word_cloud.R", local = TRUE)
      word_cloud(dataset3(), sent = "negative")
    }
  })
  
  output$plot = renderPlot({
    graph()
  })
  
  
  
  graph1 = reactive({
    if(input$lexiconChoice2 == "afinn"){
      source("afinn_avg_sentiment_per_day.R", local = TRUE)
      afinn_avg_sentiment_per_day(dataset(), dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice2 == "nrc"){
      source("count_tweets_by_date.R", local = TRUE)
      count_tweet_by_date(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice2 == "bing"){
      source("count_tweets_by_date.R", local = TRUE)
      count_tweet_by_date(dataset(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  graph2 = reactive({
    if(input$lexiconChoice2 == "afinn"){
      source("afinn_avg_sentiment_by_tweet.R", local = TRUE)
      afinn_avg_sentiment_per_tweet(dataset(), dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice2 == "nrc"){
      source("tweet_percentage_by_date.R", local = TRUE)
      tweet_percentage_by_date(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice2 == "bing"){
      source("tweet_percentage_by_date.R", local = TRUE)
      tweet_percentage_by_date(dataset(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  graph3 = reactive({
    if(input$lexiconChoice2 == "afinn"){
      source("num_tweets_per_day.R", local = TRUE)
      num_tweets_per_day(dataset(), lexicon = 'afinn', dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice2 == "nrc"){
      source("num_tweets_per_day.R", local = TRUE)
      num_tweets_per_day(dataset(), lexicon = 'nrc', dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice2 == "bing"){
      source("num_tweets_per_day.R", local = TRUE)
      num_tweets_per_day(dataset(), lexicon = 'bing', dropwords = c("trump", "grand"))
    }
  })
  
  graph4 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("waffle_func.R", local = TRUE)
      waffleFun(dataset2(), lexicon = "afinn", dropwords = c("trump", "grand"), num_Rows = 50)
    } else if(input$lexiconChoice == "nrc"){
      source("waffle_func.R", local = TRUE)
      waffleFun(dataset2(), lexicon = "nrc", dropwords = c("trump", "grand"), num_Rows = 70)
    } else if(input$lexiconChoice == "bing"){
      source("waffle_func.R", local = TRUE)
      waffleFun(dataset2(), lexicon = "bing", dropwords = c("trump", "grand"), num_Rows = 50)
    }
  })
  
  graph5 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("histogram_of_scores.R", local = TRUE)
      histogram_of_scores(dataset2(), dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "nrc"){
      source("count_sentiments.R", local = TRUE)
      count_sentiments(dataset2(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "bing"){
      source("count_sentiments.R", local = TRUE)
      count_sentiments(dataset2(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  graph6 = reactive({
    if(input$lexiconChoice == "bing"){
      source("word_contribution_sent.R", local = TRUE)
      word_contribution_sent(dataset2(), dropwords = c("trump", "grand"), lexicon = 'bing')
    } else if(input$lexiconChoice == "nrc"){
      source("word_contribution_sent.R", local = TRUE)
      word_contribution_sent(dataset2(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "afinn"){
      source("score_by_tweet.R", local = TRUE)
      score_by_tweet(dataset2(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  output$plot1 = renderPlot({
    graph1()
  })
  output$plot2 = renderPlot({
    graph2()
  })
  output$plot3 = renderPlot({
    graph3()
  })
  
  output$plot4 = renderPlot({
    graph4()
  })
  output$plot5 = renderPlot({
    graph5()
  })
  output$plot6 = renderPlot({
    graph6()
  })
  
  
}

shinyApp(ui = ui, server = server)
