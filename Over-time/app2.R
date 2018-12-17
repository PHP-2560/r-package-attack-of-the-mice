ui <- fluidPage(navbarPage("Tabs",
                           tabPanel("Over Time Tab",
                                    titlePanel(h1("Over-Time Analysis of Twitter Data")),
                                    sidebarPanel(
                                      helpText("Explore how the sentiments change over time."),
                                      
                                      selectInput("twitter_acc",
                                                  label = "Choose a twitter account",
                                                  choices = list("Donald Trump",
                                                                 "Ariana Grande",
                                                                 "Chrissy Teigen")),
                                      
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

server <- function(input, output) {
  
  dataset2 = reactive({
    if(input$twitter_acc == "Donald Trump")
    {
      readRDS("tidy_data.rds")
    }
  })
  
  dataset = reactive({
    filter(dataset2(), between(as.Date(date, '%b %d'), input$date[1], input$date[2]))
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
  
  output$plot1 = renderPlot({
    graph1()
  })
  output$plot2 = renderPlot({
    graph2()
  })
  output$plot3 = renderPlot({
    graph3()
  })
}

shinyApp(ui = ui, server = server)