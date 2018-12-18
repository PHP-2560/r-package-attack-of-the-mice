source('check_packages.R')
check_packages(c('tidytext', 'ggplot2', 'stringr', 'stats', 'waffle', 'scales', 'dplyr', 'wordcloud', 'shinythemes', 'vembedr', 'htmltools'))

ui <- fluidPage(theme = shinytheme("cerulean"), navbarPage("Tabs",
                                                           
                           #INTRO TAB
                           tabPanel("Introduction",
                                    sidebarPanel(
                                      helpText("Introduction to this Shiny App")),
                                    
                                    mainPanel(                                      
                                      tabsetPanel(
                                        tabPanel("Background", 
                                                 h1("Background"), 
                                                 h5("The goal of this application is to graphically explain the results of a sentiment and text analysis of tweets. We previously scraped the data from Twitter and
                                                    stored the datasets. If you are interested in seeing how we did this, please see our", a(href="https://github.com/PHP-2560/final-project-twittersentiment/tree/master/TwitterSentiment", "Github"),
                                                    'More specifically, we recommend looking at our dynamic_webscraping function.'),
                                                 h5("Our loaded datasets are tweets from Donald Trump, Chrissy Teigen, and Ariana Grande. If you would like to run the analyses on your own data, we also included an option to upload your own dataset. 
                                                    If you would like to pursue this option, note that the datasets must have columns that are named 'tweet num', 'date', and 'word'.
                                                      The word column is composed of the individual words from each tweet. The date column is the date each word was written. The tweet number is the tweet that corresponded
                                                      to that word."),
                                                 h5("Within the app you can choose a dataset, and then choose by sentiment which analyses you'd like to see. Sentiment dictionaries are explained 
                                                    within the Dictionaries tab of Introduction."),
                                                h5("Enjoy!")),
                                        
                                        tabPanel("Dictionaries", 
                                                 h1("Dictionaries"),
                                                 h5("Our sentiment analyses allow you to pick different graphs based on sentiment lexicon, which come from the tidytext library."),
                                                 h5("Each lexicon joins a sentiment by a word, where a word in the data is each word in each tweet. Below are brief descriptions for each:"),
                                                 h5(strong("afinn:")),
                                                 h5("afinn gives each word a score between -5 and 5. A negative score means the word is more negative, 
                                                    while a positive score means the word is more positive."),
                                                 h5(strong("bing:")),
                                                 h5("bing classifies words as either positive or negative."),
                                                 h5(strong("nrc:")),
                                                 h5("nrc classifies words as positive or negative, and also various other classifications such as anger, trust, and fear. 
                                                    Words can have multiple classifications. For example, the word absent is classified as negative and sadness.")),
                                        
                                        tabPanel("Video", h1("Video Tutorial"), h4('Please open in browswer to view the video. If it does not work, please use this', a(href="https://www.youtube.com/watch?v=AJ-HJOTAY4c", "link")),
                                                 uiOutput("video"))))),
                           
                         ##TAB 1
                         tabPanel("Text analysis",
                                  titlePanel(h1("Text Analysis of Twitter Data")),
                                  sidebarPanel(
                                    helpText("Explore which words are used the most by twitter accounts"),
                                     
                                    selectInput("twitter_acc1",
                                                label = "Choose a twitter account",
                                                choices = list("Donald Trump" = "1",
                                                               "Chrissy Teigen" = "2",
                                                               "Ariana Grande"= "3", 
                                                               "Choose your own dataset" = "4")),
                                    fileInput("file1", "Choose RDS File"),
                                    
                                    radioButtons("plottype",
                                                 label = "Choose a graph",
                                                 choices = c("10 most common words",
                                                             "Positive word cloud",
                                                             "Negative word cloud")),
                                    dateRangeInput(inputId = "date_ID1", label = "Date Range", start = '2018-12-01', end = '2018-12-15')),
                                  
                                    mainPanel(plotOutput("plot"))),
                         
                           ##TAB 2
                           tabPanel("Sentiment analysis",
                                    titlePanel(h1("Sentiment Analysis of Twitter Data")),
                                    sidebarPanel(
                                      helpText("Explore which sentiments are used the most by twitter accounts"),
                                      
                                      selectInput("twitter_acc2",
                                                  label = "Choose a twitter account",
                                                  choices = list("Donald Trump" = "1",
                                                                 "Chrissy Teigen" = "2",
                                                                 "Ariana Grande"= "3", 
                                                                 "Choose your own dataset" = "4")),
                                      
                                      fileInput("file2", "Choose RDS File"),
                                      
                                      
                                      radioButtons(inputId = "lexiconChoice", 
                                                   label = "Choose a sentiment lexicon", 
                                                   choices = c("afinn", "bing", "nrc"),
                                                   selected = "afinn"),
                                      
                                      dateRangeInput(inputId = "date_ID2", label = "Date Range", start = '2018-12-01', end = '2018-12-15')),
                                    
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel("Waffle", plotOutput("plot4", width = "100%", height = 800)),
                                        tabPanel("Scores & Sentiments", plotOutput("plot5", width = "100%", height = 500)),
                                        tabPanel("Word Contribution", plotOutput("plot6", width = "100%", height = 1000))))),
                          ##TAB 3
                          tabPanel("Over Time Tab",
                                   titlePanel(h1("Over-Time Analysis of Twitter Data")),
                                   sidebarPanel(
                                       helpText("Explore how the sentiments change over time."),
                                       
                                       selectInput("twitter_acc3",
                                                   label = "Choose a twitter account",
                                                   choices = list("Donald Trump" = "1",
                                                                  "Chrissy Teigen" = "2",
                                                                  "Ariana Grande"= "3", 
                                                                  "Choose your own dataset" = "4")),
                                       
                                       fileInput("file3", "Choose RDS File"),
                                       
                                       radioButtons(inputId = "lexiconChoice2", 
                                                    label = "Choose a sentiment lexicon", 
                                                    choices = c("afinn", "bing", "nrc"),
                                                    selected = "afinn"), 
                                       
                                       dateRangeInput(inputId = "date_ID3", label = "Date Range", start = '2018-12-01', end = '2018-12-15')),
                                     
                                   mainPanel(
                                       tabsetPanel(
                                         tabPanel("Average & Counts", plotOutput("plot1", width = "100%", height = 500)),
                                         tabPanel("Average and Percentages", plotOutput("plot2", width = "100%", height = 500)),
                                         tabPanel("Number of Tweets Per Day", plotOutput("plot3", width = "100%", height = 1000)))))))
#Loads datasets into list
datasets = list(readRDS('tidy_trump.rds'), readRDS('tidy_chrissy.rds'), readRDS('tidy_ariana.rds'))

server <- function(input, output) {
  
  dataset <- reactive({
    dataset4 <- datasets[[as.numeric(input$twitter_acc3)]]
    filter(dataset4, as.Date(date, '%b %d') >= input$date_ID3[1], as.Date(date, '%b %d') <= input$date_ID3[2])
  })
  dataset2 <- reactive({
    dataset4 <- datasets[[as.numeric(input$twitter_acc2)]]
    filter(dataset4, as.Date(date, '%b %d') >= input$date_ID2[1], as.Date(date, '%b %d') <= input$date_ID2[2])
  })
  dataset3 <- reactive({
    dataset4 <- datasets[[as.numeric(input$twitter_acc1)]]
    filter(dataset4, as.Date(date, '%b %d') >= input$date_ID1[1], as.Date(date, '%b %d') <= input$date_ID1[2])
  })
  
  dataset5 = reactive({
    dataset4 = readRDS(input$file1$datapath)
    filter(dataset4, as.Date(date, '%b %d') >= input$date_ID1[1], as.Date(date, '%b %d') <= input$date_ID1[2])
    
  })
  
  dataset6 = reactive({
    dataset4 = readRDS(input$file2$datapath)
    filter(dataset4, as.Date(date, '%b %d') >= input$date_ID2[1], as.Date(date, '%b %d') <= input$date_ID2[2])
  })
  
  dataset7 = reactive({
    dataset4 = readRDS(input$file3$datapath)
    filter(dataset4, as.Date(date, '%b %d') >= input$date_ID3[1], as.Date(date, '%b %d') <= input$date_ID3[2])
  })
  
  
  #TAB 1 Graphs
  graph = reactive({
    if(input$twitter_acc1 == "4")
    {
      req(input$file1)
      if(input$plottype == "10 most common words"){
        source("most_common_words.R", local = TRUE)
        most_common_words(dataset5(), num_words = 10)
      } else if(input$plottype == "Positive word cloud"){
        source("word_cloud.R", local = TRUE)
        word_cloud(dataset5(), sent = "positive")
      } else if(input$plottype == "Negative word cloud"){
        source("word_cloud.R", local = TRUE)
        word_cloud(dataset5(), sent = "negative")
      }
    }else{
      if(input$plottype == "10 most common words"){
        source("most_common_words.R", local = TRUE)
        most_common_words(dataset3(), num_words = 10)
      } else if(input$plottype == "Positive word cloud"){
        source("word_cloud.R", local = TRUE)
        word_cloud(dataset3(), sent = "positive")
      } else if(input$plottype == "Negative word cloud"){
        source("word_cloud.R", local = TRUE)
        word_cloud(dataset3(), sent = "negative")
      }
    }
  })
  
  # TAB 3 Graphs
  graph1 = reactive({
    if(input$twitter_acc3 == "4")
    {
      req(input$file3)
      if(input$lexiconChoice2 == "afinn"){
        source("afinn_avg_sentiment_per_day.R", local = TRUE)
        afinn_avg_sentiment_per_day(dataset7(), dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice2 == "nrc"){
        source("count_tweets_by_date.R", local = TRUE)
        count_tweet_by_date(dataset7(), lexicon = "nrc", dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice2 == "bing"){
        source("count_tweets_by_date.R", local = TRUE)
        count_tweet_by_date(dataset7(), lexicon = "bing", dropwords = c("trump", "grand"))
      }
    }else{
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
    }
  })
  
  graph2 = reactive({
    if(input$twitter_acc3 == '4')
    {
      req(input$file3)
      if(input$lexiconChoice2 == "afinn"){
        source("afinn_avg_sentiment_by_tweet.R", local = TRUE)
        afinn_avg_sentiment_per_tweet(dataset7(), dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice2 == "nrc"){
        source("tweet_percentage_by_date.R", local = TRUE)
        tweet_percentage_by_date(dataset7(), lexicon = "nrc", dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice2 == "bing"){
        source("tweet_percentage_by_date.R", local = TRUE)
        tweet_percentage_by_date(dataset7(), lexicon = "bing", dropwords = c("trump", "grand"))
      }
    }else{
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
    }
  })
  
  graph3 = reactive({
    if(input$twitter_acc3 == "4")
    {
      req(input$file3)
      if(input$lexiconChoice2 == "afinn"){
        source("num_tweets_per_day.R", local = TRUE)
        num_tweets_per_day(dataset7(), lexicon = 'afinn', dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice2 == "nrc"){
        source("num_tweets_per_day.R", local = TRUE)
        num_tweets_per_day(dataset7(), lexicon = 'nrc', dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice2 == "bing"){
        source("num_tweets_per_day.R", local = TRUE)
        num_tweets_per_day(dataset7(), lexicon = 'bing', dropwords = c("trump", "grand"))
      }
    }else{
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
    }
  })
  
  #TAB 2 Graphs
  graph4 = reactive({
    if(input$twitter_acc2 == "4")
    {
      req(input$file2)
      if(input$lexiconChoice == "afinn"){
        source("waffle_func.R", local = TRUE)
        waffleFun(dataset6(), lexicon = "afinn", dropwords = c("trump", "grand"), num_Rows = 25)
      } else if(input$lexiconChoice == "nrc"){
        source("waffle_func.R", local = TRUE)
        waffleFun(dataset6(), lexicon = "nrc", dropwords = c("trump", "grand"), num_Rows = 50)
      } else if(input$lexiconChoice == "bing"){
        source("waffle_func.R", local = TRUE)
        waffleFun(dataset6(), lexicon = "bing", dropwords = c("trump", "grand"), num_Rows = 25)
      }
    }else
      {
        if(input$lexiconChoice == "afinn"){
          source("waffle_func.R", local = TRUE)
          waffleFun(dataset2(), lexicon = "afinn", dropwords = c("trump", "grand"), num_Rows = 25)
        } else if(input$lexiconChoice == "nrc"){
          source("waffle_func.R", local = TRUE)
          waffleFun(dataset2(), lexicon = "nrc", dropwords = c("trump", "grand"), num_Rows = 50)
        } else if(input$lexiconChoice == "bing"){
          source("waffle_func.R", local = TRUE)
          waffleFun(dataset2(), lexicon = "bing", dropwords = c("trump", "grand"), num_Rows = 25)
        }
      }
  })
  
  graph5 = reactive({
    if(input$twitter_acc2 == "4")
    {
      req(input$file2)
      if(input$lexiconChoice == "afinn"){
        source("histogram_of_scores.R", local = TRUE)
        histogram_of_scores(dataset6(), dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice == "nrc"){
        source("count_sentiments.R", local = TRUE)
        count_sentiments(dataset6(), lexicon = "nrc", dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice == "bing"){
        source("count_sentiments.R", local = TRUE)
        count_sentiments(dataset6(), lexicon = "bing", dropwords = c("trump", "grand"))
      }
    }else
    {
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
    }
  })
  
  graph6 = reactive({
    if(input$twitter_acc2 == "4")
    {
      req(input$file2)
      if(input$lexiconChoice == "bing"){
        source("word_contribution_sent.R", local = TRUE)
        word_contribution_sent(dataset6(), dropwords = c("trump", "grand"), lexicon = 'bing')
      } else if(input$lexiconChoice == "nrc"){
        source("word_contribution_sent.R", local = TRUE)
        word_contribution_sent(dataset6(), lexicon = "nrc", dropwords = c("trump", "grand"))
      } else if(input$lexiconChoice == "afinn"){
        source("score_by_tweet.R", local = TRUE)
        score_by_tweet(dataset6(), lexicon = "bing", dropwords = c("trump", "grand"))
      }
    }else
    {
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
    }
  })
  
  #Outputting the grpahs
  output$plot = renderPlot({graph()})
  output$plot1 = renderPlot({graph1()})
  output$plot2 = renderPlot({graph2()})
  output$plot3 = renderPlot({graph3()})
  output$plot4 = renderPlot({graph4()})
  output$plot5 = renderPlot({graph5()})
  output$plot6 = renderPlot({graph6()})
  output$video = renderUI({embed_youtube("AJ-HJOTAY4c")})
}

#Running app
shinyApp(ui = ui, server = server)
