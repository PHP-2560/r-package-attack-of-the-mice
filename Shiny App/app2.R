library(shiny)

ui <- fluidPage(navbarPage("Tabs",
  
  ##TAB 1
  tabPanel("Text analysis",
           sidebarPanel(
             helpText("Explore which words are used the most by twitter accounts"),
             
             selectInput("twitter_acc",
                         label = "Choose a twitter account",
                         choices = list("Donald Trump",
                                        "Chrissy Teigen",
                                        "Ariana Grande",
                                        "Choose your own dataset")),
               
            fileInput("file1", "Upload your own file",
                        multiple = TRUE,
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
             
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
             selectInput("twitter_acc",
                         label = "Choose a twitter account",
                         choices = list("Donald Trump",
                                        "Ariana Grande",
                                        "Chrissy Teigen")),
                          
             radioButtons(inputId = "lexiconChoice", 
                          label = "Choose a sentiment lexicon", 
                          choices = c("afinn", "bing", "nrc"),
                          selected = "afinn")),
           mainPanel(
             tabsetPanel(
               tabPanel("Waffle", plotOutput("plot1", width = "100%", height = 500)),
               tabPanel("Scores & Sentiments", plotOutput("plot2", width = "100%", height = 800)),
               tabPanel("Word Contribution", plotOutput("plot3", width = "100%", height = 1000)))
             )
           )
))

server <- function(input, output) {
  
  dataset = reactive({
    if(input$twitter_acc == "Donald Trump"){
      read.csv("donaldtrump.csv")
    } else if(input$twitter_acc == "Choose your own dataset"){
      inFile = input$file1
      read.csv(inFile$datapath, header = TRUE)
    }
  })

  #TAB 1 
  graph = reactive({
    if(input$plottype == "10 most common words"){
      source("most_common_words.R", local = TRUE)
      most_common_words(dataset(), 10)
    } else if(input$plottype == "Positive word cloud"){
      source("word_cloud.R", local = TRUE)
      word_cloud(dataset(), sent = "positive")
    } else if(input$plottype == "Negative word cloud"){
      source("word_cloud.R", local = TRUE)
      word_cloud(dataset(), sent = "negative")
    }
  })
  
  output$plot = renderPlot({
    graph()
  })
  
  #TAB 2
  graph1 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("waffle_func.R", local = TRUE)
      waffleFun(dataset(), lexicon = "afinn", dropwords = c("trump", "grand"), num_Rows = 50)
    } else if(input$lexiconChoice == "nrc"){
      source("waffle_func.R", local = TRUE)
      waffleFun(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"), num_Rows = 70)
    } else if(input$lexiconChoice == "bing"){
      source("waffle_func.R", local = TRUE)
      waffleFun(dataset(), lexicon = "bing", dropwords = c("trump", "grand"), num_Rows = 50)
    }
  })
  
  graph2 = reactive({
    if(input$lexiconChoice == "afinn"){
      source("histogram_of_scores.R", local = TRUE)
      histogram_of_scores(dataset(), dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "nrc"){
      source("count_sentiments.R", local = TRUE)
      count_sentiments(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "bing"){
      source("count_sentiments.R", local = TRUE)
      count_sentiments(dataset(), lexicon = "bing", dropwords = c("trump", "grand"))
    }
  })
  
  graph3 = reactive({
    if(input$lexiconChoice == "bing"){
      source("word_contribution_sent.R", local = TRUE)
      word_contribution_sent(dataset(), dropwords = c("trump", "grand"), lexicon = 'bing')
    } else if(input$lexiconChoice == "nrc"){
      source("word_contribution_sent.R", local = TRUE)
      word_contribution_sent(dataset(), lexicon = "nrc", dropwords = c("trump", "grand"))
    } else if(input$lexiconChoice == "afinn"){
      source("score_by_tweet.R", local = TRUE)
      score_by_tweet(dataset(), lexicon = "bing", dropwords = c("trump", "grand"))
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

