library(shiny)

ui <- fluidPage(
  titlePanel("Twitter Text Analysis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Explore which words are used the most by twitter accounts"),

      selectInput("twitter_acc",
                  label = "Choose a twitter account",
                  choices = list("Donald Trump",
                                 "Account 2",
                                 "Account 3")),

      checkboxGroupInput("checkGroup",
                         label = "Pick which graph you want to output",
                         choices = list("Top 10 words per sentiments",
                                        "Word cloud",
                                        "Number of tweets per day"),
                         selected = "Top 10 words per sentiments")
    ),

    mainPanel(
      plotOutput("most_common_words"),
      plotOutput("word_cloud"),
      plotOutput("tweets_per_day")
    )
  )
)

server <- function(input, output) {
  dataset = reactive({
      if(input$twitter_acc == "Donald Trump"){
        read.csv("donaldtrump.csv")
      }
    })

  dataset2 = reactive({
    if(input$twitter_acc == "Donald Trump"){
      read.csv("donald_trump.csv")
    }
  })

  graph1 = reactive({
    if(input$checkGroup == "Top 10 words per sentiments"){
      source("most_common_words.R", local = TRUE)
      most_common_words(dataset(), 10)
    }
  })
  
  graph2 = reactive({
    if(input$checkGroup == "Word cloud"){
      source("word_cloud.R", local = TRUE)
      make_word_cloud(dataset())
    }
  })
  
  
  output$most_common_words = renderPlot({
    graph1()
  })
  
  output$word_cloud = renderPlot({
    graph2()
  })

  #if(input$checkGroup == 2){
    #output$word_cloud = renderPlot({
     # source("word_cloud.R", local = TRUE)
     # make_word_cloud(dataset())
   # })
 # }

 # if(input$checkGroup == 3){
   # output$tweets_per_day = renderPlot({
     # source("tweets_per_day.R", local = TRUE)
     # tweets_per_day(dataset2())
    #})
  #}
}

shinyApp(ui = ui, server = server)

