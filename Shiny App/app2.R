library(shiny)

ui <- fluidPage(
  
  navbarPage("Tabs",
             tabPanel("Text analysis",
                
                     #sidebarLayout(
                        sidebarPanel(
                          helpText("Explore which words are used the most by twitter accounts"),
                          
                          selectInput("twitter_acc",
                                      label = "Choose a twitter account",
                                      choices = list("Donald Trump",
                                                     "Chrissy Teigen",
                                                     "Ariana Grande")
                                      ),
                          radioButtons("plottype", 
                                       label = "Choose a graph",
                                       choices = c("10 most common words",
                                        "Positive word cloud",
                                        "Negative word cloud")
                                       )
                          ),
            
                        mainPanel(
                          plotOutput("plot")
                          )
                        #) 
                      ),
             tabPanel("Sentiment analysis")
             )
  )

server <- function(input, output, session) {
  
  dataset = reactive({
    if(input$twitter_acc == "Donald Trump"){
      read.csv("donaldtrump.csv")
    }
  })
  
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
}

shinyApp(ui = ui, server = server)

