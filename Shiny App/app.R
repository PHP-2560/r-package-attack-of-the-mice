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
      radioButtons("plottype", 
                   label = "Choose a graph",
                   choices = c("10 most common words",
                               "Positive word cloud",
                               "Negative word cloud")
      )

      # checkboxGroupInput("checkGroup",
      #                    label = "Pick which graph you want to output",
      #                    choices = list("10 most common words",
      #                                   "Positive word cloud",
      #                                   "Negative word cloud"),
      #                    selected = "10 most common words"
      #                    )
    ),
    
    
    
    mainPanel(
     plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
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

