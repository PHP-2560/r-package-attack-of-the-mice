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
                         choices = list("10 most common words",
                                        "Positive word cloud",
                                        "Negative word cloud")
                         )
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
    if(input$checkGroup == "10 most common words"){
      source("most_common_words.R", local = TRUE)
      most_common_words(dataset(), 10)
    }
    
    if(input$checkGroup == "Positive word cloud"){
      source("word_cloud.R", local = TRUE)
      word_cloud(dataset(), sent = "positive")
    }
    
    if(input$checkGroup == "Negative word cloud"){
      source("word_cloud.R", local = TRUE)
      word_cloud(dataset(), sent = "negative")
    }
  })
  
  output$plot = renderPlot({
    graph()
  })
}

shinyApp(ui = ui, server = server)

