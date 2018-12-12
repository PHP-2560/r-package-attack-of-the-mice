


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
    mainPanel(
      plotOutput("plot"))
  )
)

server <- function(input, output) {
  dataset = reactive({
    if(input$twitter_acc == "Donald Trump"){
      read.csv("donaldtrump.csv")
    }
  })
  
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
  
    output$plot = renderPlot({
      graph2()
    }
    )
    
    
    
  
  }

shinyApp(ui = ui, server = server)
