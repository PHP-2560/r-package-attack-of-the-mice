#afinn, bing, nrc
tweet_percent = function(data, lexicon, dropwords)
{
  new_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
  #Analysis Using given lexicon
  sentiment = 
    data %>%
    inner_join(new_lexicon)
  
  #Percent of tweets per day
  graph = 
    sentiment %>%
    group_by(date) %>%
    ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(aes(y = ..count../sum(..count..))) + coord_flip() + labs(title = "Number of Tweets Per Day", y = "Percent", x = "Date") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5)) +
    scale_y_continuous(labels=scales::percent) 
  return(graph)
}