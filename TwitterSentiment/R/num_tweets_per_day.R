#afinn, bing, nrc
num_tweets_per_day = function(data, lexicon, dropwords)
{
  lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
  #Analysis of any lexicon
  sentiment = 
    data %>%
    inner_join(lexicon)
  
  #Number of tweets per day
  graph =
    sentiment %>%
    group_by(date) %>%
    ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(aes(y = ..count..)) + coord_flip() + labs(title = "Number of Tweets Per Day", y = "Count", x = "Date") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
  return(graph)
}
