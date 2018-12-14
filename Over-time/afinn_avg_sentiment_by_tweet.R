#Average of each tweet
#Just for Afinn dictionary
afinn_avg_sentiment_per_tweet = function(data, dropwords)
{
  lexicon <- get_sentiments("afinn") %>% filter(!word %in% dropwords)
  #Analysis of "afinn" lexicon
  sentiment = 
    data %>%
    inner_join(lexicon)
  
  graph = 
    sentiment %>%
    group_by(`tweet number`) %>%
    summarise(mean = mean(score)) %>%
    ggplot(aes(x = as.numeric(`tweet number`), y = mean, group = 1)) + geom_line(color = "indianred3", size = 2.5) + 
    labs(title = "Average Sentiment Over time", x = "Tweet Number", y = "Average Sentiment Score") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
  
  return(graph)
}