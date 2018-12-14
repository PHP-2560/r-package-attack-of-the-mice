#Just for Afinn dictionary
afinn_avg_sentiment_per_day = function(data, dropwords)
{
  afinn_lexicon <- get_sentiments('afinn') %>% filter(!word %in% dropwords)
  
  #Analysis of "afinn" lexicon
  sentiment_afinn = 
    data %>%
    inner_join(afinn_lexicon)
  
  #Average of each day
  graph = 
    sentiment_afinn %>%
    group_by(date) %>%
    summarise(mean = mean(score)) %>%
    ggplot(aes(x = as.Date(date, format = "%b %d"), y = mean, group = 1)) + geom_line(color = "indianred3", size = 2.5) + 
    labs(title = "Average Sentiment Over time", x = "Date", y = "Average Sentiment Score") +
    scale_x_date(date_breaks = "3 days") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
  
  return(graph)
}