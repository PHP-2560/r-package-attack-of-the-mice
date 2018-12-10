#For bing, or nrc
tweet_percentage_by_date = function(data, lexicon, dropwords)
{
  new_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
  #Analysis Using given lexicon
  sentiment = 
    data %>%
    inner_join(new_lexicon)

  #Percentage of tweets grouped by date and sentiment
  graph = 
    sentiment %>%
    group_by(date, sentiment) %>%
    ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) +
    labs(title = "Percentage of Tweets By Date", x = "Date", y = "Percentage") + facet_wrap(~sentiment) +
    scale_x_date(date_breaks = "7 days") + 
    scale_y_continuous(labels=scales::percent) +
    theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
  return(graph)
}