#For bing, or nrc
count_tweet_by_date = function(data, lexicon, dropwords)
{
  new_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
  #Analysis Using given lexicon
  sentiment = 
    data %>%
    inner_join(new_lexicon)
  
  #Count tweets grouped by date and sentiment
  graph = 
    sentiment %>%
    group_by(date,sentiment) %>%
    count() %>%
    ggplot(aes(x = as.Date(date, format = "%b %d"),  y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
    labs(title = "Count of Tweets By Date", x = "Date", y = "Count") +
    scale_x_date(date_breaks = "7 days") + 
    theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
}