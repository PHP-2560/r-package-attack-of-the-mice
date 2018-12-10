# NRC: count of individual senitments by tweet (% or #)(NRC);
count_sentiments = function(data, dropwords, lexicon = c("nrc", "bing"))
{
  #drops words from the lexicon that could lead to misleading results
  #outputs various graphs
  if(lexicon == "nrc")
  {
    nrc_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis Using "nrc" lexicon
    sentiment_nrc = 
      data %>%
      inner_join(nrc_lexicon)
    
    #Count by sentiment
    
    countGraph = 
      sentiment_nrc %>%
      group_by(sentiment) %>%
      count() %>%
      ggplot(aes(x = sentiment,  y = n)) + geom_col(fill = "indianred3") + 
      labs(title = "Count of Individual Sentiments in Tweets", x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
  }else
  {
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)
    
    #Count by sentiment
    countGraph = 
      sentiment_bing %>%
      group_by(sentiment) %>%
      count() %>%
      ggplot(aes(x = sentiment,  y = n)) + geom_col(fill = "indianred3") + 
      labs(title = "Count of Individual Sentiments in Tweets", x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))

  }
  #returns to the local
  return(countGraph)
}
