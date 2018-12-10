# NRC: count of individual senitments by tweet (% or #)(NRC);
percent_sentiments = function(data, dropwords, lexicon = c("nrc", "bing"))
{
  #Sets the local so that the dates format nicely
  lct <- Sys.getlocale("LC_TIME"); 
  Sys.setlocale("LC_TIME", "C")  
  
  #drops words from the lexicon that could lead to misleading results
  #outputs various graphs
  if(lexicon == "nrc")
  {
    nrc_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis Using "nrc" lexicon
    sentiment_nrc = 
      data %>%
      inner_join(nrc_lexicon)
    
    #Percentage of each sentiment
    percentGraph = 
      sentiment_nrc %>%
      group_by(sentiment) %>%
      ggplot(aes(x = sentiment)) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) + xlab("Sentiment") + ylab("Relative Frequencies") + ggtitle("Percentage of Sentiments in Tweets") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
  }else
  {
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)
 
    #Percentage of each sentiment
    percentGraph = 
      sentiment_bing %>%
      group_by(sentiment) %>%
      ggplot(aes(x = sentiment)) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) + xlab("Sentiment") + ylab("Relative Frequencies") + ggtitle("Percentage of Sentiments in Tweets") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
  }
    #returns to the local
    Sys.setlocale("LC_TIME", lct)
    return(percentGraph)
  }