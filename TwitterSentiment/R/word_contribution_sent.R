word_contribution_sent = function(data, dropwords, lexicon = c("nrc", "bing"))
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

    #Displays the words that contribute the most to each sentiment
    wordContributionGraph = 
      sentiment_nrc %>%
      # Count by word and sentiment
      count(word, sentiment) %>%
      # Group by sentiment
      group_by(sentiment) %>%
      # Take the top 10 words for each sentiment
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      # Set up the plot with aes()
      ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~ sentiment, scales = "free") + coord_flip() +
      labs(title = "Words that Contributed the Most to Each Sentiment", x = "Count") +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5))

  }else
  {
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)
    
    #Displays the words that contribute the most to each sentiment
    wordContributionGraph = 
      sentiment_bing %>%
      # Count by word and sentiment
      count(word, sentiment) %>%
      # Group by sentiment
      group_by(sentiment) %>%
      # Take the top 10 words for each sentiment
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      # Set up the plot with aes()
      ggplot(aes(word, n, fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~ sentiment, scales = "free") + coord_flip() +
      labs(title = "Words that Contributed the Most to Each Sentiment", x = "Count") +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5))
    
}
return(wordContributionGraph)
  
  #returns to the local
  Sys.setlocale("LC_TIME", lct)
}
