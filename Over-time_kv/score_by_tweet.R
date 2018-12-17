score_by_tweet = function(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
{
  lexicon = match.arg(lexicon)
  #drops words from the lexicon that could lead to misleading results
  #outputs various graphs
  if(lexicon == "nrc")
  {
    nrc_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis Using "nrc" lexicon
    sentiment_nrc = 
      data %>%
      inner_join(nrc_lexicon)
    
    # use nrc data to total sentiment by tweet
    datanrcTotal = 
      sentiment_nrc %>%
      group_by(`tweet number`) %>%
      summarise(totalSum = sum(n = n()))
    
    #Graph nrc Sentiment Totals by Tweet
    sentimentTotals = 
      datanrcTotal %>%
      ggplot(aes(reorder(`tweet number`, as.numeric(`tweet number`)), totalSum, fill = totalSum)) + 
      geom_bar(stat = "identity", width = 0.25) +
      scale_fill_gradient(low = "red", high = "seagreen4") +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "snow1"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      scale_x_discrete("Tweet Number", breaks = c()) +
      scale_y_continuous("Sum of Score") +
      labs(fill = "Sum of Score", title = "Total Score Per Tweet") 
    
  }else if(lexicon == "bing")
  {
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)
    
    # use Bing data to total sentiment by tweet
    dataBingTotal = 
      sentiment_bing %>%
      group_by(`tweet number`) %>%
      summarise(totalSum = sum(n = n()))
    
    #Graph Bing Sentiment Totals by Tweet
    sentimentTotals = 
      dataBingTotal %>%
      ggplot(aes(reorder(`tweet number`, as.numeric(`tweet number`)), totalSum, fill = totalSum)) + 
      geom_bar(stat = "identity", width = 0.25) +
      scale_fill_gradient(low = "red", high = "seagreen4") +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "snow1"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      scale_x_discrete("Tweet Number", breaks = c()) +
      scale_y_continuous("Sum of Score") +
      labs(fill = "Sum of Score", title = "Total Score Per Tweet") 
    
  }else
  {
    afinn_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "afinn" lexicon
    sentiment_afinn = 
      data %>%
      inner_join(afinn_lexicon)
    
    # use afinn data to total sentiment by tweet
    dataAfinnTotal = 
      sentiment_afinn %>%
      group_by(`tweet number`) %>%
      summarise(totalSum = sum(score))
    
    #Graph Afinn Sentiment Totals by Tweet
    sentimentTotals = 
      dataAfinnTotal %>%
      ggplot(aes(reorder(`tweet number`, as.numeric(`tweet number`)), totalSum, fill = totalSum)) + 
      geom_bar(stat = "identity", width = 0.25) +
      scale_fill_gradient(low = "red", high = "seagreen4") +
      theme(rect = element_blank()) +
      theme(plot.background = element_rect(fill = "snow1"), axis.line = element_line(color = "grey"), axis.ticks = element_line(color = "grey")) +
      scale_x_discrete("Tweet Number", breaks = c()) +
      scale_y_continuous("Sum of Score") +
      labs(fill = "Sum of Score", title = "Total Score Per Tweet") 
}
  return(sentimentTotals)
}
