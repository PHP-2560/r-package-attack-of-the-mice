#-------------------------------------------------------------
#Sentiment Analysis with data 
#-------------------------------------------------------------
twitter_sentiment_analysis = function(data, dropwords, lexicon = c("nrc", "bing", "afinn"))
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
    
    #Count by sentiment
    graph1 = 
    sentiment_nrc %>%
      group_by(sentiment) %>%
      count() %>%
      ggplot(aes(x = sentiment,  y = n)) + geom_col(fill = "indianred3") + 
      labs(title = "Count of Individual Sentiments in Tweets", x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Percentage of each sentiment
    graph2 = 
      sentiment_nrc %>%
      group_by(sentiment) %>%
      ggplot(aes(x = sentiment)) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) + xlab("Sentiment") + ylab("Relative Frequencies") + ggtitle("Percentage of Sentiments in Tweets") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Count by sentiment and grouped by date
    graph3 = 
    sentiment_nrc %>%
      group_by(date, sentiment) %>%
      count() %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"),  y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Date", y = "Count") +
      scale_x_date(date_breaks = "3 days") + 
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Percentage by sentiment and grouped by date
    graph4 = 
      sentiment_nrc %>%
      group_by(date, sentiment) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Tweets By Date", x = "Date", y = "Percentage") +
      scale_x_date(date_breaks = "3 days") + 
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Displays the words that contribute the most to each sentiment
    graph5 = 
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
    
    #Count by sentiment and grouped by individual tweet
    graph6 = 
      sentiment_nrc %>%
      group_by(sentiment, `tweet number`) %>%
      count() %>%
      ggplot(aes(x = as.numeric(`tweet number`), y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Tweet Number", y = "Count") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Percentage of sentiment grouped by individual tweet
    graph7 = 
      sentiment_nrc %>%
      group_by(sentiment, `tweet number`) %>%
      ggplot(aes(x = as.numeric(`tweet number`))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Each Tweet", x = "Tweet Number", y = "Percentage") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    
  }else if(lexicon == "bing")
  {
    bing_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "bing" lexicon
    sentiment_bing = 
      data %>%
      inner_join(bing_lexicon)
    
    #Count by sentiment
    graph1 = 
      sentiment_bing %>%
      group_by(sentiment) %>%
      count() %>%
      ggplot(aes(x = sentiment,  y = n)) + geom_col(fill = "indianred3") + 
      labs(title = "Count of Individual Sentiments in Tweets", x = "Sentiments", y = "Count") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Percentage of each sentiment
    graph2 = 
      sentiment_bing %>%
      group_by(sentiment) %>%
      ggplot(aes(x = sentiment)) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) + xlab("Sentiment") + ylab("Relative Frequencies") + ggtitle("Percentage of Sentiments in Tweets") +
      theme(axis.title.x = element_text(face = "bold")) + theme(axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Count by sentiment and grouped by date
    graph3 = 
      sentiment_bing %>%
      group_by(date, sentiment) %>%
      count() %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"),  y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Date", y = "Count") +
      scale_x_date(date_breaks = "3 days") + 
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Percentage by sentiment and grouped by date
    graph4 = 
      sentiment_bing %>%
      group_by(date, sentiment) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Tweets By Date", x = "Date", y = "Percentage") +
      scale_x_date(date_breaks = "3 days") + 
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Displays the words that contribute the most to each sentiment
    graph5 = 
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
    
    #Count by sentiment and grouped by individual tweet
    graph6 = 
      sentiment_bing %>%
      group_by(sentiment, `tweet number`) %>%
      count() %>%
      ggplot(aes(x = as.numeric(`tweet number`), y = n)) + geom_col(fill = "indianred3") + facet_wrap(~sentiment) +
      labs(title = "Count of Individual Sentiments in Tweets By Date", x = "Tweet Number", y = "Count") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Percentage of sentiment grouped by individual tweet
    graph7 = 
      sentiment_bing %>%
      group_by(sentiment, `tweet number`) %>%
      ggplot(aes(x = as.numeric(`tweet number`))) + geom_bar(fill = "indianred3", aes(y = (..count..)/sum(..count..))) + facet_wrap(~sentiment) +
      labs(title = "Percentage of Individual Sentiments in Each Tweet", x = "Tweet Number", y = "Percentage") +
      scale_x_continuous(breaks = seq(1, max(unlist(data[,1])), by = 10)) +
      scale_y_continuous(labels=scales::percent) +
      theme(strip.text = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x=element_text(angle=63, hjust=1, face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
  }else
  {
    afinn_lexicon <- get_sentiments(lexicon) %>% filter(!word %in% dropwords)
    
    #Analysis of "afinn" lexicon
    sentiment_afinn = 
      data %>%
      inner_join(afinn_lexicon)
    
    #Average of each day
    graph1 = 
    sentiment_afinn %>%
      group_by(date) %>%
      summarise(mean = mean(score)) %>%
      ggplot(aes(x = as.Date(date, format = "%b %d"), y = mean, group = 1)) + geom_line(color = "indianred3", size = 2.5) + 
      labs(title = "Average Sentiment Over time", x = "Date", y = "Average Sentiment Score") +
      scale_x_date(date_breaks = "3 days") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))
    
    #Histogram of scores
    graph2 = 
    sentiment_afinn %>% 
      group_by(score) %>%
      ggplot(aes(x = score)) + geom_density(color = "black", fill = "indianred3",  alpha = 0.5 ) + 
      labs(title = "Histogram of Scores", x = "Score", y = "Density") +
      theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), plot.title = element_text(face = "bold", hjust=0.5))

}
 
  #-------------------------------------------------------------
  #Other interesting stats
  #-------------------------------------------------------------
  #Most common word
  graph8 = 
  data %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    mutate(word = reorder(word, n)) %>%
    top_n(10)%>%
    ggplot(aes(word, n)) +
    geom_col(fill = "indianred3") + 
    labs(title = "Top 10 Most Commonly Used Words", x = "Word", y = "Number of Times Used") +
    theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_blank(), plot.title = element_text(face = "bold", hjust=0.5)) +
    coord_flip()
  
  #returns to the local
  Sys.setlocale("LC_TIME", lct)
  
  #saves graphs
  if(lexicon == "bing" | lexicon == "nrc")
  {
    return(list(graph1,graph2,graph3,graph4,graph5,graph6,graph7,graph8))
  } else 
  {
    return(list(graph1, graph2, graph8))
  }
}
